#!/usr/bin/env python

from collections import namedtuple

import argparse
import base64
import copy
import json
import subprocess
import sys
import urllib.parse
import urllib.request
import urllib.error

class Error(Exception):
    pass

class Version(object):
    def __init__(self, version):
        versions = version.split(sep='.')
        if len(versions) < 2 or len(versions) > 3:
            raise Error("Invalid version string '{}'".format(version))
        self.major = int(versions[0])
        self.minor = int(versions[1])
        self.revision = int(versions[2]) if len(versions) == 3 else 0

    def __str__(self):
        return '{}.{}.{}'.format(self.major, self.minor, self.revision)

    def __eq__(self, other):
        return self.major == other.major and self.minor == other.minor and self.revision == other.revision

def verify_version(version):
    expected = {
        'VERSION':      [ '"{}"'.format(version), None ],
        'VER_MAJOR':    [ str(version.major), None ],
        'VER_MINOR':    [ str(version.minor), None ],
        'VER_REVISION': [ str(version.revision), None ],
        'VER_PATCH':    [ '0', None ],
        'SOVERSION':    [ '"{}.{}"'.format(version.major, version.minor), None ],
    }

    # Parse CMakeLists
    with open('CMakeLists.txt') as f:
        for line in f.readlines():
            if line.startswith('project(libgit2 VERSION "{}"'.format(version)):
                break
        else:
            raise Error("cmake: invalid project definition")

    # Parse version.h
    with open('include/git2/version.h') as f:
        lines = f.readlines()

    for key in expected.keys():
        define = '#define LIBGIT2_{} '.format(key)
        for line in lines:
            if line.startswith(define):
                expected[key][1] = line[len(define):].strip()
                break
        else:
            raise Error("version.h: missing define for '{}'".format(key))

    for k, v in expected.items():
        if v[0] != v[1]:
            raise Error("version.h: define '{}' does not match (got '{}', expected '{}')".format(k, v[0], v[1]))

    with open('package.json') as f:
        pkg = json.load(f)

    try:
        pkg_version = Version(pkg["version"])
    except KeyError as err:
        raise Error("package.json: missing the field {}".format(err))

    if pkg_version != version:
        raise Error("package.json: version does not match (got '{}', expected '{}')".format(pkg_version, version))

def generate_relnotes(tree, version):
    with open('docs/changelog.md') as f:
        lines = f.readlines()

    if not lines[0].startswith('v'):
        raise Error("changelog.md: missing section for v{}".format(version))
    try:
        v = Version(lines[0][1:].strip())
    except:
        raise Error("changelog.md: invalid version string {}".format(lines[0].strip()))
    if v != version:
        raise Error("changelog.md: changelog version doesn't match (got {}, expected {})".format(v, version))
    if not lines[1].startswith('----'):
        raise Error("changelog.md: missing version header")
    if lines[2] != '\n':
        raise Error("changelog.md: missing newline after version header")

    for i, line in enumerate(lines[3:]):
        if not line.startswith('v'):
            continue
        try:
            Version(line[1:].strip())
            break
        except:
            continue
    else:
        raise Error("changelog.md: cannot find section header of preceding release")

    return ''.join(lines[3:i + 3]).strip()

def git(*args):
    process = subprocess.run([ 'git', *args ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if process.returncode != 0:
        raise Error('Failed executing git {}: {}'.format(' '.join(args),  process.stderr.decode()))
    return process.stdout

def post(url, data, contenttype, user, password):
    request = urllib.request.Request(url, data=data)
    request.add_header('Accept', 'application/json')
    request.add_header('Content-Type', contenttype)
    request.add_header('Content-Length', len(data))
    request.add_header('Authorization', 'Basic ' + base64.b64encode('{}:{}'.format(user, password).encode()).decode())

    try:
        response = urllib.request.urlopen(request)
        if response.getcode() != 201:
            raise Error("POST to '{}' failed: {}".format(url, response.reason))
    except urllib.error.URLError as e:
        raise Error("POST to '{}' failed: {}".format(url, e))
    data = json.load(response)

    return data

def generate_asset(version, tree, archive_format):
    Asset = namedtuple('Asset', ['name', 'label', 'mimetype', 'data'])
    mimetype = 'application/{}'.format('gzip' if archive_format == 'tar.gz' else 'zip')
    return Asset(
        "libgit2-{}.{}".format(version, archive_format), "Release sources: libgit2-{}.{}".format(version, archive_format), mimetype,
        git('archive', '--format', archive_format, '--prefix', 'libgit2-{}/'.format(version), tree)
    )

def release(args):
    params = {
        "tag_name": 'v' + str(args.version),
        "name": 'libgit2 v' + str(args.version),
        "target_commitish": git('rev-parse', args.tree).decode().strip(),
        "body": generate_relnotes(args.tree, args.version),
    }
    assets = [
        generate_asset(args.version, args.tree, 'tar.gz'),
        generate_asset(args.version, args.tree, 'zip'),
    ]

    if args.dryrun:
        for k, v in params.items():
            print('{}: {}'.format(k, v))
        for asset in assets:
            print('asset: name={}, label={}, mimetype={}, bytes={}'.format(asset.name, asset.label, asset.mimetype, len(asset.data)))
        return

    try:
        url = 'https://api.github.com/repos/{}/releases'.format(args.repository)
        response = post(url, json.dumps(params).encode(), 'application/json', args.user, args.password)
    except Error as e:
        raise Error('Could not create release: ' + str(e))

    for asset in assets:
        try:
            url = list(urllib.parse.urlparse(response['upload_url'].split('{?')[0]))
            url[4] = urllib.parse.urlencode({ 'name': asset.name, 'label': asset.label })
            post(urllib.parse.urlunparse(url), asset.data, asset.mimetype, args.user, args.password)
        except Error as e:
            raise Error('Could not upload asset: ' + str(e))

def main():
    parser = argparse.ArgumentParser(description='Create a libgit2 release')
    parser.add_argument('--tree', default='HEAD', help='tree to create release for (default: HEAD)')
    parser.add_argument('--dryrun', action='store_true', help='generate release, but do not post it')
    parser.add_argument('--repository', default='libgit2/libgit2', help='GitHub repository to create repository in')
    parser.add_argument('--user', help='user to authenticate as')
    parser.add_argument('--password', help='password to authenticate with')
    parser.add_argument('version', type=Version, help='version of the new release')
    args = parser.parse_args()

    verify_version(args.version)
    release(args)

if __name__ == '__main__':
    try:
        main()
    except Error as e:
        print(e)
        sys.exit(1)
