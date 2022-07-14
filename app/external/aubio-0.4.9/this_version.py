#! python
import os
import sys

__version_info = {} # keep a reference to parse VERSION once

def get_version_info():
    # read from VERSION
    # return dictionary filled with content of version
    if not __version_info:
        this_file_dir = os.path.dirname(os.path.abspath(__file__))
        version_file = os.path.join(this_file_dir, 'VERSION')

        if not os.path.isfile(version_file):
            raise SystemError("VERSION file not found.")

        for l in open(version_file).readlines():
            if l.startswith('AUBIO_MAJOR_VERSION'):
                __version_info['AUBIO_MAJOR_VERSION'] = int(l.split('=')[1])
            if l.startswith('AUBIO_MINOR_VERSION'):
                __version_info['AUBIO_MINOR_VERSION'] = int(l.split('=')[1])
            if l.startswith('AUBIO_PATCH_VERSION'):
                __version_info['AUBIO_PATCH_VERSION'] = int(l.split('=')[1])
            if l.startswith('AUBIO_VERSION_STATUS'):
                __version_info['AUBIO_VERSION_STATUS'] = \
                    l.split('=')[1].strip()[1:-1]

            if l.startswith('LIBAUBIO_LT_CUR'):
                __version_info['LIBAUBIO_LT_CUR'] = int(l.split('=')[1])
            if l.startswith('LIBAUBIO_LT_REV'):
                __version_info['LIBAUBIO_LT_REV'] = int(l.split('=')[1])
            if l.startswith('LIBAUBIO_LT_AGE'):
                __version_info['LIBAUBIO_LT_AGE'] = int(l.split('=')[1])

        if len(__version_info) < 6:
            raise SystemError("Failed parsing VERSION file.")

        # switch version status with commit sha in alpha releases
        if __version_info['AUBIO_VERSION_STATUS'] and \
                '~alpha' in __version_info['AUBIO_VERSION_STATUS']:
            AUBIO_GIT_SHA = get_git_revision_hash()
            if AUBIO_GIT_SHA:
                __version_info['AUBIO_VERSION_STATUS'] = '~git+' + AUBIO_GIT_SHA

    return __version_info

def get_libaubio_version():
    verfmt = '%(LIBAUBIO_LT_CUR)s.%(LIBAUBIO_LT_REV)s.%(LIBAUBIO_LT_AGE)s'
    return str(verfmt % get_version_info())

def get_aubio_version():
    verfmt = '%(AUBIO_MAJOR_VERSION)s.%(AUBIO_MINOR_VERSION)s.%(AUBIO_PATCH_VERSION)s%(AUBIO_VERSION_STATUS)s'
    return str(verfmt % get_version_info())

def get_aubio_pyversion():
    # convert to version for python according to pep 440
    # see https://www.python.org/dev/peps/pep-0440/
    # outputs MAJ.MIN.PATCH[a0[+git.<sha>[.mods]]]
    aubio_version = get_aubio_version()
    if '~git+' in aubio_version:
        pep440str = aubio_version.replace('+', '.')
        verstr = pep440str.replace('~git.', 'a0+')
    elif '~alpha' in aubio_version:
        verstr = aubio_version.replace('~alpha', 'a0')
    else:
        verstr = aubio_version
    return verstr

def get_git_revision_hash(short=True):
    # get commit id, with +mods if local tree is not clean
    if not os.path.isdir('.git'):
        # print('Version : not in git repository : can\'t get sha')
        return None
    import subprocess
    aubio_dir = os.path.dirname(os.path.abspath(__file__))
    if not os.path.exists(aubio_dir):
        raise SystemError("git / root folder not found")
    gitcmd = ['git', '-C', aubio_dir, 'rev-parse']
    if short:
        gitcmd.append('--short')
    gitcmd.append('HEAD')
    try:
        gitsha = subprocess.check_output(gitcmd).strip().decode('utf8')
    except Exception as e:
        sys.stderr.write('git command error :%s\n' % e)
        return None
    # check if we have a clean tree
    gitcmd = ['git', '-C', aubio_dir, 'status', '--porcelain']
    try:
        output = subprocess.check_output(gitcmd).decode('utf8')
        if len(output):
            sys.stderr.write('Info: current tree is not clean\n\n')
            sys.stderr.write(output + '\n')
            gitsha += '+mods'
    except subprocess.CalledProcessError as e:
        sys.stderr.write('git command error :%s\n' % e)
        pass
    return gitsha

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == '-v':
        print (get_aubio_version())
    elif len(sys.argv) > 1 and sys.argv[1] == '-p':
        print (get_aubio_version())
    else:
        print ('%30s'% 'aubio version:', get_aubio_version())
        print ('%30s'% 'python-aubio version:', get_aubio_pyversion())
