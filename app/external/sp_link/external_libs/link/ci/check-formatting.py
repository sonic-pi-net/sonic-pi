#!/usr/bin/env python

import argparse
import logging
import os
import subprocess
import sys


def parse_args():
    arg_parser = argparse.ArgumentParser()

    arg_parser.add_argument(
        '-c', '--clang-format',
        default='clang-format-6.0',
        help='Path to clang-format executable')

    arg_parser.add_argument(
        '-f', '--fix', action='store_true',
        help='Automatically fix all files with formatting errors')

    return arg_parser.parse_args(sys.argv[1:])


def parse_clang_xml(xml):
    for line in xml.splitlines():
        if line.startswith('<replacement '):
            return False
    return True


def fix_file(args, file_absolute_path):
    logging.info('Fixing formatting errors in file: {}'.format(file_absolute_path))
    clang_format_args = [args.clang_format, '-style=file', '-i', file_absolute_path]
    try:
        subprocess.check_call(clang_format_args)
    except subprocess.CalledProcessError:
        logging.error('Error running clang-format on {},'
                      ' please run clang-format -i by hand'.format(file_absolute_path))


def check_files_in_path(args, path):
    logging.info('Checking files in {}'.format(path))
    errors_found = False

    for (path, dirs, files) in os.walk(path):
        for file in files:
            if file.endswith('cpp') or file.endswith('hpp') or file.endswith('.ipp'):
                file_absolute_path = path + os.path.sep + file
                clang_format_args = [
                    args.clang_format, '-style=file',
                    '-output-replacements-xml', file_absolute_path]

                try:
                    clang_format_output = subprocess.check_output(clang_format_args)
                except subprocess.CalledProcessError:
                    logging.error(
                        'Could not execute {}, try running this script with the'
                        '--clang-format option'.format(args.clang_format))
                    sys.exit(2)

                if not parse_clang_xml(clang_format_output):
                    if args.fix:
                        fix_file(args, file_absolute_path)
                    else:
                        logging.warning(
                            '{} has formatting errors'.format(file_absolute_path))
                        errors_found = True

    return errors_found


def check_formatting(args):
    errors_found = False
    script_dir = os.path.dirname(os.path.realpath(__file__))
    for path in ['examples', 'include', 'src']:
        subdir_abs_path = os.path.join(script_dir, path)
        if check_files_in_path(args, subdir_abs_path):
            errors_found = True

    if errors_found:
        logging.warning(
            'Formatting errors found, please fix with clang-format -style=file -i')
    else:
        logging.debug('No formatting errors found!')

    return errors_found


if __name__ == '__main__':
    logging.basicConfig(format='%(message)s', level=logging.INFO, stream=sys.stdout)
    sys.exit(check_formatting(parse_args()))
