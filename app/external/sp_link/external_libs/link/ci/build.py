#!/usr/bin/env python

import argparse
import logging
import os
import sys

from distutils.spawn import find_executable
from subprocess import call


def parse_args():
    arg_parser = argparse.ArgumentParser()

    arg_parser.add_argument(
        '--cmake',
        default=find_executable("cmake"),
        help='Path to CMake executable (default: %(default)s)')

    arg_parser.add_argument(
        '-c', '--configuration',
        help='Build configuration to use (not supported by IDE generators)')

    arg_parser.add_argument(
        '-a', '--arguments',
        help='Arguments to pass to builder')

    return arg_parser.parse_args(sys.argv[1:])


def build_cmake_args(args, build_dir):
    if args.cmake is None:
        logging.error('CMake not found, please use the --cmake option')
        return None

    cmake_args = []
    cmake_args.append(args.cmake)
    cmake_args.append('--build')
    cmake_args.append(build_dir)

    if args.configuration is not None:
        cmake_args.append('--config')
        cmake_args.append(args.configuration)

    if args.arguments is not None:
        cmake_args.append('--')
        for arg in args.arguments.split():
            cmake_args.append(arg)

    return cmake_args


def build(args):
    scripts_dir = os.path.dirname(os.path.realpath(__file__))
    root_dir = os.path.join(scripts_dir, os.pardir)
    build_dir = os.path.join(root_dir, 'build')
    if not os.path.exists(build_dir):
        logging.error(
            'Build directory not found, did you forget to run the configure.py script?')
        return 2

    cmake_args = build_cmake_args(args, build_dir)
    if cmake_args is None:
        return 1

    logging.info('Running CMake')
    return call(cmake_args)


if __name__ == '__main__':
    logging.basicConfig(format='%(message)s', level=logging.INFO, stream=sys.stdout)
    sys.exit(build(parse_args()))
