#! /usr/bin/env python

from numpy.testing import TestCase
import aubio.cmd

class aubio_cmd(TestCase):

    def setUp(self):
        self.a_parser = aubio.cmd.aubio_parser()

    def test_default_creation(self):
        try:
            assert self.a_parser.parse_args(['-V']).show_version
        except SystemExit:
            url = 'https://bugs.python.org/issue9253'
            self.skipTest('subcommand became optional in py3, see %s' % url)

class aubio_cmd_utils(TestCase):

    def test_samples2seconds(self):
        self.assertEqual(aubio.cmd.samples2seconds(3200, 32000),
                "0.100000\t")

    def test_samples2milliseconds(self):
        self.assertEqual(aubio.cmd.samples2milliseconds(3200, 32000),
                "100.000000\t")

    def test_samples2samples(self):
        self.assertEqual(aubio.cmd.samples2samples(3200, 32000),
                "3200\t")

if __name__ == '__main__':
    from unittest import main
    main()
