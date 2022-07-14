#! /usr/bin/env python

import aubio.cut
from numpy.testing import TestCase

class aubio_cut(TestCase):

    def setUp(self):
        self.a_parser = aubio.cut.aubio_cut_parser()

    def test_default_creation(self):
        assert self.a_parser.parse_args(['-v']).verbose

if __name__ == '__main__':
    from unittest import main
    main()
