#! /usr/bin/env python

from numpy.testing import TestCase, assert_equal
from aubio import slice_source_at_stamps
from utils import count_files_in_directory, get_default_test_sound
from utils import count_samples_in_directory, count_samples_in_file

import tempfile
import shutil

n_slices = 4

class aubio_slicing_test_case(TestCase):

    def setUp(self):
        self.source_file = get_default_test_sound(self)
        self.output_dir = tempfile.mkdtemp(suffix = 'aubio_slicing_test_case')

    def test_slice_start_only(self):
        regions_start = [i*1000 for i in range(n_slices)]
        slice_source_at_stamps(self.source_file, regions_start, output_dir = self.output_dir)

    def test_slice_start_only_no_zero(self):
        regions_start = [i*1000 for i in range(1, n_slices)]
        slice_source_at_stamps(self.source_file, regions_start,
                output_dir = self.output_dir, create_first=True)

    def test_slice_start_beyond_end(self):
        regions_start = [i*1000 for i in range(1, n_slices)]
        regions_start += [count_samples_in_file(self.source_file) + 1000]
        slice_source_at_stamps(self.source_file, regions_start,
                output_dir = self.output_dir, create_first=True)

    def test_slice_start_every_blocksize(self):
        hopsize = 200
        regions_start = [i*hopsize for i in range(0, n_slices)]
        slice_source_at_stamps(self.source_file, regions_start, output_dir = self.output_dir,
                hopsize = 200)

    def test_slice_start_every_half_blocksize(self):
        hopsize = 200
        regions_start = [i*hopsize//2 for i in range(0, n_slices)]
        slice_source_at_stamps(self.source_file, regions_start,
                output_dir = self.output_dir, hopsize = 200)

    def tearDown(self):
        original_samples = count_samples_in_file(self.source_file)
        written_samples = count_samples_in_directory(self.output_dir)
        total_files = count_files_in_directory(self.output_dir)
        assert_equal(n_slices, total_files,
            "number of slices created different from expected")
        assert_equal(written_samples, original_samples,
            "number of samples written different from number of original samples")
        shutil.rmtree(self.output_dir)

class aubio_slicing_with_ends_test_case(TestCase):

    def setUp(self):
        self.source_file = get_default_test_sound(self)
        self.output_dir = tempfile.mkdtemp(suffix = 'aubio_slicing_test_case')

    def test_slice_start_and_ends_no_gap(self):
        regions_start = [i*1000 for i in range(n_slices)]
        regions_ends = [start - 1 for start in regions_start[1:]] + [1e120]
        slice_source_at_stamps(self.source_file, regions_start, regions_ends,
                output_dir = self.output_dir)
        original_samples = count_samples_in_file(self.source_file)
        written_samples = count_samples_in_directory(self.output_dir)
        total_files = count_files_in_directory(self.output_dir)
        assert_equal(n_slices, total_files,
            "number of slices created different from expected")
        assert_equal(written_samples, original_samples,
            "number of samples written different from number of original samples")

    def test_slice_start_and_ends_200_gap(self):
        regions_start = [i*1000 for i in range(n_slices)]
        regions_ends = [start + 199 for start in regions_start]
        slice_source_at_stamps(self.source_file, regions_start, regions_ends,
                output_dir = self.output_dir)
        expected_samples = 200 * n_slices
        written_samples = count_samples_in_directory(self.output_dir)
        total_files = count_files_in_directory(self.output_dir)
        assert_equal(n_slices, total_files,
            "number of slices created different from expected")
        assert_equal(written_samples, expected_samples,
            "number of samples written different from number of original samples")

    def test_slice_start_and_ends_overlaping(self):
        regions_start = [i*1000 for i in range(n_slices)]
        regions_ends = [start + 1199 for start in regions_start]
        slice_source_at_stamps(self.source_file, regions_start, regions_ends,
                output_dir = self.output_dir)
        expected_samples = 1200 * n_slices
        written_samples = count_samples_in_directory(self.output_dir)
        total_files = count_files_in_directory(self.output_dir)
        assert_equal(n_slices, total_files,
            "number of slices created different from expected")
        assert_equal(written_samples, expected_samples,
            "number of samples written different from number of original samples")

    def test_slice_start_and_ends_with_missing_end(self):
        regions_start = [i*1000 for i in range(n_slices)]
        regions_ends = [r-1 for r in regions_start[1:]]
        slice_source_at_stamps(self.source_file, regions_start, regions_ends,
                output_dir = self.output_dir)
        written_samples = count_samples_in_directory(self.output_dir)
        original_samples = count_samples_in_file(self.source_file)
        total_files = count_files_in_directory(self.output_dir)
        assert_equal(n_slices, total_files,
            "number of slices created different from expected")
        assert_equal(written_samples, original_samples,
            "number of samples written different from number of original samples")

    def tearDown(self):
        shutil.rmtree(self.output_dir)


class aubio_slicing_wrong_starts_test_case(TestCase):

    def setUp(self):
        self.source_file = get_default_test_sound(self)
        self.output_dir = tempfile.mkdtemp(suffix = 'aubio_slicing_test_case')

    def test_slice_start_empty(self):
        regions_start = []
        self.assertRaises(ValueError,
                slice_source_at_stamps,
                self.source_file, regions_start, output_dir = self.output_dir)

    def test_slice_start_none(self):
        regions_start = None
        self.assertRaises(ValueError,
                slice_source_at_stamps,
                self.source_file, regions_start, output_dir = self.output_dir)

    def tearDown(self):
        shutil.rmtree(self.output_dir)

class aubio_slicing_wrong_ends_test_case(TestCase):

    def setUp(self):
        self.source_file = get_default_test_sound(self)
        self.output_dir = tempfile.mkdtemp(suffix = 'aubio_slicing_test_case')

    def test_slice_wrong_ends(self):
        regions_start = [i*1000 for i in range(1, n_slices)]
        regions_end = []
        self.assertRaises (ValueError,
            slice_source_at_stamps, self.source_file, regions_start, regions_end,
                output_dir = self.output_dir)

    def test_slice_no_ends(self):
        regions_start = [i*1000 for i in range(1, n_slices)]
        regions_end = None
        slice_source_at_stamps (self.source_file, regions_start, regions_end,
                output_dir = self.output_dir, create_first=True)
        total_files = count_files_in_directory(self.output_dir)
        assert_equal(n_slices, total_files,
            "number of slices created different from expected")
        original_samples = count_samples_in_file(self.source_file)
        written_samples = count_samples_in_directory(self.output_dir)
        assert_equal(written_samples, original_samples,
            "number of samples written different from number of original samples")

    def tearDown(self):
        shutil.rmtree(self.output_dir)

if __name__ == '__main__':
    from unittest import main
    main()
