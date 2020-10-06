/*
SC_fftlib.h
An interface to abstract over different FFT libraries, for SuperCollider 3.
Copyright (c) 2008 Dan Stowell. All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

*/

#pragma once

#include <string.h>

// These specify the min & max FFT sizes expected (used when creating windows, also allocating some other arrays).
#define SC_FFT_MINSIZE 8
#define SC_FFT_LOG2_MINSIZE 3
#define SC_FFT_MAXSIZE 32768
#define SC_FFT_LOG2_MAXSIZE 15


// Note that things like *fftWindow actually allow for other sizes, to be created on user request.
#define SC_FFT_ABSOLUTE_MAXSIZE 262144
#define SC_FFT_LOG2_ABSOLUTE_MAXSIZE 18
#define SC_FFT_LOG2_ABSOLUTE_MAXSIZE_PLUS1 19

struct scfft;

class SCFFT_Allocator {
public:
    virtual void* alloc(size_t size) = 0;
    virtual void free(void* ptr) = 0;
    virtual ~SCFFT_Allocator() {}
};

enum SCFFT_Direction { kForward = 1, kBackward = 0 };

// These values are referred to from SC lang as well as in the following code - do not rearrange!
enum SCFFT_WindowFunction { kRectWindow = -1, kSineWindow = 0, kHannWindow = 1 };

////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions

// To initialise a specific FFT, ensure your input and output buffers exist. Internal data structures
// will be allocated using the alloc object,
// Both "fullsize" and "winsize" should be powers of two (this is not checked internally).
scfft* scfft_create(size_t fullsize, size_t winsize, SCFFT_WindowFunction wintype, float* indata, float* outdata,
                    SCFFT_Direction forward, SCFFT_Allocator& alloc);

// These two will take data from indata, use trbuf to process it, and put their results in outdata.
void scfft_dofft(scfft* f);
void scfft_doifft(scfft* f);

// destroy any resources held internally.
void scfft_destroy(scfft* f, SCFFT_Allocator& alloc);
