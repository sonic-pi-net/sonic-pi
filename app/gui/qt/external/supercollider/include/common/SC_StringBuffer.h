// emacs:		-*- c++ -*-
// file:		SC_StringBuffer.h
// copyright:	2003 stefan kersten <steve@k-hornz.de>
// cvs:			$Id$

// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
// USA

#pragma once

#include <stdarg.h>
#include <stdlib.h>

// =====================================================================
// SC_StringBuffer - Autogrowing string buffer.
// =====================================================================

class SC_StringBuffer {
public:
    SC_StringBuffer(size_t initialSize = 0);
    SC_StringBuffer(const SC_StringBuffer& other);
    ~SC_StringBuffer();

    size_t getCapacity() const { return mCapacity; }
    size_t getSize() const { return mPtr - mData; }
    size_t getRemaining() const { return mCapacity - getSize(); }
    char* getData() const { return mData; }

    bool isEmpty() const { return getSize() == 0; }

    void finish() { append('\0'); }
    void reset() { mPtr = mData; }
    void append(const char* src, size_t len);
    void append(char c);
    void append(const char* str);
    void vappendf(const char* fmt, va_list vargs);
    void appendf(const char* fmt, ...);

protected:
    enum { kGrowAlign = 256, kGrowMask = kGrowAlign - 1 };

    void growBy(size_t request);

private:
    size_t mCapacity;
    char* mPtr;
    char* mData;
};
