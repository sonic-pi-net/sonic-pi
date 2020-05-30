//  Stethoscope shared memory buffer implementation
//  This file is part of SuperCollider
//
//  Copyright (C) 2011 Jakob Leben
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
#pragma once

#include <atomic>

#include <boost/interprocess/offset_ptr.hpp>

namespace detail_server_shm
{

using boost::interprocess::offset_ptr;
using std::atomic;

class scope_buffer_writer;
class scope_buffer_reader;

class scope_buffer
{
    friend class scope_buffer_writer;
    friend class scope_buffer_reader;

    typedef offset_ptr<float> sh_float_ptr;

    enum status
    {
        free = 0,
        initialized
    };

    atomic<int> _status;

    unsigned int _size;
    unsigned int _channels;
    sh_float_ptr _data;

    atomic<int> _stage;
    int _in;
    int _out;

    struct data_desc
    {
        data_desc()
            : data(0)
            , frames(0)
            , changed(false)
        {
        }
        sh_float_ptr data;
        unsigned int frames;
        atomic<bool> changed;
    } _state[3];

public:
    scope_buffer()
        : _status(free)
        , _stage(0)
        , _in(1)
        , _out(2)
    {
    }

private:
    bool valid()
    {
        return _status.load(std::memory_order_acquire) == initialized;
    }

    float* read_address()
    {
        return _state[_out].data.get();
    }

    unsigned int pull()
    {
        int stage = _stage.load(std::memory_order_relaxed);
        bool changed = _state[stage].changed.load(std::memory_order_relaxed);

        if (changed)
        {
            _state[_out].changed.store(false, std::memory_order_relaxed);
            _out = _stage.exchange(_out, std::memory_order_acquire);
        }

        return _state[_out].frames;
    }
};

// FIXME: how do we ensure that scope_buffer data members used in the reader
// are consistent among themselves at all times???
class scope_buffer_reader
{
    scope_buffer* buffer;

public:
    scope_buffer_reader(scope_buffer* buffer_ = 0)
        : buffer(buffer_)
    {
    }

    bool valid()
    {
        // places an acquire memory ordering fence
        return (buffer && buffer->valid());
    }

    bool pull(unsigned int& frames)
    {
        unsigned int new_frames = buffer->pull();

        if (new_frames)
            frames = new_frames;

        return new_frames != 0;
    }

    float* data()
    {
        return buffer->read_address();
    }

    unsigned int max_frames()
    {
        return buffer->_size;
    }

    unsigned int channels()
    {
        return buffer->_channels;
    }
};

} /* namespace detail_server_shm */

