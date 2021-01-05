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

extern "C" {
#include "tlsf.h"
}

namespace detail_server_shm {

using boost::interprocess::offset_ptr;
using std::atomic;

class scope_buffer_writer;
class scope_buffer_reader;

class scope_buffer_pool {
public:
    void init(void* pool, size_t size_of_pool) {
        pool_ = (char*)pool;
        memset(pool_, 0, size_of_pool);
        init_memory_pool(size_of_pool, pool_);
    }

    void* allocate(size_t bytes) { return malloc_ex(bytes, pool_); }

    void deallocate(void* ptr) { free_ex(ptr, pool_); }

private:
    friend class server_shared_memory;
    char* pool_;
};

class scope_buffer {
    friend class scope_buffer_writer;
    friend class scope_buffer_reader;

    typedef offset_ptr<float> sh_float_ptr;

    enum status { free = 0, initialized };

    atomic<int> _status;

    unsigned int _size;
    unsigned int _channels;
    sh_float_ptr _data;

    /*
    Reader/writer synchronization mechanism:

    _stage, _in and _out are indexes into _state - an array of 3 equal data regions.

    _out denotes the region where the writer writes.
    _in denotes the region where the reader reads.
    _stage denotes the region where data is exchanged between the writer and the reader.

    After the writer is done writing, it sets the changed flag of the _in region,
    and swaps _in with _stage.

    The reader polls the changed flag of the _stage region. If it is set, it swaps _out
    with _stage, reads the new _out region, and unsets its changed flag.
    */

    atomic<int> _stage;
    int _in;
    int _out;

    struct data_desc {
        data_desc(): data(0), frames(0), changed(false) {}
        sh_float_ptr data;
        unsigned int frames;
        atomic<bool> changed;
    } _state[3];

public:
    scope_buffer(): _status(free), _stage(0), _in(1), _out(2) {}

private:
    // writer interface

    bool allocate(scope_buffer_pool& pool, unsigned int channels, unsigned int size) {
        bool available = _status.load(std::memory_order_relaxed) == free;
        if (!available)
            return false;

        _size = size;
        _channels = channels;

        unsigned int asset_size = channels * size;
        _data = (float*)pool.allocate(asset_size * 3 * sizeof(float));
        if (_data == NULL)
            return false;

        _state[0].data = _data;
        _state[1].data = _data + asset_size;
        _state[2].data = _data + asset_size + asset_size;

        _status.store(initialized, std::memory_order_release);

        return true;
    }

    void release(scope_buffer_pool& pool) {
        bool allocated = _status.load(std::memory_order_relaxed) != free;
        if (!allocated)
            return;

        pool.deallocate(_data.get());

        _status.store(free, std::memory_order_release);
    }

    float* write_address() { return _state[_in].data.get(); }

    void push(unsigned int frames) {
        _state[_in].frames = frames;
        _state[_in].changed.store(true, std::memory_order_relaxed);
        _in = _stage.exchange(_in, std::memory_order_release);
    }

    // reader interface

    bool valid() { return _status.load(std::memory_order_acquire) == initialized; }

    float* read_address() { return _state[_out].data.get(); }

    unsigned int pull() {
        int stage = _stage.load(std::memory_order_relaxed);
        bool changed = _state[stage].changed.load(std::memory_order_relaxed);

        if (changed) {
            _state[_out].changed.store(false, std::memory_order_relaxed);
            _out = _stage.exchange(_out, std::memory_order_acquire);
        }

        return _state[_out].frames;
    }
};

class scope_buffer_writer {
public:
    scope_buffer* buffer;

    scope_buffer_writer(scope_buffer* buffer = 0): buffer(buffer) {}

    scope_buffer_writer(scope_buffer* buf, scope_buffer_pool& pool, unsigned int channels, unsigned int size):
        buffer(buf) {
        if (!buffer->allocate(pool, channels, size))
            buffer = 0;
    }

    bool valid() { return buffer != 0; }

    float* data() { return buffer->write_address(); }

    unsigned int max_size() { return buffer->_size; }

    void push(unsigned int frames) { buffer->push(frames); }

    void release(scope_buffer_pool& pool) { buffer->release(pool); }
};

// FIXME: how do we ensure that scope_buffer data members used in the reader
// are consistent among themselves at all times???

class scope_buffer_reader {
    scope_buffer* buffer;

public:
    scope_buffer_reader(scope_buffer* buffer_ = 0): buffer(buffer_) {}

    bool valid() {
        // places an acquire memory ordering fence
        return (buffer && buffer->valid());
    }

    bool pull(unsigned int& frames) {
        unsigned int new_frames = buffer->pull();

        if (new_frames)
            frames = new_frames;

        return new_frames != 0;
    }

    float* data() { return buffer->read_address(); }

    unsigned int max_frames() { return buffer->_size; }

    unsigned int channels() { return buffer->_channels; }
};

} /* namespace detail_server_shm */
