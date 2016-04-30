//  shared memory interface to the supercollider server
//  Copyright (C) 2011 Tim Blechmann
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

#ifndef SERVER_SHM_HPP
#define SERVER_SHM_HPP

#include "scope_buffer.hpp"

#include <boost/version.hpp>
#include <boost/foreach.hpp>
#include <boost/ref.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/containers/vector.hpp>

namespace detail_server_shm {

using std::string; using std::pair;

using boost::ref;

namespace bi = boost::interprocess;
using bi::managed_shared_memory; using bi::shared_memory_object;

static inline string make_shmem_name(unsigned int port_number)
{
	return string("SuperColliderServer_") + boost::lexical_cast<string>(port_number);
}

class server_shared_memory
{
public:
	typedef offset_ptr<float> sh_float_ptr;
	typedef offset_ptr<scope_buffer> scope_buffer_ptr;

	typedef bi::allocator<scope_buffer_ptr, managed_shared_memory::segment_manager> scope_buffer_ptr_allocator;
	typedef bi::vector<scope_buffer_ptr, scope_buffer_ptr_allocator> scope_buffer_vector;

	server_shared_memory(managed_shared_memory & segment, int control_busses, int num_scope_buffers = 128):
		num_control_busses(control_busses),

		scope_buffers(scope_buffer_ptr_allocator(segment.get_segment_manager()))
	{
		control_busses_ = (float*)segment.allocate(control_busses * sizeof(float));
		std::fill(control_busses_.get(), control_busses_.get() + control_busses, 0.f);

		for (int i = 0; i != num_scope_buffers; ++i) {
			scope_buffer * raw_scope_ptr = (scope_buffer*)segment.allocate(sizeof(scope_buffer));
			new(raw_scope_ptr) scope_buffer();
			scope_buffer_ptr buf = raw_scope_ptr;
			scope_buffers.push_back(buf);
		}
	}

	void destroy(managed_shared_memory & segment)
	{
		segment.deallocate(control_busses_.get());

		for (size_t i = 0; i != scope_buffers.size(); ++i)
			segment.deallocate(scope_buffers[i].get());
	}

	void set_control_bus(int bus, float value)
	{
		// TODO: we need to set the control busses via a work queue
	}

	float * get_control_busses(void)
	{
		return control_busses_.get();
	}

	scope_buffer * get_scope_buffer(unsigned int index)
	{
		if (index < scope_buffers.size())
			return scope_buffers[index].get();
		else
			return 0;
	}

private:
	string shmem_name;
	int num_control_busses;
	sh_float_ptr control_busses_; // control busses
	scope_buffer_vector scope_buffers;
};

class server_shared_memory_creator
{
public:
	server_shared_memory_creator(unsigned int port_number, unsigned int control_busses):
		shmem_name(detail_server_shm::make_shmem_name(port_number)),
		segment(bi::open_or_create, shmem_name.c_str(), 8192 * 1024)
	{
#if (BOOST_VERSION < 105100)
		segment.flush();
#endif

		const int num_scope_buffers = 128;
		size_t scope_pool_size = num_scope_buffers * sizeof(float) * 8192; // pessimize, about 4 MB
		void * memory_for_scope_pool = segment.allocate(scope_pool_size);
		scope_pool.init(memory_for_scope_pool, scope_pool_size);

		shm = segment.construct<server_shared_memory>(shmem_name.c_str())(ref(segment), control_busses,
																		  num_scope_buffers);
	}

	static void cleanup(unsigned int port_number)
	{
		shared_memory_object::remove(detail_server_shm::make_shmem_name(port_number).c_str());
	}

	~server_shared_memory_creator(void)
	{
		if (shm)
			disconnect();
	}

	void disconnect()
	{
		shm->destroy(segment);
		segment.destroy<server_shared_memory>(shmem_name.c_str());
		shared_memory_object::remove(shmem_name.c_str());
		shm = NULL;
	}

	float * get_control_busses(void)
	{
		return shm->get_control_busses();
	}

	scope_buffer_writer get_scope_buffer_writer(unsigned int index, unsigned int channels, unsigned int size)
	{
		scope_buffer *buf = shm->get_scope_buffer(index);
		if (buf)
			return scope_buffer_writer(buf, scope_pool, channels, size);
		else
			return scope_buffer_writer();
	}

	void release_scope_buffer_writer( scope_buffer_writer & writer )
	{
		writer.release( scope_pool );
	}

private:
	string shmem_name;
	managed_shared_memory segment;
	scope_buffer_pool scope_pool;

protected:
	server_shared_memory * shm;
};


class server_shared_memory_client
{
public:
	server_shared_memory_client(unsigned int port_number):
		shmem_name(detail_server_shm::make_shmem_name(port_number)),
		segment(bi::open_only, shmem_name.c_str())
	{
		pair<server_shared_memory*, size_t> res = segment.find<server_shared_memory> (shmem_name.c_str());
		if (res.second != 1)
			throw std::runtime_error("Cannot connect to shared memory");
		shm = res.first;
	}

	float * get_control_busses(void)
	{
		return shm->get_control_busses();
	}

	scope_buffer_reader get_scope_buffer_reader(unsigned int index)
	{
		scope_buffer *buf = shm->get_scope_buffer(index);
		return scope_buffer_reader(buf);
	}

private:
	string shmem_name;
	managed_shared_memory segment;
	server_shared_memory * shm;
};

}

using detail_server_shm::server_shared_memory_client;
using detail_server_shm::server_shared_memory_creator;
using detail_server_shm::scope_buffer_writer;
using detail_server_shm::scope_buffer_reader;
using detail_server_shm::scope_buffer;

#endif /* SERVER_SHM_HPP */
