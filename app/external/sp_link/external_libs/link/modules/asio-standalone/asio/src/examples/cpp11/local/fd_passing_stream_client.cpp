//
// fd_passing_stream_client.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
// Copyright (c) 2021 Heiko Hund (heiko at openvpn dot net)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

// Demonstrates how to pass file descriptors between processes with Asio.
// The client send a file name (destfile) to the server. The server opens
// the file and the associated file descriptor back to the client.

#include <cstdlib>
#include <cstring>
#include <iostream>
#include "asio.hpp"

#if defined(ASIO_HAS_LOCAL_SOCKETS)

#include <sys/types.h>
#include <sys/socket.h>

using asio::local::stream_protocol;

constexpr std::size_t max_length = 1024;

int main(int argc, char* argv[])
{
  try
  {
    if (argc != 2)
    {
      std::cerr << "Usage: fd_passing_stream_client <serversocket>\n";
      return 1;
    }

    asio::io_context io_context;

    stream_protocol::socket s(io_context);
    s.connect(stream_protocol::endpoint(argv[1]));

    std::cout << "Enter path to write to: ";
    char request[max_length];
    std::cin.getline(request, max_length);
    size_t request_length = std::strlen(request);
    asio::write(s, asio::buffer(request, request_length));

    char reply[max_length];
    struct msghdr msg = {};
    struct iovec iov = { reply, sizeof(reply) };
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;

    union
    {
      struct cmsghdr align;
      char buf[CMSG_SPACE(sizeof(int))];
    } cmsgu;
    msg.msg_control = cmsgu.buf;
    msg.msg_controllen = sizeof(cmsgu.buf);

    ::recvmsg(s.native_handle(), &msg, 0);

    int fd = -1;
    struct cmsghdr* cmsg = CMSG_FIRSTHDR(&msg);
    while (cmsg != NULL)
    {
      if (cmsg->cmsg_level == SOL_SOCKET && cmsg->cmsg_type == SCM_RIGHTS)
      {
        std::memcpy(&fd, CMSG_DATA(cmsg), sizeof(fd));
        break;
      }
      cmsg = CMSG_NXTHDR(&msg, cmsg);
    }

    if (fd != -1)
    {
      std::cout << "File descriptor received is: " << fd << "\n";
      FILE* f(::fdopen(fd, "w+"));
      if (f)
      {
        ::fprintf(f, "stream_client writing to received fd #%d\n", fd);
        ::fclose(f);
      }
      else
        ::close(fd);
    }
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}

#else // defined(ASIO_HAS_LOCAL_SOCKETS)
# error Local sockets not available on this platform.
#endif // defined(ASIO_HAS_LOCAL_SOCKETS)
