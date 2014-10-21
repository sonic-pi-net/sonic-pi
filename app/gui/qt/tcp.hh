/*
  This file provides a dumb c++ wrapper for sending OSC packets over TCP.
*/

/* Based on UDP version from OSCPack (https://code.google.com/p/oscpack/source/browse/)

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  (this is the zlib license)
*/

#ifndef OSCPKT_TCP_HH
#define OSCPKT_TCP_HH

#include <sys/types.h>
#if defined(_MSC_VER) || defined(WIN32)
/*
  if windows.h has been already included, be prepared for tons of
  compile errors. winsock2 must be included BEFORE windows.h . -- OR
  define WIN32_LEAN_AND_MEAN before the first #include <windows.h> to
  prevent it from including tons of crap (winsock.h etc)
*/
# include <winsock2.h>
# include <windows.h>
# include <ws2tcpip.h>
# if defined(_MSC_VER)
#  pragma comment(lib, "ws2_32.lib")
# endif
#else
# include <sys/socket.h>
# include <netinet/in.h>
# include <netdb.h>
# include <sys/time.h>
# include <unistd.h>
#endif
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <cerrno>
#include <cassert>
#include <string>
#include <vector>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include <iostream>
#include <math.h>
#include <sstream>

namespace oscpkt {

/** a wrapper class for holding an ip address, mostly used internnally */
class TcpSockAddr {
  union {
    sockaddr_storage ss; // hold an IPv4 or IPv6 address
    struct sockaddr sa;
  } addr_;
public:
  struct sockaddr &addr() { return addr_.sa; }
  const struct sockaddr &addr() const { return addr_.sa; }
  size_t maxLen() const { return sizeof addr_; }
  size_t actualLen() const {
    if (addr().sa_family == AF_UNSPEC) return 0;
    else if (addr().sa_family == AF_INET) return sizeof(struct sockaddr_in);
    else if (addr().sa_family == AF_INET6) return sizeof(struct sockaddr_in6);
    else return sizeof addr_;
  }

  TcpSockAddr() { memset(&addr_, 0, sizeof addr_); }
  bool empty() const { return addr().sa_family == AF_UNSPEC; /* this is the 0 value */ }
  /** retrieve the current port number, -1 in case of error */
  int getPort() const {
    char servname[512];
    int err = getnameinfo(&addr_.sa, sizeof addr_, 0, 0, servname, sizeof servname, NI_NUMERICSERV);
    return (err == 0 ? atoi(servname) : -1);
  }
  /* convert to a string representation (ip:port) */
  std::string asString() const {
    std::string s;
    if (addr().sa_family) {
      char hostname[512], servname[512];
      int err = getnameinfo(&addr_.sa,
                            sizeof addr_, hostname, sizeof hostname, servname, sizeof servname, NI_NUMERICHOST|NI_NUMERICSERV);
      if (err == 0) {
        s = hostname; s += ":"; s += servname;
      }
    }
    return s;
  }

  friend std::ostream &operator<<(std::ostream &os, const TcpSockAddr &ip) {
    os << "[";
    switch (ip.addr().sa_family) {
      case AF_UNSPEC: os << "AF_UNSPEC"; break;
      case AF_INET: os << "IPv4"; break;
      case AF_INET6: os << "IPv6"; break;
      default: os << "unknown family '" << ip.addr().sa_family << "'"; break;
    }
    os << " " << ip.asString() << "]";
    return os;
  }
};

/**
    just a wrapper over the classical socket stuff

    should be robust, simple to use, IPv6 ready (avoids all deprecated
    stuff such as gethostbyname etc), and portable (mac/linux/windows)

*/
struct TcpSocket {
  std::string error_message;
  int sockfd;              /* the file descriptor for the socket */
  int clientfd;            /* the file descriptor for a connected client socket*/
  TcpSockAddr local_addr   /* initialised only for bound sockets */;
  TcpSockAddr remote_addr; /* initialised for connected sockets. Also updated for bound sockets after each datagram received */

  std::vector<char> buffer;

  TcpSocket() : sockfd(-1), clientfd(-1) {
#ifdef WIN32
    WSADATA wsa_data;
    if (WSAStartup(MAKEWORD(2,2), &wsa_data) != 0) {
      setErr("winsock failed to initialise");
    }
#endif
  }

  ~TcpSocket() {
    close();
#ifdef WIN32
    WSACleanup();
#endif
  }

  void close() {
    if (sockfd != -1) {
#ifdef WIN32
      ::closesocket(sockfd);
#else
      ::close(sockfd);
#endif
      sockfd = -1;
    }
  }

  void closeClient() {
    if (clientfd != -1) {
#ifdef WIN32
      ::closesocket(sockfd);
#else
      ::close(clientfd);
#endif
      clientfd = -1;
    }
  }

  bool isOk() const { return error_message.empty(); }

  const std::string &errorMessage() const { return error_message; }

  bool isBound() const { return !local_addr.empty(); }
  int  boundPort() const { return local_addr.getPort(); }
  std::string boundPortAsString() const {
    char s[512];
#ifndef _MSC_VER
    snprintf(s, 512, "%d", boundPort());
#else
    _snprintf_s(s,512,512, "%d", boundPort());
#endif
    return s;
  }

  int  socketHandle() const { return sockfd; }
  std::string localHostName() const {
    /* this stuff is not very nice but this is what liblo does in order to
       find out a sensible name for the local host */
    char hostname_buf[512];
    if (gethostname(hostname_buf, sizeof hostname_buf) != 0)
      hostname_buf[0] = 0;
    hostname_buf[sizeof hostname_buf - 1] = 0;
    struct hostent * host = gethostbyname(hostname_buf);
    if (host) { return host->h_name; }
    return hostname_buf[0] ? hostname_buf : "localhost";
  }
  std::string localHostNameWithPort() const { return (localHostName() + ":") + boundPortAsString(); }

  enum { OPTION_UNSPEC=0, OPTION_FORCE_IPV4=1, OPTION_FORCE_IPV6=2,
         OPTION_DEFAULT=OPTION_FORCE_IPV4 // according to liblo's README, using ipv6 sockets causes issues with other non-ipv6 enabled osc software
  };

  bool bindTo(int port, int options = OPTION_DEFAULT) {
    return openServerSocket("", port, options);
  }

  bool connectTo(const std::string &host, const std::string &port, int options = OPTION_DEFAULT) {
    return openSocket(host, port, options);
  }
  bool connectTo(const std::string &host, int port, int options = OPTION_DEFAULT) {
    return openSocket(host, port, options);
  }

  void setErr(const std::string &msg) {
    if (error_message.empty()) error_message = msg;
  }

  void *packetData() { return buffer.empty() ? 0 : &buffer[0]; }
  size_t packetSize() { return buffer.size(); }
  TcpSockAddr &packetOrigin() { return remote_addr; }


  bool receiveNext() {
    if (!isOk() || sockfd == -1) { setErr("not opened.."); return false; }
    int bufferSize = 1024*128;
    buffer.resize(bufferSize);

    if(clientfd == -1){
        socklen_t len = (socklen_t)remote_addr.maxLen();
        clientfd = accept(sockfd, &remote_addr.addr(), &len);
    }

    int bufferGrowth = 1;
    int totalRead = 0;
    while(int nread = recv(clientfd, &buffer[0 + totalRead], (int)buffer.size() - totalRead, 0)){
        totalRead += nread;
        if(buffer[totalRead-1] == '\x00'){ break; }
        bufferGrowth += 1;
        buffer.resize(bufferSize * bufferGrowth);
    }

     if (totalRead < 0) {
       // maybe here we should differentiate EAGAIN/EINTR/EWOULDBLOCK from real errors
 #ifdef WIN32
       if (WSAGetLastError() != WSAEINTR && WSAGetLastError() != WSAEWOULDBLOCK &&
           WSAGetLastError() != WSAECONNRESET && WSAGetLastError() != WSAECONNREFUSED) {
         char s[512];
 #ifdef _MSC_VER
         _snprintf_s(s,512,512, "system error #%d", WSAGetLastError());
 #else
         snprintf(s,512, "system error #%d", WSAGetLastError());
 #endif
         setErr(s);
       }
 #else
       if (errno != EAGAIN && errno != EINTR && errno != EWOULDBLOCK &&
           errno != ECONNRESET && errno != ECONNREFUSED) {
         setErr(strerror(errno));
       }
 #endif

    if (!isOk()) closeClient();
       return false;
    }

    buffer.resize(totalRead);
    std::vector<char> tmp(buffer); tmp.swap(buffer);
    return true;
  }

  bool sendPacket(const void *ptr, size_t sz) {
    if (!isOk() || sockfd == -1) { setErr("not opened.."); return false; }
    if (!ptr || sz == 0) return false;

    int sent = 0;
    int bytesleft = (int)sz;
    while(sent < (int)sz){
      int res;
      res = send(sockfd, (const char*)ptr + sent, bytesleft, 0);

#ifdef WIN32
      if (res == -1 && WSAGetLastError() == WSAEINTR) continue;
#else
      if (res == -1 && errno == EINTR) continue;
#endif
      if(res == -1) { break; }
      sent += res;
      bytesleft -= res;
    }

    return (size_t)sent == sz;
  }

private:
  bool openServerSocket(const std::string &hostname, int port, int options) {
    char port_string[64];
#ifdef _MSC_VER
    _snprintf_s(port_string, 64, 64, "%d", port);
#else
    snprintf(port_string, 64, "%d", port);
#endif
    return openServerSocket(hostname, port_string, options);
  }

  bool openServerSocket(const std::string &hostname, const std::string &port, int options) {
    bool binding = hostname.empty();
    close(); error_message.clear();

    struct addrinfo hints;
    struct addrinfo *result = 0, *rp = 0;

    memset(&hints, 0, sizeof(struct addrinfo));
    if (options == OPTION_FORCE_IPV4) hints.ai_family = AF_INET;
    else if (options == OPTION_FORCE_IPV6) hints.ai_family = AF_INET6;
    else hints.ai_family = AF_UNSPEC;    /* Allow IPv4 or IPv6 -- in case of problem, try with AF_INET ...*/
    hints.ai_socktype = SOCK_STREAM; /* TCP socket */
    hints.ai_flags = (binding ? AI_PASSIVE : 0);    /* AI_PASSIVE means socket address is intended for bind */

    int err = 0;

    err = getaddrinfo(binding ? 0 : hostname.c_str(), port.empty() ? 0 : port.c_str(), &hints, &result);
    if (err != 0) {
      setErr(gai_strerror(err));
      return false;
    }

    for (rp = result; rp && sockfd==-1; rp = rp->ai_next) {
      sockfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
      if (sockfd == -1)
        continue;

      if (bind(sockfd, rp->ai_addr, (socklen_t)rp->ai_addrlen) != 0) {
        close();
      } else {
        listen(sockfd,5);

        assert((size_t)rp->ai_addrlen <= sizeof remote_addr);
        memcpy(&remote_addr.addr(), rp->ai_addr, rp->ai_addrlen);
        break;
      }
    }

    freeaddrinfo(result); result = 0;

    if (!rp) { // we failed miserably
      setErr(binding ? "bind failed" : "connect failed"); assert(sockfd == -1);
      return false;
    }

    return true;
  };

  bool openSocket(const std::string &hostname, int port, int options) {
    char port_string[64];
#ifdef _MSC_VER
    _snprintf_s(port_string, 64, 64, "%d", port);
#else
    snprintf(port_string, 64, "%d", port);
#endif
    return openSocket(hostname, port_string, options);
  }

  bool openSocket(const std::string &hostname, const std::string &port, int options) {
    if(sockfd != -1 && isOk()){
        //Already connected, just used cached socket
        return true;
    }
    else{
        close();
        error_message.clear();
    }

    bool binding = hostname.empty();

    struct addrinfo hints;
    struct addrinfo *result = 0, *rp = 0;

    memset(&hints, 0, sizeof(struct addrinfo));
    if (options == OPTION_FORCE_IPV4) hints.ai_family = AF_INET;
    else if (options == OPTION_FORCE_IPV6) hints.ai_family = AF_INET6;
    else hints.ai_family = AF_UNSPEC;    /* Allow IPv4 or IPv6 -- in case of problem, try with AF_INET ...*/
    hints.ai_socktype = SOCK_STREAM; /* TCP socket */
    hints.ai_flags = (binding ? AI_PASSIVE : 0);    /* AI_PASSIVE means socket address is intended for bind */

    int err = 0;

    err = getaddrinfo(binding ? 0 : hostname.c_str(), port.empty() ? 0 : port.c_str(), &hints, &result);
    if (err != 0) {
      setErr(gai_strerror(err));
      return false;
    }

    for (rp = result; rp && sockfd==-1; rp = rp->ai_next) {
      sockfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
      if (sockfd == -1)
        continue;

      if(connect(sockfd, rp->ai_addr, (socklen_t)rp->ai_addrlen)!=0){
        std::cerr << "Failed to connect to OSC Server" << std::endl;
        close();
      } else {
        assert((size_t)rp->ai_addrlen <= sizeof remote_addr);
        memcpy(&remote_addr.addr(), rp->ai_addr, rp->ai_addrlen);
        break;
      }
    }

    freeaddrinfo(result); result = 0;

    if (!rp) { // we failed miserably
      setErr(binding ? "bind failed" : "connect failed"); assert(sockfd == -1);
      return false;
    }
    return true;
  }
};

/** dumb struct for parsing OSC urls, such as: osc.tcp://foobar:9999/foo/plop/ */
struct TcpUrl {
  std::string protocol;
  std::string hostname;
  std::string port;
  std::string path;
  int err;
  TcpUrl() : err(0) {}
  TcpUrl(const std::string &url) { init(url); }
  bool isOk() const { return err == 0; }
  bool init(const std::string &url) {
    err = 0;
    const char *s = url.c_str();
    const char *prot = strstr(s, "osc.");
    if (prot == 0) { protocol = "tcp"; }
    else {
      const char *p2 = strstr(prot, "://");
      if (p2) { protocol.assign(prot+4, p2); }
      else { err = 1; return false; }
      s = p2+3;
    }
    const char *po = strstr(s, ":");
    if (!po) { err = 2; return false; }
    hostname.assign(s, po);
    s = po+1;

    const char *pa = strstr(s, "/");
    if (!pa) { port = s; path = "/"; }
    else { port.assign(s, pa); path = pa; }
    return true;
  }
};


} // namespace oscpkt

#endif // OSC_PKT_TCP_HH
