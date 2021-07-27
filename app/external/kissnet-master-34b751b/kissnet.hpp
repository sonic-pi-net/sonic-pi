/*
 * MIT License
 *
 * Copyright (c) 2018-2020 Arthur Brainville (Ybalrid) and with the help of
 * Comunity Contributors!
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * INTRODUCTION
 * ============
 *
 * Kissnet is a simple C++17 layer around the raw OS provided socket API to be
 * used on IP networks with the TCP and UDP protocols.
 *
 * Kissnet is not a networking framework, and it will not process your data or
 * assist you in any way. Kissnet's only goal is to provide a simple API to send
 * and receive bytes,
 * without having to play around with a bunch of structure, file descriptors,
 * handles and pointers given to a C-style API. The other goal of kissnet is to
 * provide an API that will works in a cross platform setting.
 *
 * Kissnet will automatically manage the eventual startup/shutdown of the
 * library needed to perform socket operations on a particular platform. (e.g.
 * the Windows Socket API on MS-Windows.
 *
 * Kissnet leverages (and expect you to do so), multiple features from C++17,
 * including: std::byte, if constexpr, structured bindings, if-initializer and
 * template parameter type deduction.
 *
 * The library is structured across 4 exposed data types:
 *
 *  - buffer<size_t> : a static array of std::byte implemented via std::array.
 *  This is what you should use to hold raw data you are getting from a socket,
 *  before extracting what you need from the bytes
 *  - port_t : a 16 bit unsigned number. Represent a network port number
 *  - endpoint : a structure that represent a location where you need to connect
 *  to. Contains a hostname (as std::string) and a port number (as port_t)
 *  - socket<protocol> : a templated class that represents an ipv4 or ipv6 socket.
 *  Protocol is either TCP or UDP
 *
 * Kissnet does error handling in 2 ways:
 *
 *  1:
 *  When an operation can generate an error that the user should handle by hand
 *  anyway, a tuple containing the expected type returned, and an object that
 *  represent the status of what happens is returned.
 *
 *  For example, socket send/receive operation can discover that the connection
 *  was closed, or was shut down properly. It could also be the fact that a
 *  socket was configured "non blocking" and would have blocked in this
 *  situation. On both occasion, these methods will return the fact that 0 bytes
 *  came across as the transaction size, and the status will indicate either an
 *  error (socket no longer valid), or an actual status message (connection
 *  closed, socket would have blocked)
 *
 *  These status objects will behave like a const bool that equals "false" when
 *  an error occurred, and "true" when it's just a status notification
 *
 *  2:
 *  Fatal errors are by default handled by throwing a runtime_error exception.
 *  But, for many reasons, you may want to
 *  not use exceptions entirely.
 *
 *  kissnet give you some facilities to get fatal errors information back, and
 *  to choose how to handle it. Kissnet give you a few levers you can use:
 *
 *  - You can deactivate the exception support by #defining KISSNET_NO_EXCEP
 *  before #including kissnet.hpp. Instead, kissnet will use a function based
 *  error handler
 *  - By default, the error handler prints to stderr the error message, and
 *  abort the program
 *  - kissnet::error::callback is a function pointer that gets a string, and a
 *  context pointer. The string is the error message, and the context pointer
 * what ever you gave kissnet for the occasion. This is a global pointer that
 * you can set as you want. This will override the "print to stderr" behavior
 * at fatal error time.
 *  - kissnet::error::ctx is a void*, this will be passed to your error handler
 *  as a "context" pointer. If you need your handler to write to a log,
 *  or to turn on the HTCPCP enabled teapot on John's desk, you can.
 *  - kissnet::abortOnFatalError is a boolean that will control the call to
 *  abort(). This is independent to the fact that you did set or not an error
 *  callback. please note that any object involved with the operation that
 * triggered the fatal error is probably in an invalid state, and probably
 * deserve to be thrown away.
 */

#ifndef KISS_NET
#define KISS_NET

///Define this to not use exceptions
#ifndef KISSNET_NO_EXCEP
#define kissnet_fatal_error(STR) throw std::runtime_error(STR)
#else
#define kissnet_fatal_error(STR) kissnet::error::handle(STR);
#endif

#include <array>
#include <memory>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <cassert>
#include <stdexcept>
#include <string>
#include <utility>

#ifdef _WIN32

#define _WINSOCK_DEPRECATED_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN

#ifndef NOMINMAX
#define NOMINMAX
#endif //endif nominmax

#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>

using ioctl_setting = u_long;
using buffsize_t	= int;

#define AI_ADDRCONFIG 0x00000400

#ifndef SHUT_RDWR
#define SHUT_RDWR SD_BOTH
#endif

// taken from: https://github.com/rxi/dyad/blob/915ae4939529b9aaaf6ebfd2f65c6cff45fc0eac/src/dyad.c#L58
inline const char* inet_ntop(int af, const void* src, char* dst, socklen_t size)
{
	union
	{
		struct sockaddr sa;
		struct sockaddr_in sai;
		struct sockaddr_in6 sai6;
	} addr;
	int res;
	memset(&addr, 0, sizeof(addr));
	addr.sa.sa_family = af;
	if (af == AF_INET6)
	{
		memcpy(&addr.sai6.sin6_addr, src, sizeof(addr.sai6.sin6_addr));
	}
	else
	{
		memcpy(&addr.sai.sin_addr, src, sizeof(addr.sai.sin_addr));
	}
	res = WSAAddressToStringA(&addr.sa, sizeof(addr), 0, dst, reinterpret_cast<LPDWORD>(&size));
	if (res != 0) return NULL;
	return dst;
}

//Handle WinSock2/Windows Socket API initialization and cleanup
#pragma comment(lib, "Ws2_32.lib")
namespace kissnet
{

	namespace win32_specific
	{
		///Forward declare the object that will permit to manage the WSAStartup/Cleanup automatically
		struct WSA;

		///Enclose the global pointer in this namespace. Only use this inside a shared_ptr
		namespace internal_state
		{
			static WSA* global_WSA = nullptr;
		}

		///WSA object. Only to be constructed with std::make_shared()
		struct WSA : std::enable_shared_from_this<WSA>
		{
			//For safety, only initialize Windows Socket API once, and delete it once
			///Prevent copy construct
			WSA(const WSA&) = delete;
			///Prevent copy assignment
			WSA& operator=(const WSA&) = delete;
			///Prevent moving
			WSA(WSA&&) = delete;
			///Prevent move assignment
			WSA& operator=(WSA&&) = delete;

			///data storage
			WSADATA wsa_data;

			///Startup
			WSA() :
			 wsa_data {}
			{
				if (const auto status = WSAStartup(MAKEWORD(2, 2), &wsa_data); status != 0)
				{
					std::string error_message;
					switch (status) // https://docs.microsoft.com/en-us/windows/win32/api/winsock/nf-winsock-wsastartup#return-value
					{
						default:
							error_message = "Unknown error happened.";
							break;
						case WSASYSNOTREADY:
							error_message = "The underlying network subsystem is not ready for network communication.";
							break;
						case WSAVERNOTSUPPORTED: //unlikely, we specify 2.2!
							error_message = " The version of Windows Sockets support requested "
											"(2.2)" //we know here the version was 2.2, add that to the error message copied from MSDN
											" is not provided by this particular Windows Sockets implementation. ";
							break;
						case WSAEINPROGRESS:
							error_message = "A blocking Windows Sockets 1.1 operation is in progress.";
							break;
						case WSAEPROCLIM:
							error_message = "A limit on the number of tasks supported by the Windows Sockets implementation has been reached.";
							break;
						case WSAEFAULT: //unlikely, if this ctor is running, wsa_data is part of this object's "stack" data
							error_message = "The lpWSAData parameter is not a valid pointer.";
							break;
					}
					kissnet_fatal_error(error_message);
				}
#ifdef KISSNET_WSA_DEBUG
				std::cerr << "Initialized Windows Socket API\n";
#endif
			}

			///Cleanup
			~WSA()
			{
				WSACleanup();
				internal_state::global_WSA = nullptr;
#ifdef KISSNET_WSA_DEBUG
				std::cerr << "Cleanup Windows Socket API\n";
#endif
			}

			///get the shared pointer
			std::shared_ptr<WSA> getPtr()
			{
				return shared_from_this();
			}
		};

		///Get-or-create the global pointer
		inline std::shared_ptr<WSA> getWSA()
		{
			//If it has been created already:
			if (internal_state::global_WSA)
				return internal_state::global_WSA->getPtr(); //fetch the smart pointer from the naked pointer

			//Create in wsa
			auto wsa = std::make_shared<WSA>();

			//Save the raw address in the global state
			internal_state::global_WSA = wsa.get();

			//Return the smart pointer
			return wsa;
		}
	}

#define KISSNET_OS_SPECIFIC_PAYLOAD_NAME wsa_ptr
#define KISSNET_OS_SPECIFIC std::shared_ptr<kissnet::win32_specific::WSA> KISSNET_OS_SPECIFIC_PAYLOAD_NAME
#define KISSNET_OS_INIT KISSNET_OS_SPECIFIC_PAYLOAD_NAME = kissnet::win32_specific::getWSA()

	///Return the last error code
	inline int get_error_code()
	{
		const auto error = WSAGetLastError();

		//We need to posixify the values that we are actually using inside this header.
		switch (error)
		{
			case WSAEWOULDBLOCK:
				return EWOULDBLOCK;
			case WSAEBADF:
				return EBADF;
			case WSAEINTR:
				return EINTR;
			default:
				return error;
		}
	}
}
#else //UNIX platform

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

using ioctl_setting = int;
using buffsize_t	= size_t;

//To get consistent socket API between Windows and Linux:
static const int INVALID_SOCKET = -1;
static const int SOCKET_ERROR	= -1;
using SOCKET					= int;
using SOCKADDR_IN				= sockaddr_in;
using SOCKADDR					= sockaddr;
using IN_ADDR					= in_addr;

//Wrap them in their WIN32 names
inline int closesocket(SOCKET in)
{
	return close(in);
}

template <typename... Params>
inline int ioctlsocket(int fd, int request, Params&&... params)
{
	return ioctl(fd, request, params...);
}

#define KISSNET_OS_SPECIFIC_PAYLOAD_NAME dummy
#define KISSNET_OS_SPECIFIC char dummy
#define KISSNET_OS_INIT dummy = 42;

namespace unix_specific
{
}

inline int get_error_code()
{
	return errno;
}

#endif //ifdef WIN32

#ifdef KISSNET_USE_OPENSSL

#include <openssl/ssl.h>
#include <openssl/err.h>

#include <vector>
#include <mutex>

#endif //Kissnet use OpenSSL

#ifndef SOL_TCP
#define SOL_TCP IPPROTO_TCP
#endif

///Main namespace of kissnet
namespace kissnet
{

	///Exception-less error handling infrastructure
	namespace error
	{
		static void (*callback)(const std::string&, void* ctx) = nullptr;
		static void* ctx									   = nullptr;
		static bool abortOnFatalError						   = true;

		inline void handle(const std::string& str)
		{
			//if the error::callback function has been provided, call that
			if (callback)
			{
				callback(str, ctx);
			}
			//Print error into the standard error output
			else
			{
				fputs(str.c_str(), stderr);
			}

			//If the error abort hasn't been deactivated
			if (abortOnFatalError)
			{
				abort();
			}
		}
	}

	///low level protocol used, between TCP\TCP_SSL and UDP
	enum class protocol {
		tcp,
		tcp_ssl,
		udp
	};

	///File descriptor set types
	static constexpr int fds_read	= 0x1;
	static constexpr int fds_write	= 0x2;
	static constexpr int fds_except = 0x4;

	///buffer is an array of std::byte
	template <size_t buff_size>
	using buffer = std::array<std::byte, buff_size>;

	///port_t is the port
	using port_t = uint16_t;

	///An endpoint is where the network will connect to (address and port)
	struct endpoint
	{
		///The address to connect to
		std::string address {};

		///The port to connect to
		port_t port {};

		///Default constructor, the endpoint is not valid at that point, but you can set the address/port manually
		endpoint() = default;

		///Basically create the endpoint with what you give it
		endpoint(std::string addr, port_t prt) :
		 address { std::move(addr) }, port { prt }
		{ }

		static bool is_valid_port_number(unsigned long n)
		{
			return n < 1 << 16;
		}

		///Construct the endpoint from "address:port"
		endpoint(std::string addr)
		{
			const auto separator = addr.find_last_of(':');

			//Check if input wasn't missformed
			if (separator == std::string::npos)
				kissnet_fatal_error("string is not of address:port form");
			if (separator == addr.size() - 1)
				kissnet_fatal_error("string has ':' as last character. Expected port number here");

			//Isolate address
			address = addr.substr(0, separator);

			//Read from string as unsigned
			const auto parsed_port = strtoul(addr.substr(separator + 1).c_str(), nullptr, 10);

			//In all other cases, port was always given as a port_t type, strongly preventing it to be a number outside of the [0; 65535] range. Here it's not the case.
			//To detect errors early, check it here :
			if (!is_valid_port_number(parsed_port))
				kissnet_fatal_error("Invalid port number " + std::to_string(parsed_port));

			//Store it
			port = static_cast<port_t>(parsed_port);
		}

		///Construct an endpoint from a SOCKADDR
		endpoint(SOCKADDR* addr)
		{
			switch (addr->sa_family)
			{
				case AF_INET: {
					auto ip_addr = (SOCKADDR_IN*)(addr);
					address		 = inet_ntoa(ip_addr->sin_addr);
					port		 = ntohs(ip_addr->sin_port);
				}
				break;

				case AF_INET6: {
					auto ip_addr = (sockaddr_in6*)(addr);
					char buffer[INET6_ADDRSTRLEN];
					address = inet_ntop(AF_INET6, &(ip_addr->sin6_addr), buffer, INET6_ADDRSTRLEN);
					port	= ntohs(ip_addr->sin6_port);
				}
				break;

				default: {
					kissnet_fatal_error("Trying to construct an endpoint for a protocol familly that is neither AF_INET or AF_INET6");
				}
			}

			if (address.empty())
				kissnet_fatal_error("Couldn't construct endpoint from sockaddr(_storage) struct");
		}
	};

	//Wrap "system calls" here to avoid conflicts with the names used in the socket class

	///socket()
	inline auto syscall_socket = [](int af, int type, int protocol) {
		return ::socket(af, type, protocol);
	};

	///select()
	inline auto syscall_select = [](int nfds, fd_set* readfds, fd_set* writefds, fd_set* exceptfds, struct timeval* timeout) {
		return ::select(nfds, readfds, writefds, exceptfds, timeout);
	};

	///recv()
	inline auto syscall_recv = [](SOCKET s, char* buff, buffsize_t len, int flags) {
		return ::recv(s, buff, len, flags);
	};

	///send()
	inline auto syscall_send = [](SOCKET s, const char* buff, buffsize_t len, int flags) {
		return ::send(s, buff, len, flags);
	};

	///bind()
	inline auto syscall_bind = [](SOCKET s, const struct sockaddr* name, socklen_t namelen) {
		return ::bind(s, name, namelen);
	};

	///connect()
	inline auto syscall_connect = [](SOCKET s, const struct sockaddr* name, socklen_t namelen) {
		return ::connect(s, name, namelen);
	};

	///listen()
	inline auto syscall_listen = [](SOCKET s, int backlog) {
		return ::listen(s, backlog);
	};

	///accept()
	inline auto syscall_accept = [](SOCKET s, struct sockaddr* addr, socklen_t* addrlen) {
		return ::accept(s, addr, addrlen);
	};

	///shutdown()
	inline auto syscall_shutdown = [](SOCKET s) {
		return ::shutdown(s, SHUT_RDWR);
	};

	///Represent the status of a socket as returned by a socket operation (send, received). Implicitly convertible to bool
	struct socket_status
	{
		///Enumeration of socket status, with a 1 byte footprint
		enum values : int8_t {
			errored							= 0x0,
			valid							= 0x1,
			cleanly_disconnected			= 0x2,
			non_blocking_would_have_blocked = 0x3,
			timed_out						= 0x4

			/* ... any other info on a "still valid socket" goes here ... */

		};

		///Actual value of the socket_status.
		const values value;

		///Use the default constructor
		socket_status() :
		 value { errored } { }

		///Construct a "errored/valid" status for a true/false
		explicit socket_status(bool state) :
		 value(values(state ? valid : errored)) { }

		socket_status(values v) :
		 value(v) { }

		///Copy socket status by default
		socket_status(const socket_status&) = default;

		///Move socket status by default
		socket_status(socket_status&&) = default;

		///implicitly convert this object to const bool (as the status should not change)
		operator bool() const
		{
			//See the above enum: every value <= 0 correspond to an error, and will return false. Every value > 0 returns true
			return value > 0;
		}

		int8_t get_value()
		{
			return value;
		}

		bool operator==(values v)
		{
			return v == value;
		}
	};

#ifdef KISSNET_USE_OPENSSL
#if OPENSSL_VERSION_NUMBER < 0x10100000L
	static std::shared_ptr<std::vector<std::mutex>> SSL_lock_cs;

	class ThreadSafe_SSL
	{
	public:
		ThreadSafe_SSL()
		{
			SSL_lock_cs = std::make_shared<std::vector<std::mutex>>(CRYPTO_num_locks());

			CRYPTO_set_locking_callback((void (*)(int, int, const char*, int))
											win32_locking_callback);
		}

		~ThreadSafe_SSL() { CRYPTO_set_locking_callback(nullptr); }

	private:
		static void win32_locking_callback(int mode, int type, const char* file, int line)
		{
			auto& locks = *SSL_lock_cs;

			if (mode & CRYPTO_LOCK)
			{
				locks[type].lock();
			}
			else
			{
				locks[type].unlock();
			}
		}
	};

#endif

	class Initialize_SSL
	{
	public:
		Initialize_SSL()
		{
#if OPENSSL_VERSION_NUMBER < 0x1010001fL
			SSL_load_error_strings();
			SSL_library_init();
#else
			OPENSSL_init_ssl(
				OPENSSL_INIT_LOAD_SSL_STRINGS | OPENSSL_INIT_LOAD_CRYPTO_STRINGS, NULL);

			OPENSSL_init_crypto(
				OPENSSL_INIT_LOAD_CONFIG | OPENSSL_INIT_ADD_ALL_CIPHERS | OPENSSL_INIT_ADD_ALL_DIGESTS,
				nullptr);
#endif
		}

		~Initialize_SSL()
		{
#if OPENSSL_VERSION_NUMBER < 0x1010001fL
			ERR_free_strings();
#endif
		}

#if OPENSSL_VERSION_NUMBER < 0x10100000L
	private:
		ThreadSafe_SSL thread_setup;
#endif
	};

	static Initialize_SSL InitializeSSL;
#endif

	///Class that represent a socket
	template <protocol sock_proto>
	class socket
	{
		///Represent a number of bytes with a status information. Some of the methods of this class returns this.
		using bytes_with_status = std::tuple<size_t, socket_status>;

		///OS specific stuff. payload we have to hold onto for RAII management of the Operating System's socket library (e.g. Windows Socket API WinSock2)
		KISSNET_OS_SPECIFIC;

		///operatic-system type for a socket object
		SOCKET sock = INVALID_SOCKET;

#ifdef KISSNET_USE_OPENSSL
		SSL* pSSL		  = nullptr;
		SSL_CTX* pContext = nullptr;
#endif

		///Location where this socket is bound
		endpoint bind_loc = {};

		///Address information structures
		addrinfo getaddrinfo_hints	  = {};
		addrinfo* getaddrinfo_results = nullptr;
		addrinfo* socket_addrinfo	  = nullptr;

		void initialize_addrinfo()
		{
			int type {};
			int iprotocol {};
			if constexpr (sock_proto == protocol::tcp || sock_proto == protocol::tcp_ssl)
			{
				type	  = SOCK_STREAM;
				iprotocol = IPPROTO_TCP;
			}

			else if constexpr (sock_proto == protocol::udp)
			{
				type	  = SOCK_DGRAM;
				iprotocol = IPPROTO_UDP;
			}

			getaddrinfo_hints			  = {};
			getaddrinfo_hints.ai_family	  = AF_UNSPEC;
			getaddrinfo_hints.ai_socktype = type;
			getaddrinfo_hints.ai_protocol = iprotocol;
			getaddrinfo_hints.ai_flags	  = AI_ADDRCONFIG;
		}

		///Create and connect to socket
		socket_status connect(addrinfo* addr, int64_t timeout, bool createsocket)
		{
			if constexpr (sock_proto == protocol::tcp || sock_proto == protocol::tcp_ssl) //only TCP is a connected protocol
			{
				if (createsocket)
				{
					close();
					socket_addrinfo = nullptr;
					sock			= syscall_socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
				}

				if (sock == INVALID_SOCKET)
					return socket_status::errored;

				socket_addrinfo = addr;

				if (timeout > 0)
					set_non_blocking(true);

				int error = syscall_connect(sock, addr->ai_addr, socklen_t(addr->ai_addrlen));
				if (error == SOCKET_ERROR)
				{
					error = get_error_code();
					if (error == EWOULDBLOCK || error == EAGAIN || error == EINPROGRESS)
					{
						struct timeval tv;
						tv.tv_sec  = static_cast<long>(timeout / 1000);
						tv.tv_usec = 1000 * static_cast<long>(timeout % 1000);

						fd_set fd_write, fd_except;
						;
						FD_ZERO(&fd_write);
						FD_SET(sock, &fd_write);
						FD_ZERO(&fd_except);
						FD_SET(sock, &fd_except);

						int ret = syscall_select(static_cast<int>(sock) + 1, NULL, &fd_write, &fd_except, &tv);
						if (ret == -1)
							error = get_error_code();
						else if (ret == 0)
							error = ETIMEDOUT;
						else
						{
							socklen_t errlen = sizeof(error);
							if (getsockopt(sock, SOL_SOCKET, SO_ERROR, reinterpret_cast<char*>(&error), &errlen) != 0)
								kissnet_fatal_error("getting socket error returned an error");
						}
					}
				}

				if (timeout > 0)
					set_non_blocking(false);

				if (error == 0)
				{
					return socket_status::valid;
				}
				else
				{
					close();
					socket_addrinfo = nullptr;
					return socket_status::errored;
				}
			}

			kissnet_fatal_error("connect called for non-tcp socket");
		}

		///sockaddr struct
		sockaddr_storage socket_input  = {};
		socklen_t socket_input_socklen = 0;

	public:
		///Construct an invalid socket
		socket() = default;

		///socket<> isn't copyable
		socket(const socket&) = delete;

		///socket<> isn't copyable
		socket& operator=(const socket&) = delete;

		///Move constructor. socket<> isn't copyable
		socket(socket&& other) noexcept
		{
			KISSNET_OS_SPECIFIC_PAYLOAD_NAME = std::move(other.KISSNET_OS_SPECIFIC_PAYLOAD_NAME);
			bind_loc						 = std::move(other.bind_loc);
			sock							 = std::move(other.sock);
			socket_input					 = std::move(other.socket_input);
			socket_input_socklen			 = std::move(other.socket_input_socklen);
			getaddrinfo_results				 = std::move(other.getaddrinfo_results);
			socket_addrinfo					 = std::move(other.socket_addrinfo);

#ifdef KISSNET_USE_OPENSSL
			pSSL		   = other.pSSL;
			pContext	   = other.pContext;
			other.pSSL	   = nullptr;
			other.pContext = nullptr;
#endif

			other.sock				  = INVALID_SOCKET;
			other.getaddrinfo_results = nullptr;
			other.socket_addrinfo	  = nullptr;
		}

		///Move assign operation
		socket& operator=(socket&& other) noexcept
		{
			if (this != &other)
			{
				if (!(sock < 0) || sock != INVALID_SOCKET)
					closesocket(sock);

				KISSNET_OS_SPECIFIC_PAYLOAD_NAME = std::move(other.KISSNET_OS_SPECIFIC_PAYLOAD_NAME);
				bind_loc						 = std::move(other.bind_loc);
				sock							 = std::move(other.sock);
				socket_input					 = std::move(other.socket_input);
				socket_input_socklen			 = std::move(other.socket_input_socklen);
				getaddrinfo_results				 = std::move(other.getaddrinfo_results);
				socket_addrinfo					 = std::move(other.socket_addrinfo);

#ifdef KISSNET_USE_OPENSSL
				pSSL		   = other.pSSL;
				pContext	   = other.pContext;
				other.pSSL	   = nullptr;
				other.pContext = nullptr;
#endif

				other.sock				  = INVALID_SOCKET;
				other.getaddrinfo_results = nullptr;
				other.socket_addrinfo	  = nullptr;
			}
			return *this;
		}

		///Return true if the underlying OS provided socket representation (file descriptor, handle...). Both socket are pointing to the same thing in this case
		bool operator==(const socket& other) const
		{
			return sock == other.sock;
		}

		///Return true if socket is valid. If this is false, you probably shouldn't attempt to send/receive anything, it will probably explode in your face!
		bool is_valid() const
		{
			return sock != INVALID_SOCKET;
		}

		inline operator bool() const
		{
			return is_valid();
		}

		///Construct socket and (if applicable) connect to the endpoint
		socket(endpoint bind_to) :
		 bind_loc { std::move(bind_to) }
		{
			//operating system related housekeeping
			KISSNET_OS_INIT;

			//Do we use streams or datagrams
			initialize_addrinfo();

			if (getaddrinfo(bind_loc.address.c_str(), std::to_string(bind_loc.port).c_str(), &getaddrinfo_hints, &getaddrinfo_results) != 0)
			{
				kissnet_fatal_error("getaddrinfo failed!");
			}

			for (auto* addr = getaddrinfo_results; addr; addr = addr->ai_next)
			{
				sock = syscall_socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
				if (sock != INVALID_SOCKET)
				{
					socket_addrinfo = addr;
					break;
				}
			}

			if (sock == INVALID_SOCKET)
			{
				kissnet_fatal_error("unable to create socket!");
			}
		}

		///Construct a socket from an operating system socket, an additional endpoint to remember from where we are
		socket(SOCKET native_sock, endpoint bind_to) :
		 sock { native_sock }, bind_loc(std::move(bind_to))
		{
			KISSNET_OS_INIT;

			initialize_addrinfo();
		}

		///Set the socket in non blocking mode
		/// \param state By default "true". If put to false, it will set the socket back into blocking, normal mode
		void set_non_blocking(bool state = true) const
		{
#ifdef _WIN32
			ioctl_setting set = state ? 1 : 0;
			if (ioctlsocket(sock, FIONBIO, &set) < 0)
#else
			const auto flags	= fcntl(sock, F_GETFL, 0);
			const auto newflags = state ? flags | O_NONBLOCK : flags ^ O_NONBLOCK;
			if (fcntl(sock, F_SETFL, newflags) < 0)
#endif
				kissnet_fatal_error("setting socket to nonblock returned an error");
		}

		///Set the socket option for broadcasts
		/// \param state By default "true". If put to false, it will disable broadcasts
		void set_broadcast(bool state = true) const
		{
			const int broadcast = state ? 1 : 0;
			if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, reinterpret_cast<const char*>(&broadcast), sizeof(broadcast)) != 0)
				kissnet_fatal_error("setting socket broadcast mode returned an error");
		}

		/// Set the socket option for TCPNoDelay
		/// \param state By default "true". If put to false, it will disable TCPNoDelay
		void set_tcp_no_delay(bool state = true) const
		{
			if constexpr (sock_proto == protocol::tcp)
			{
				const int tcpnodelay = state ? 1 : 0;
				if (setsockopt(sock, SOL_TCP, TCP_NODELAY, reinterpret_cast<const char*>(&tcpnodelay), sizeof(tcpnodelay)) != 0)
					kissnet_fatal_error("setting socket tcpnodelay mode returned an error");
			}
		}

		/// Get socket status
		socket_status get_status() const
		{
			int sockerror	 = 0;
			socklen_t errlen = sizeof(sockerror);
			if (getsockopt(sock, SOL_SOCKET, SO_ERROR, reinterpret_cast<char*>(&sockerror), &errlen) != 0)
				kissnet_fatal_error("getting socket error returned an error");

			return sockerror == SOCKET_ERROR ? socket_status::errored : socket_status::valid;
		}

		///Bind socket locally using the address and port of the endpoint
		void bind()
		{
			if (syscall_bind(sock, static_cast<SOCKADDR*>(socket_addrinfo->ai_addr), socklen_t(socket_addrinfo->ai_addrlen)) == SOCKET_ERROR)
			{
				kissnet_fatal_error("bind() failed\n");
			}
		}

		///(For TCP) connect to the endpoint as client
		socket_status connect(int64_t timeout = 0)
		{
			if constexpr (sock_proto == protocol::tcp) //only TCP is a connected protocol
			{
				// try to connect to existing native socket, if any.
				auto curr_addr = socket_addrinfo;
				if (connect(curr_addr, timeout, false) != socket_status::valid)
				{
					// try to create/connect native socket for one of the other addrinfo, if any
					for (auto* addr = getaddrinfo_results; addr; addr = addr->ai_next)
					{
						if (addr == curr_addr)
							continue; // already checked

						if (connect(addr, timeout, true) == socket_status::valid)
							break; // success
					}
				}

				if (sock == INVALID_SOCKET)
					kissnet_fatal_error("unable to create connectable socket!");

				return socket_status::valid;
			}
#ifdef KISSNET_USE_OPENSSL
			else if constexpr (sock_proto == protocol::tcp_ssl) //only TCP is a connected protocol
			{
				// try to connect to existing native socket, if any.
				auto curr_addr = socket_addrinfo;
				if (connect(curr_addr, timeout, false) != socket_status::valid)
				{
					// try to create/connect native socket for one of the other addrinfo, if any
					for (auto* addr = getaddrinfo_results; addr; addr = addr->ai_next)
					{
						if (addr == curr_addr)
							continue; // already checked

						if (connect(addr, timeout, true) == socket_status::valid)
							break; // success
					}
				}

				if (sock == INVALID_SOCKET)
					kissnet_fatal_error("unable to create connectable socket!");

				auto* pMethod =
#if (OPENSSL_VERSION_NUMBER < 0x10100000L)
					TLSv1_2_client_method();
#else
					TLS_client_method();
#endif

				pContext = SSL_CTX_new(pMethod);
				pSSL	 = SSL_new(pContext);
				if (!pSSL)
					return socket_status::errored;

				if (!(static_cast<bool>(SSL_set_fd(pSSL, sock))))
					return socket_status::errored;

				if (SSL_connect(pSSL) != 1)
					return socket_status::errored;

				return socket_status::valid;
			}
#endif
		}

		///(for TCP= setup socket to listen to connection. Need to be called on binded socket, before being able to accept()
		void listen()
		{
			if constexpr (sock_proto == protocol::tcp)
			{
				if (syscall_listen(sock, SOMAXCONN) == SOCKET_ERROR)
				{
					kissnet_fatal_error("listen failed\n");
				}
			}
		}

		///(for TCP) Wait for incoming connection, return socket connect to the client. Blocking.
		socket accept()
		{
			if constexpr (sock_proto != protocol::tcp)
			{
				return { INVALID_SOCKET, {} };
			}

			sockaddr_storage socket_address;
			SOCKET s;
			socklen_t size = sizeof socket_address;

			if ((s = syscall_accept(sock, reinterpret_cast<SOCKADDR*>(&socket_address), &size)) == INVALID_SOCKET)
			{
				const auto error = get_error_code();
				switch (error)
				{
					case EWOULDBLOCK: //if socket "would have blocked" from the call, ignore
					case EINTR:		  //if blocking call got interrupted, ignore;
						return {};
				}

				kissnet_fatal_error("accept() returned an invalid socket\n");
			}

			return { s, endpoint(reinterpret_cast<SOCKADDR*>(&socket_address)) };
		}

		void close()
		{
			if (sock != INVALID_SOCKET)
			{
#ifdef KISSNET_USE_OPENSSL
				if constexpr (sock_proto == protocol::tcp_ssl)
				{
					if (pSSL)
					{
						SSL_set_shutdown(pSSL, SSL_RECEIVED_SHUTDOWN | SSL_SENT_SHUTDOWN);
						SSL_shutdown(pSSL);
						SSL_free(pSSL);
						if (pContext)
							SSL_CTX_free(pContext);
					}
				}
#endif

				closesocket(sock);
			}

			sock = INVALID_SOCKET;
		}

		void shutdown()
		{
			if (sock != INVALID_SOCKET)
			{
				syscall_shutdown(sock);
			}
		}

		///Close socket on destruction
		~socket()
		{
			close();

			if (getaddrinfo_results)
				freeaddrinfo(getaddrinfo_results);
		}

		///Select socket with timeout
		socket_status select(int fds, int64_t timeout)
		{
			fd_set fd_read, fd_write, fd_except;
			;
			struct timeval tv;

			tv.tv_sec  = static_cast<long>(timeout / 1000);
			tv.tv_usec = 1000 * static_cast<long>(timeout % 1000);

			if (fds & fds_read)
			{
				FD_ZERO(&fd_read);
				FD_SET(sock, &fd_read);
			}
			if (fds & fds_write)
			{
				FD_ZERO(&fd_write);
				FD_SET(sock, &fd_write);
			}
			if (fds & fds_except)
			{
				FD_ZERO(&fd_except);
				FD_SET(sock, &fd_except);
			}

			int ret = syscall_select(static_cast<int>(sock) + 1,
									 fds & fds_read ? &fd_read : NULL,
									 fds & fds_write ? &fd_write : NULL,
									 fds & fds_except ? &fd_except : NULL,
									 &tv);
			if (ret == -1)
				return socket_status::errored;
			else if (ret == 0)
				return socket_status::timed_out;
			return socket_status::valid;
		}

		template <size_t buff_size>
		bytes_with_status send(const buffer<buff_size>& buff, const size_t length = buff_size)
		{
			assert(buff_size >= length);
			return send(buff.data(), length);
		}

		///Send some bytes through the pipe
		bytes_with_status send(const std::byte* read_buff, size_t length)
		{
			auto received_bytes { 0 };
			if constexpr (sock_proto == protocol::tcp)
			{
				received_bytes = syscall_send(sock, reinterpret_cast<const char*>(read_buff), static_cast<buffsize_t>(length), 0);
			}
#ifdef KISSNET_USE_OPENSSL
			else if constexpr (sock_proto == protocol::tcp_ssl)
			{
				received_bytes = SSL_write(pSSL, reinterpret_cast<const char*>(read_buff), static_cast<buffsize_t>(length));
			}
#endif
			else if constexpr (sock_proto == protocol::udp)
			{
				received_bytes = sendto(sock, reinterpret_cast<const char*>(read_buff), static_cast<buffsize_t>(length), 0, static_cast<SOCKADDR*>(socket_addrinfo->ai_addr), socklen_t(socket_addrinfo->ai_addrlen));
			}

			if (received_bytes < 0)
			{
				if (get_error_code() == EWOULDBLOCK)
				{
					return { 0, socket_status::non_blocking_would_have_blocked };
				}

				return { 0, socket_status::errored };
			}

			return { received_bytes, socket_status::valid };
		}

		///receive bytes inside the buffer, return the number of bytes you got. You can choose to write inside the buffer at a specific start offset (in number of bytes)
		template <size_t buff_size>
		bytes_with_status recv(buffer<buff_size>& write_buff, size_t start_offset = 0)
		{
			auto received_bytes = 0;
			if constexpr (sock_proto == protocol::tcp)
			{
				received_bytes = syscall_recv(sock, reinterpret_cast<char*>(write_buff.data()) + start_offset, static_cast<buffsize_t>(buff_size - start_offset), 0);
			}
#ifdef KISSNET_USE_OPENSSL
			else if constexpr (sock_proto == protocol::tcp_ssl)
			{
				received_bytes = SSL_read(pSSL, reinterpret_cast<char*>(write_buff.data()) + start_offset, static_cast<buffsize_t>(buff_size - start_offset));
			}
#endif
			else if constexpr (sock_proto == protocol::udp)
			{
				socket_input_socklen = sizeof socket_input;

				received_bytes = ::recvfrom(sock, reinterpret_cast<char*>(write_buff.data()) + start_offset, static_cast<buffsize_t>(buff_size - start_offset), 0, reinterpret_cast<sockaddr*>(&socket_input), &socket_input_socklen);
			}

			if (received_bytes < 0)
			{
				const auto error = get_error_code();
				if (error == EWOULDBLOCK)
					return { 0, socket_status::non_blocking_would_have_blocked };
				if (error == EAGAIN)
					return { 0, socket_status::non_blocking_would_have_blocked };
				return { 0, socket_status::errored };
			}

			if (received_bytes == 0)
			{
				return { received_bytes, socket_status::cleanly_disconnected };
			}

			return { size_t(received_bytes), socket_status::valid };
		}

		///receive up-to len bytes inside the memory location pointed by buffer
		bytes_with_status recv(std::byte* buffer, size_t len, bool wait = true)
		{
			auto received_bytes = 0;
			if constexpr (sock_proto == protocol::tcp)
			{
				int flags;
				if (wait)
					flags = MSG_WAITALL;
				else
				{
#ifdef _WIN32
					flags = 0; // MSG_DONTWAIT not avail on windows, need to make socket nonblockingto emulate
					set_non_blocking(true);
#else
					flags = MSG_DONTWAIT;
#endif
				}
				received_bytes = syscall_recv(sock, reinterpret_cast<char*>(buffer), static_cast<buffsize_t>(len), flags);
#ifdef _WIN32
				set_non_blocking(false);
#endif
			}

#ifdef KISSNET_USE_OPENSSL
			else if constexpr (sock_proto == protocol::tcp_ssl)
			{
				received_bytes = SSL_read(pSSL, reinterpret_cast<char*>(buffer), static_cast<buffsize_t>(len));
			}
#endif

			else if constexpr (sock_proto == protocol::udp)
			{
				socket_input_socklen = sizeof socket_input;

				received_bytes = ::recvfrom(sock, reinterpret_cast<char*>(buffer), static_cast<buffsize_t>(len), 0, reinterpret_cast<sockaddr*>(&socket_input), &socket_input_socklen);
			}

			if (received_bytes < 0)
			{
				const auto error = get_error_code();
				if (error == EWOULDBLOCK)
					return { 0, socket_status::non_blocking_would_have_blocked };
				if (error == EAGAIN)
					return { 0, socket_status::non_blocking_would_have_blocked };
				return { 0, socket_status::errored };
			}

			if (received_bytes == 0)
			{
				return { received_bytes, socket_status::cleanly_disconnected };
			}

			return { size_t(received_bytes), socket_status::valid };
		}

		///Return the endpoint where this socket is talking to
		endpoint get_bind_loc() const
		{
			return bind_loc;
		}

		///Return an endpoint that originated the data in the last recv
		endpoint get_recv_endpoint() const
		{
			if constexpr (sock_proto == protocol::tcp)
			{
				return get_bind_loc();
			}
			if constexpr (sock_proto == protocol::udp)
			{
				return { (sockaddr*)&socket_input };
			}
		}

		///Return the number of bytes available inside the socket
		size_t bytes_available() const
		{
			static ioctl_setting size = 0;
			const auto status		  = ioctlsocket(sock, FIONREAD, &size);

			if (status < 0)
			{
				kissnet_fatal_error("ioctlsocket status is negative when getting FIONREAD\n");
			}

			return size > 0 ? size : 0;
		}

		///Return the protocol used by this socket
		static protocol get_protocol()
		{
			return sock_proto;
		}
	};

	///Alias for socket<protocol::tcp>
	using tcp_socket = socket<protocol::tcp>;
#ifdef KISSNET_USE_OPENSSL
	///Alias for socket<protocol::tcp_ssl>
	using tcp_ssl_socket = socket<protocol::tcp_ssl>;
#endif //KISSNET_USE_OPENSSL
	///Alias for socket<protocol::udp>
	using udp_socket = socket<protocol::udp>;
}

//cleanup preprocessor macros
#undef KISSNET_OS_SPECIFIC_PAYLOAD_NAME
#undef KISSNET_OS_SPECIFIC
#undef KISSNET_OS_INIT
#undef kissnet_fatal_error

#endif //KISS_NET
