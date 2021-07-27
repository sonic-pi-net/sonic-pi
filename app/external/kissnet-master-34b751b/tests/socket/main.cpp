// IPv4: ./socket 127.0.0.1 0.0.0.0
// IPv6: ./socket ::1 ::
#include <iostream>
#include <thread>
#include <chrono>
#include <vector>

#define KISSNET_WSA_DEBUG
#include <kissnet.hpp>
using namespace std::chrono_literals;
namespace kn = kissnet;

int main(int argc, char* argv[])
{
	std::string addr_send = "127.0.0.1";
	if (argc > 1) addr_send = argv[1];

	std::string addr_recv = "0.0.0.0";
	if (argc > 2) addr_recv = argv[2];

	{
		//Create a kissnet TCP ipv4 socket
		kn::tcp_socket a_socket(kn::endpoint("avalon.ybalrid.info:80"));
		a_socket.connect();

		//Create a "GET /" HTTP request, and send that packet into the socket
		auto get_index_request = std::string { "GET / HTTP/1.1\r\nHost: percival.ybalrid.info\r\n\r\n" };

		//Send request
		a_socket.send(reinterpret_cast<const std::byte*>(get_index_request.c_str()), get_index_request.size());
		{
			//Receive data into a buffer
			kn::buffer<4096> static_buffer;

			//Useless wait, just to show how long the response was
			std::this_thread::sleep_for(1s);

			//Print how much data our OS has for us
			std::cout << "bytes available to read : " << a_socket.bytes_available() << '\n';

			//Get the data, and the lengh of data
			const auto [data_size, socket_status] = a_socket.recv(static_buffer);

			//To print it as a good old C string, add a null terminator
			if (data_size < static_buffer.size())
				static_buffer[data_size] = std::byte { '\0' };

			//Print the raw data as text into the terminal (should display html/css code here)
			std::cout << reinterpret_cast<const char*>(static_buffer.data()) << '\n';
		}
		{
			std::vector<std::byte> heap_buffer(4096);

			a_socket.send(reinterpret_cast<const std::byte*>(get_index_request.c_str()), get_index_request.size());
			std::this_thread::sleep_for(1s);
			const auto [data_size, socket_status] = a_socket.recv(heap_buffer.data(), heap_buffer.size());

			//To print it as a good old C string, add a null terminator
			if (data_size < heap_buffer.size())
				heap_buffer[data_size] = std::byte { '\0' };

			//Print the raw data as text into the terminal (should display html/css code here)
			std::cout << reinterpret_cast<const char*>(heap_buffer.data()) << '\n';
		}
	}

	std::cerr << "Every socket object used here has gone out of scope, Thanks to RAII, this will actually close WSA on Windows\n";

	{
		//Socket used to send, the "endpoint" is the destination of the data
		kn::udp_socket a_socket(kn::endpoint(addr_send, 6666));

		//Socket used to receive, the "endpoint" is where to listen to data
		kn::udp_socket b_socket(kn::endpoint(addr_recv, 6666));
		b_socket.bind();

		//Byte buffer
		kn::buffer<16> buff;

		//Build data to send (flat array of bytes
		for (unsigned char i = 0; i < 16; i++)
			buff[i] = std::byte { i };

		//Send data
		a_socket.send(buff, 16);

		//Same deal as above
		std::this_thread::sleep_for(1s);

		//We do know, for the sake of the example, that there are 16 bytes to get from the network
		kn::buffer<16> recv_buff;

		//Actually print bytes_available
		std::cout << "avaliable in UDP socket : " << b_socket.bytes_available() << " bytes\n";

		//You receive in the same way
		b_socket.recv(recv_buff);
		auto from = b_socket.get_recv_endpoint();

		//Print the data
		std::cout << "Received: ";

		for (unsigned char i = 0; i < 16; i++)
		{
			std::cout << std::hex << std::to_integer<int>(recv_buff[i]) << std::dec << ' ';
		}

		//Print who send the data
		std::cout << "From: " << from.address << ' ' << from.port << '\n';
	}

	std::cerr << "Every socket object used here has gone out of scope, Thanks to RAII, this will actually close WSA on Windows\n";

	//nonblocking listener
	{
		kn::tcp_socket listener(kn::endpoint("0.0.0.0:6666"));
		listener.set_non_blocking();
		listener.bind();
		listener.listen();

		const char* hello_goodbye			= "Hello hello, I don't no why you say GooBye, I say Hello!";
		const size_t hello_goodbye_size		= strlen(hello_goodbye);
		const std::byte* hello_goodbye_byte = reinterpret_cast<const std::byte*>(hello_goodbye);
		bool run							= true;

		std::thread quit_th([&] {
			std::cout << "press return to quit\n";
			std::cin.get();
			std::cin.clear();
			run = false;
		});

		quit_th.detach();

		for (size_t i = 0; run && i < 50; ++i)
		{
			std::this_thread::sleep_for(100ms);
			if (auto socket = listener.accept(); socket.is_valid())
			{
				std::cout << "Accepted connect\n";
				socket.send(hello_goodbye_byte, hello_goodbye_size);
			}
			else
			{
				std::cout << "No connections to accept...\n";
			}
		}
	}

	std::cerr << "Every socket object used here has gone out of scope, Thanks to RAII, this will actually close WSA on Windows\n";

	//So long, and thanks for all the fish
	return EXIT_SUCCESS;
}
