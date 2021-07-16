#include <iostream>
#include <thread>
#include <chrono>

#include <kissnet.hpp>
using namespace std::chrono_literals;
namespace kn = kissnet;

void loopback_tcp(const std::string listen_address, const std::string connect_address, const int port)
{
	std::thread listen_th([&] {
		kn::tcp_socket listener(kn::endpoint(listen_address, port));
		listener.set_non_blocking();
		listener.bind();
		listener.listen();

		const char* hello_goodbye			= "Hello hello, I don't know why you say goodbye, I say hello!";
		const size_t hello_goodbye_size		= strlen(hello_goodbye);
		const std::byte* hello_goodbye_byte = reinterpret_cast<const std::byte*>(hello_goodbye);

		for (size_t i = 0; i < 50; ++i)
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
		listener.close();
	});

	listen_th.detach();

	std::this_thread::sleep_for(200ms);
	kn::tcp_socket a_socket(kn::endpoint(connect_address, port));

	a_socket.connect();

	//Receive data into a buffer
	kn::buffer<4096> static_buffer;

	//Get the data, and the lengh of data
	const auto [data_size, socket_status] = a_socket.recv(static_buffer);
	a_socket.close();

	//To print it as a good old C string, add a null terminator
	if (data_size < static_buffer.size())
		static_buffer[data_size] = std::byte { '\0' };

	//Print the raw data as text into the terminal (should display html/css code here)
	std::cout << reinterpret_cast<const char*>(static_buffer.data()) << '\n';
}

int main()
{
	// Increment port number to avoid bind failure on TIME-WAIT connections
	loopback_tcp("0.0.0.0", "127.0.0.1", 6666);
	loopback_tcp("::", "::1", 6667);

	return EXIT_SUCCESS;
}
