#include <iostream>
#include <thread>

#include <kissnet.hpp>
namespace kn = kissnet;

void loopback_udp(const std::string send_address, const std::string recv_address)
{
	//Socket used to send, the "endpoint" is the destination of the data
	kn::udp_socket a_socket(kn::endpoint(send_address, 6666));

	//Socket used to receive, the "endpoint" is where to listen to data
	kn::udp_socket b_socket(kn::endpoint(recv_address, 6666));
	b_socket.bind();

	//Byte buffer
	kn::buffer<16> buff;

	//Build data to send (flat array of bytes
	for (unsigned char i = 0; i < 16; i++)
		buff[i] = std::byte { i };

	//Send data
	a_socket.send(buff.data(), 16);

	//We do know, for the sake of the example, that there are 16 bytes to get from the network
	kn::buffer<16> recv_buff;

	//Actually print bytes_available
	std::cout << "avaliable in UDP socket : " << b_socket.bytes_available() << " bytes\n";

	//You receive in the same way
	auto [received_bytes, status] = b_socket.recv(recv_buff);
	const auto from				  = b_socket.get_recv_endpoint();

	//Print the data
	std::cout << "Received: ";

	for (unsigned char i = 0; i < 16; i++)
	{
		std::cout << std::hex << std::to_integer<int>(recv_buff[i]) << std::dec << ' ';
	}

	//Print who send the data
	std::cout << "From: " << from.address << ' ' << from.port << '\n';
}

int main()
{
	loopback_udp("127.0.0.1", "0.0.0.0");
	loopback_udp("::1", "::");

	//So long, and thanks for all the fish
	return EXIT_SUCCESS;
}
