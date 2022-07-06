#include <iostream>

#include <kissnet.hpp>
namespace kn = kissnet;

void acceptor(const std::string address)
{
	//setup socket
	kn::tcp_socket server(kn::endpoint(address, 8080));
	server.bind();
	server.listen();

	//Wait for one co
	auto client = server.accept();

	//Read once in a 1k buffer
	kn::buffer<1024> buff;
	const auto [size, status] = client.recv(buff);

	//Add null terminator, and print as string
	if (size < buff.size()) buff[size] = std::byte { 0 };
	std::cout << reinterpret_cast<const char*>(buff.data()) << '\n';
}

int main()
{
	acceptor("0.0.0.0");
	acceptor("::");

	//So long, and thanks for all the fish
	return 0;
}
