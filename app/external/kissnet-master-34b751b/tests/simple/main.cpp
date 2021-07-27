#include <iostream>
#include <kissnet.hpp>

int main()
{
	std::cout << "hello world\n";

	//if comoiler crash, theses types doens't exist
	(void)kissnet::protocol::tcp;
	(void)kissnet::protocol::udp;

	//give me 2k of memory, please!
	kissnet::buffer<2048> test_buffer;

	std::cout << "buffer size : " << test_buffer.size() << '\n';
	std::cout << "buffer data start address : 0x" << std::hex << (size_t)test_buffer.data() << std::dec << '\n';

	//If a byte is not a single byte, It will not work
	static_assert(sizeof(std::byte) == 1);

	//Can manipulate the value of a byte directly?
	const auto size = test_buffer.size();

	//setting bytes insdie the buffer to a specif value
	for (size_t i = 0; i < size; ++i)
		test_buffer[i] = std::byte { static_cast<uint8_t>(i % 0xFF) };

	//Print them
	for (size_t i = 0; i < size; ++i)
		std::cout << std::hex << std::to_integer<int>(test_buffer[i]) << std::dec << '\n';

	return 0;
}
