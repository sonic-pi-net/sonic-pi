#include <iostream>
#include <kissnet.hpp>

int main()
{
	kissnet::error::abortOnFatalError = false;

	kissnet::error::handle("test what code would be called on error when built without exception\n");

	kissnet::error::callback = [](const std::string& str, void* ctx) {
		std::cerr << "this is the callback : ";
		std::cerr << str;
		(void)ctx;
	};

	kissnet::error::handle("test our custom callback");

	return 0;
}
