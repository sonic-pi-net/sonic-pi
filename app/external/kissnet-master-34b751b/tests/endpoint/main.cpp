#include <iostream>
#include <kissnet.hpp>

int main()
{
	{
		kissnet::endpoint empty;

		if (empty.address.empty())
		{
			std::cout << "endpoint empty is empty\n";
		}
		else
		{
			std::cout << "endpoint empty is NOT empty\n";
		}

		std::cout << empty.port << '\n';
	}

	{
		kissnet::endpoint explicitly("www.google.com", 80);

		std::cout << explicitly.address << '\n';
		std::cout << explicitly.port << '\n';
	}

	{
		kissnet::endpoint by_string("www.google.com:80");
		std::cout << by_string.address << '\n';
		std::cout << by_string.port << '\n';
	}

	return 0;
}
