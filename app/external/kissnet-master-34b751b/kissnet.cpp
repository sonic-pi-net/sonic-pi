#include "kissnet.hpp"

int main()
{
#if defined(KISSNET_USE_OPENSSL)
	printf("kissnet is using OpenSSL.");
#else
	printf("kissnet is not using OpenSSL.");
#endif
}
