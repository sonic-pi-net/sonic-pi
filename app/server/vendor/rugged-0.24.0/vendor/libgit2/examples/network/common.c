#include "common.h"
#include <stdio.h>

/* Shamelessly borrowed from http://stackoverflow.com/questions/3417837/
 * with permission of the original author, Martin Pool.
 * http://sourcefrog.net/weblog/software/languages/C/unused.html
 */
#ifdef UNUSED
#elif defined(__GNUC__)
# define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define UNUSED(x) /*@unused@*/ x
#else
# define UNUSED(x) x
#endif

int cred_acquire_cb(git_cred **out,
		const char * UNUSED(url),
		const char * UNUSED(username_from_url),
		unsigned int UNUSED(allowed_types),
		void * UNUSED(payload))
{
	char username[128] = {0};
	char password[128] = {0};

	printf("Username: ");
	scanf("%s", username);

	/* Yup. Right there on your terminal. Careful where you copy/paste output. */
	printf("Password: ");
	scanf("%s", password);

	return git_cred_userpass_plaintext_new(out, username, password);
}
