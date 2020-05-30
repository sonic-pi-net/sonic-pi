#include "mailmap.h"

typedef struct mailmap_entry {
	const char *real_name;
	const char *real_email;
	const char *replace_name;
	const char *replace_email;
} mailmap_entry;

static const mailmap_entry resolved[] = {
	{ "Brad", "cto@company.xx", "Brad", "cto@coompany.xx" },
	{ "Brad L", "cto@company.xx", "Brad L", "cto@coompany.xx" },
	{ "Some Dude", "some@dude.xx", "nick1", "bugs@company.xx" },
	{ "Other Author", "other@author.xx", "nick2", "bugs@company.xx" },
	{ "nick3", "bugs@company.xx", "nick3", "bugs@company.xx" },
	{ "Other Author", "other@author.xx", "Some Garbage", "nick2@company.xx" },
	{ "Phil Hill", "phil@company.xx", "unknown", "phil@company.xx" },
	{ "Joseph", "joseph@company.xx", "Joseph", "bugs@company.xx" },
	{ "Santa Claus", "santa.claus@northpole.xx", "Clause", "me@company.xx" },
	{ "Charles", "charles@charles.xx", "Charles", "charles@charles.xx" }
};
