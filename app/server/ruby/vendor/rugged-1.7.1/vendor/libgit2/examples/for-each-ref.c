#include <git2.h>
#include "common.h"

static int show_ref(git_reference *ref, void *data)
{
	git_repository *repo = data;
	git_reference *resolved = NULL;
	char hex[GIT_OID_SHA1_HEXSIZE+1];
	const git_oid *oid;
	git_object *obj;
	
	if (git_reference_type(ref) == GIT_REFERENCE_SYMBOLIC)
		check_lg2(git_reference_resolve(&resolved, ref),
				  "Unable to resolve symbolic reference",
				  git_reference_name(ref));
	
	oid = git_reference_target(resolved ? resolved : ref);
	git_oid_fmt(hex, oid);
	hex[GIT_OID_SHA1_HEXSIZE] = 0;
	check_lg2(git_object_lookup(&obj, repo, oid, GIT_OBJECT_ANY),
			  "Unable to lookup object", hex);
	
	printf("%s %-6s\t%s\n",
		   hex,
		   git_object_type2string(git_object_type(obj)),
		   git_reference_name(ref));
	
	if (resolved)
		git_reference_free(resolved);
	return 0;
}

int lg2_for_each_ref(git_repository *repo, int argc, char **argv)
{
	UNUSED(argv);
	
	if (argc != 1)
		fatal("Sorry, no for-each-ref options supported yet", NULL);
	
	check_lg2(git_reference_foreach(repo, show_ref, repo),
			  "Could not iterate over references", NULL);
	
	return 0;
}
