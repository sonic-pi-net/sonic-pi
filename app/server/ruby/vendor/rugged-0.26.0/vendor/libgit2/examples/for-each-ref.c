#include <git2.h>
#include <stdio.h>
#include "common.h"

static int show_ref(git_reference *ref, void *data)
{
        git_repository *repo = data;
        git_reference *resolved = NULL;
        char hex[GIT_OID_HEXSZ+1];
        const git_oid *oid;
        git_object *obj;

        if (git_reference_type(ref) == GIT_REF_SYMBOLIC)
                check_lg2(git_reference_resolve(&resolved, ref),
                          "Unable to resolve symbolic reference",
                          git_reference_name(ref));

        oid = git_reference_target(resolved ? resolved : ref);
        git_oid_fmt(hex, oid);
        hex[GIT_OID_HEXSZ] = 0;
        check_lg2(git_object_lookup(&obj, repo, oid, GIT_OBJ_ANY),
                  "Unable to lookup object", hex);

        printf("%s %-6s\t%s\n",
               hex,
               git_object_type2string(git_object_type(obj)),
               git_reference_name(ref));

        if (resolved)
                git_reference_free(resolved);
        return 0;
}

int main(int argc, char **argv)
{
        git_repository *repo;
        git_libgit2_init();

        if (argc != 1 || argv[1] /* silence -Wunused-parameter */)
                fatal("Sorry, no for-each-ref options supported yet", NULL);

        check_lg2(git_repository_open(&repo, "."),
                  "Could not open repository", NULL);
        check_lg2(git_reference_foreach(repo, show_ref, repo),
                  "Could not iterate over references", NULL);

        git_libgit2_shutdown();
        return 0;
}
