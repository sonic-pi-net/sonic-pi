/*
 * libgit2 "general" example - shows basic libgit2 concepts
 *
 * Written by the libgit2 contributors
 *
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide. This software is distributed without any warranty.
 *
 * You should have received a copy of the CC0 Public Domain Dedication along
 * with this software. If not, see
 * <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

// [**libgit2**][lg] is a portable, pure C implementation of the Git core
// methods provided as a re-entrant linkable library with a solid API,
// allowing you to write native speed custom Git applications in any
// language which supports C bindings.
//
// This file is an example of using that API in a real, compilable C file.
// As the API is updated, this file will be updated to demonstrate the new
// functionality.
//
// If you're trying to write something in C using [libgit2][lg], you should
// also check out the generated [API documentation][ap]. We try to link to
// the relevant sections of the API docs in each section in this file.
//
// **libgit2** (for the most part) only implements the core plumbing
// functions, not really the higher level porcelain stuff. For a primer on
// Git Internals that you will need to know to work with Git at this level,
// check out [Chapter 9][pg] of the Pro Git book.
//
// [lg]: http://libgit2.github.com
// [ap]: http://libgit2.github.com/libgit2
// [pg]: http://progit.org/book/ch9-0.html

// ### Includes

// Including the `git2.h` header will include all the other libgit2 headers
// that you need.  It should be the only thing you need to include in order
// to compile properly and get all the libgit2 API.
#include <git2.h>
#include <stdio.h>

// Almost all libgit2 functions return 0 on success or negative on error.
// This is not production quality error checking, but should be sufficient
// as an example.
static void check_error(int error_code, const char *action)
{
	const git_error *error = giterr_last();
	if (!error_code)
		return;

	printf("Error %d %s - %s\n", error_code, action,
		   (error && error->message) ? error->message : "???");

	exit(1);
}

int main (int argc, char** argv)
{
  // Initialize the library, this will set up any global state which libgit2 needs
  // including threading and crypto
  git_libgit2_init();

  // ### Opening the Repository

  // There are a couple of methods for opening a repository, this being the
  // simplest.  There are also [methods][me] for specifying the index file
  // and work tree locations, here we assume they are in the normal places.
	//
	// (Try running this program against tests/resources/testrepo.git.)
  //
  // [me]: http://libgit2.github.com/libgit2/#HEAD/group/repository
  int error;
  const char *repo_path = (argc > 1) ? argv[1] : "/opt/libgit2-test/.git";
  git_repository *repo;

  error = git_repository_open(&repo, repo_path);
  check_error(error, "opening repository");

  // ### SHA-1 Value Conversions

  // For our first example, we will convert a 40 character hex value to the
  // 20 byte raw SHA1 value.
  printf("*Hex to Raw*\n");
  char hex[] = "4a202b346bb0fb0db7eff3cffeb3c70babbd2045";

  // The `git_oid` is the structure that keeps the SHA value. We will use
  // this throughout the example for storing the value of the current SHA
  // key we're working with.
  git_oid oid;
  git_oid_fromstr(&oid, hex);

  // Once we've converted the string into the oid value, we can get the raw
  // value of the SHA by accessing `oid.id`

  // Next we will convert the 20 byte raw SHA1 value to a human readable 40
  // char hex value.
  printf("\n*Raw to Hex*\n");
  char out[GIT_OID_HEXSZ+1];
  out[GIT_OID_HEXSZ] = '\0';

  // If you have a oid, you can easily get the hex value of the SHA as well.
  git_oid_fmt(out, &oid);
  printf("SHA hex string: %s\n", out);

  // ### Working with the Object Database

  // **libgit2** provides [direct access][odb] to the object database.  The
  // object database is where the actual objects are stored in Git. For
  // working with raw objects, we'll need to get this structure from the
  // repository.
  //
  // [odb]: http://libgit2.github.com/libgit2/#HEAD/group/odb
  git_odb *odb;
  git_repository_odb(&odb, repo);

  // #### Raw Object Reading

  printf("\n*Raw Object Read*\n");
  git_odb_object *obj;
  git_otype otype;
  const unsigned char *data;
  const char *str_type;

  // We can read raw objects directly from the object database if we have
  // the oid (SHA) of the object.  This allows us to access objects without
  // knowing their type and inspect the raw bytes unparsed.
  error = git_odb_read(&obj, odb, &oid);
  check_error(error, "finding object in repository");

  // A raw object only has three properties - the type (commit, blob, tree
  // or tag), the size of the raw data and the raw, unparsed data itself.
  // For a commit or tag, that raw data is human readable plain ASCII
  // text. For a blob it is just file contents, so it could be text or
  // binary data. For a tree it is a special binary format, so it's unlikely
  // to be hugely helpful as a raw object.
  data = (const unsigned char *)git_odb_object_data(obj);
  otype = git_odb_object_type(obj);

  // We provide methods to convert from the object type which is an enum, to
  // a string representation of that value (and vice-versa).
  str_type = git_object_type2string(otype);
  printf("object length and type: %d, %s\n",
      (int)git_odb_object_size(obj),
      str_type);

  // For proper memory management, close the object when you are done with
  // it or it will leak memory.
  git_odb_object_free(obj);

  // #### Raw Object Writing

  printf("\n*Raw Object Write*\n");

  // You can also write raw object data to Git. This is pretty cool because
  // it gives you direct access to the key/value properties of Git.  Here
  // we'll write a new blob object that just contains a simple string.
  // Notice that we have to specify the object type as the `git_otype` enum.
  git_odb_write(&oid, odb, "test data", sizeof("test data") - 1, GIT_OBJ_BLOB);

  // Now that we've written the object, we can check out what SHA1 was
  // generated when the object was written to our database.
  git_oid_fmt(out, &oid);
  printf("Written Object: %s\n", out);

  // ### Object Parsing

  // libgit2 has methods to parse every object type in Git so you don't have
  // to work directly with the raw data. This is much faster and simpler
  // than trying to deal with the raw data yourself.

  // #### Commit Parsing

  // [Parsing commit objects][pco] is simple and gives you access to all the
  // data in the commit - the author (name, email, datetime), committer
  // (same), tree, message, encoding and parent(s).
  //
  // [pco]: http://libgit2.github.com/libgit2/#HEAD/group/commit

  printf("\n*Commit Parsing*\n");

  git_commit *commit;
  git_oid_fromstr(&oid, "8496071c1b46c854b31185ea97743be6a8774479");

  error = git_commit_lookup(&commit, repo, &oid);
  check_error(error, "looking up commit");

  const git_signature *author, *cmtter;
  const char *message;
  time_t ctime;
  unsigned int parents, p;

  // Each of the properties of the commit object are accessible via methods,
  // including commonly needed variations, such as `git_commit_time` which
  // returns the author time and `git_commit_message` which gives you the
  // commit message (as a NUL-terminated string).
  message  = git_commit_message(commit);
  author   = git_commit_author(commit);
  cmtter   = git_commit_committer(commit);
  ctime    = git_commit_time(commit);

  // The author and committer methods return [git_signature] structures,
  // which give you name, email and `when`, which is a `git_time` structure,
  // giving you a timestamp and timezone offset.
  printf("Author: %s (%s)\n", author->name, author->email);

  // Commits can have zero or more parents. The first (root) commit will
  // have no parents, most commits will have one (i.e. the commit it was
  // based on) and merge commits will have two or more.  Commits can
  // technically have any number, though it's rare to have more than two.
  parents  = git_commit_parentcount(commit);
  for (p = 0;p < parents;p++) {
    git_commit *parent;
    git_commit_parent(&parent, commit, p);
    git_oid_fmt(out, git_commit_id(parent));
    printf("Parent: %s\n", out);
    git_commit_free(parent);
  }

  // Don't forget to close the object to prevent memory leaks. You will have
  // to do this for all the objects you open and parse.
  git_commit_free(commit);

  // #### Writing Commits

  // libgit2 provides a couple of methods to create commit objects easily as
  // well. There are four different create signatures, we'll just show one
  // of them here.  You can read about the other ones in the [commit API
  // docs][cd].
  //
  // [cd]: http://libgit2.github.com/libgit2/#HEAD/group/commit

  printf("\n*Commit Writing*\n");
  git_oid tree_id, parent_id, commit_id;
  git_tree *tree;
  git_commit *parent;

  // Creating signatures for an authoring identity and time is simple.  You
  // will need to do this to specify who created a commit and when.  Default
  // values for the name and email should be found in the `user.name` and
  // `user.email` configuration options.  See the `config` section of this
  // example file to see how to access config values.
  git_signature_new((git_signature **)&author,
      "Scott Chacon", "schacon@gmail.com", 123456789, 60);
  git_signature_new((git_signature **)&cmtter,
      "Scott A Chacon", "scott@github.com", 987654321, 90);

  // Commit objects need a tree to point to and optionally one or more
  // parents.  Here we're creating oid objects to create the commit with,
  // but you can also use
  git_oid_fromstr(&tree_id, "f60079018b664e4e79329a7ef9559c8d9e0378d1");
  git_tree_lookup(&tree, repo, &tree_id);
  git_oid_fromstr(&parent_id, "5b5b025afb0b4c913b4c338a42934a3863bf3644");
  git_commit_lookup(&parent, repo, &parent_id);

  // Here we actually create the commit object with a single call with all
  // the values we need to create the commit.  The SHA key is written to the
  // `commit_id` variable here.
  git_commit_create_v(
    &commit_id, /* out id */
    repo,
    NULL, /* do not update the HEAD */
    author,
    cmtter,
    NULL, /* use default message encoding */
    "example commit",
    tree,
    1, parent);

  // Now we can take a look at the commit SHA we've generated.
  git_oid_fmt(out, &commit_id);
  printf("New Commit: %s\n", out);

  // #### Tag Parsing

  // You can parse and create tags with the [tag management API][tm], which
  // functions very similarly to the commit lookup, parsing and creation
  // methods, since the objects themselves are very similar.
  //
  // [tm]: http://libgit2.github.com/libgit2/#HEAD/group/tag
  printf("\n*Tag Parsing*\n");
  git_tag *tag;
  const char *tmessage, *tname;
  git_otype ttype;

  // We create an oid for the tag object if we know the SHA and look it up
  // the same way that we would a commit (or any other object).
  git_oid_fromstr(&oid, "b25fa35b38051e4ae45d4222e795f9df2e43f1d1");

  error = git_tag_lookup(&tag, repo, &oid);
  check_error(error, "looking up tag");

  // Now that we have the tag object, we can extract the information it
  // generally contains: the target (usually a commit object), the type of
  // the target object (usually 'commit'), the name ('v1.0'), the tagger (a
  // git_signature - name, email, timestamp), and the tag message.
  git_tag_target((git_object **)&commit, tag);
  tname = git_tag_name(tag);		// "test"
  ttype = git_tag_target_type(tag);	// GIT_OBJ_COMMIT (otype enum)
  tmessage = git_tag_message(tag);	// "tag message\n"
  printf("Tag Message: %s\n", tmessage);

  git_commit_free(commit);

  // #### Tree Parsing

  // [Tree parsing][tp] is a bit different than the other objects, in that
  // we have a subtype which is the tree entry.  This is not an actual
  // object type in Git, but a useful structure for parsing and traversing
  // tree entries.
  //
  // [tp]: http://libgit2.github.com/libgit2/#HEAD/group/tree
  printf("\n*Tree Parsing*\n");

  const git_tree_entry *entry;
  git_object *objt;

  // Create the oid and lookup the tree object just like the other objects.
  git_oid_fromstr(&oid, "2a741c18ac5ff082a7caaec6e74db3075a1906b5");
  git_tree_lookup(&tree, repo, &oid);

  // Getting the count of entries in the tree so you can iterate over them
  // if you want to.
  size_t cnt = git_tree_entrycount(tree); // 3
  printf("tree entries: %d\n", (int)cnt);

  entry = git_tree_entry_byindex(tree, 0);
  printf("Entry name: %s\n", git_tree_entry_name(entry)); // "hello.c"

  // You can also access tree entries by name if you know the name of the
  // entry you're looking for.
  entry = git_tree_entry_byname(tree, "README");
  git_tree_entry_name(entry); // "hello.c"

  // Once you have the entry object, you can access the content or subtree
  // (or commit, in the case of submodules) that it points to.  You can also
  // get the mode if you want.
  git_tree_entry_to_object(&objt, repo, entry); // blob

  // Remember to close the looked-up object once you are done using it
  git_object_free(objt);

  // #### Blob Parsing

  // The last object type is the simplest and requires the least parsing
  // help. Blobs are just file contents and can contain anything, there is
  // no structure to it. The main advantage to using the [simple blob
  // api][ba] is that when you're creating blobs you don't have to calculate
  // the size of the content.  There is also a helper for reading a file
  // from disk and writing it to the db and getting the oid back so you
  // don't have to do all those steps yourself.
  //
  // [ba]: http://libgit2.github.com/libgit2/#HEAD/group/blob

  printf("\n*Blob Parsing*\n");
  git_blob *blob;

  git_oid_fromstr(&oid, "1385f264afb75a56a5bec74243be9b367ba4ca08");
  git_blob_lookup(&blob, repo, &oid);

  // You can access a buffer with the raw contents of the blob directly.
  // Note that this buffer may not be contain ASCII data for certain blobs
  // (e.g. binary files): do not consider the buffer a NULL-terminated
  // string, and use the `git_blob_rawsize` attribute to find out its exact
  // size in bytes
  printf("Blob Size: %ld\n", (long)git_blob_rawsize(blob)); // 8
  git_blob_rawcontent(blob); // "content"

  // ### Revwalking

  // The libgit2 [revision walking api][rw] provides methods to traverse the
  // directed graph created by the parent pointers of the commit objects.
  // Since all commits point back to the commit that came directly before
  // them, you can walk this parentage as a graph and find all the commits
  // that were ancestors of (reachable from) a given starting point.  This
  // can allow you to create `git log` type functionality.
  //
  // [rw]: http://libgit2.github.com/libgit2/#HEAD/group/revwalk

  printf("\n*Revwalking*\n");
  git_revwalk *walk;
  git_commit *wcommit;

  git_oid_fromstr(&oid, "5b5b025afb0b4c913b4c338a42934a3863bf3644");

  // To use the revwalker, create a new walker, tell it how you want to sort
  // the output and then push one or more starting points onto the walker.
  // If you want to emulate the output of `git log` you would push the SHA
  // of the commit that HEAD points to into the walker and then start
  // traversing them.  You can also 'hide' commits that you want to stop at
  // or not see any of their ancestors.  So if you want to emulate `git log
  // branch1..branch2`, you would push the oid of `branch2` and hide the oid
  // of `branch1`.
  git_revwalk_new(&walk, repo);
  git_revwalk_sorting(walk, GIT_SORT_TOPOLOGICAL | GIT_SORT_REVERSE);
  git_revwalk_push(walk, &oid);

  const git_signature *cauth;
  const char *cmsg;

  // Now that we have the starting point pushed onto the walker, we start
  // asking for ancestors. It will return them in the sorting order we asked
  // for as commit oids.  We can then lookup and parse the committed pointed
  // at by the returned OID; note that this operation is specially fast
  // since the raw contents of the commit object will be cached in memory
  while ((git_revwalk_next(&oid, walk)) == 0) {
    error = git_commit_lookup(&wcommit, repo, &oid);
	check_error(error, "looking up commit during revwalk");

    cmsg  = git_commit_message(wcommit);
    cauth = git_commit_author(wcommit);
    printf("%s (%s)\n", cmsg, cauth->email);

    git_commit_free(wcommit);
  }

  // Like the other objects, be sure to free the revwalker when you're done
  // to prevent memory leaks.  Also, make sure that the repository being
  // walked it not deallocated while the walk is in progress, or it will
  // result in undefined behavior
  git_revwalk_free(walk);

  // ### Index File Manipulation

  // The [index file API][gi] allows you to read, traverse, update and write
  // the Git index file (sometimes thought of as the staging area).
  //
  // [gi]: http://libgit2.github.com/libgit2/#HEAD/group/index

  printf("\n*Index Walking*\n");

  git_index *index;
  unsigned int i, ecount;

  // You can either open the index from the standard location in an open
  // repository, as we're doing here, or you can open and manipulate any
  // index file with `git_index_open_bare()`. The index for the repository
  // will be located and loaded from disk.
  git_repository_index(&index, repo);

  // For each entry in the index, you can get a bunch of information
  // including the SHA (oid), path and mode which map to the tree objects
  // that are written out.  It also has filesystem properties to help
  // determine what to inspect for changes (ctime, mtime, dev, ino, uid,
  // gid, file_size and flags) All these properties are exported publicly in
  // the `git_index_entry` struct
  ecount = git_index_entrycount(index);
  for (i = 0; i < ecount; ++i) {
    const git_index_entry *e = git_index_get_byindex(index, i);

    printf("path: %s\n", e->path);
    printf("mtime: %d\n", (int)e->mtime.seconds);
    printf("fs: %d\n", (int)e->file_size);
  }

  git_index_free(index);

  // ### References

  // The [reference API][ref] allows you to list, resolve, create and update
  // references such as branches, tags and remote references (everything in
  // the .git/refs directory).
  //
  // [ref]: http://libgit2.github.com/libgit2/#HEAD/group/reference

  printf("\n*Reference Listing*\n");

  // Here we will implement something like `git for-each-ref` simply listing
  // out all available references and the object SHA they resolve to.
  git_strarray ref_list;
  git_reference_list(&ref_list, repo);

  const char *refname;
  git_reference *ref;

  // Now that we have the list of reference names, we can lookup each ref
  // one at a time and resolve them to the SHA, then print both values out.
  for (i = 0; i < ref_list.count; ++i) {
    refname = ref_list.strings[i];
    git_reference_lookup(&ref, repo, refname);

    switch (git_reference_type(ref)) {
    case GIT_REF_OID:
      git_oid_fmt(out, git_reference_target(ref));
      printf("%s [%s]\n", refname, out);
      break;

    case GIT_REF_SYMBOLIC:
      printf("%s => %s\n", refname, git_reference_symbolic_target(ref));
      break;
    default:
      fprintf(stderr, "Unexpected reference type\n");
      exit(1);
    }
  }

  git_strarray_free(&ref_list);

  // ### Config Files

  // The [config API][config] allows you to list and updatee config values
  // in any of the accessible config file locations (system, global, local).
  //
  // [config]: http://libgit2.github.com/libgit2/#HEAD/group/config

  printf("\n*Config Listing*\n");

  const char *email;
  int32_t j;

  git_config *cfg;

  // Open a config object so we can read global values from it.
  char config_path[256];
  sprintf(config_path, "%s/config", repo_path);
  check_error(git_config_open_ondisk(&cfg, config_path), "opening config");

  git_config_get_int32(&j, cfg, "help.autocorrect");
  printf("Autocorrect: %d\n", j);

  git_config_get_string(&email, cfg, "user.email");
  printf("Email: %s\n", email);

  // Finally, when you're done with the repository, you can free it as well.
  git_repository_free(repo);

  return 0;
}

