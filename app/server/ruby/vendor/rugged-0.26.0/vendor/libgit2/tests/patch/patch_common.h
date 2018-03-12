/* The original file contents */

#define FILE_ORIGINAL \
	"hey!\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"(this line is changed)\n" \
	"and this\n" \
	"is additional context\n" \
	"below it!\n"

/* A change in the middle of the file (and the resultant patch) */

#define FILE_CHANGE_MIDDLE \
	"hey!\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"(THIS line is changed!)\n" \
	"and this\n" \
	"is additional context\n" \
	"below it!\n"

#define PATCH_ORIGINAL_TO_CHANGE_MIDDLE \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -3,7 +3,7 @@ this is some context!\n" \
	" around some lines\n" \
	" that will change\n" \
	" yes it is!\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n"

#define PATCH_ORIGINAL_TO_CHANGE_MIDDLE_NOCONTEXT \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -6 +6 @@ yes it is!\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n"

/* A change of the first line (and the resultant patch) */

#define FILE_CHANGE_FIRSTLINE \
	"hey, change in head!\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"(this line is changed)\n" \
	"and this\n" \
	"is additional context\n" \
	"below it!\n"

#define PATCH_ORIGINAL_TO_CHANGE_FIRSTLINE \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..c81df1d 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -1,4 +1,4 @@\n" \
	"-hey!\n" \
	"+hey, change in head!\n" \
	" this is some context!\n" \
	" around some lines\n" \
	" that will change\n"

/* A change of the last line (and the resultant patch) */

#define FILE_CHANGE_LASTLINE \
	"hey!\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"(this line is changed)\n" \
	"and this\n" \
	"is additional context\n" \
	"change to the last line.\n"

#define PATCH_ORIGINAL_TO_CHANGE_LASTLINE \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..f70db1c 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -6,4 +6,4 @@ yes it is!\n" \
	" (this line is changed)\n" \
	" and this\n" \
	" is additional context\n" \
	"-below it!\n" \
	"+change to the last line.\n"

/* A change of the middle where we remove many lines */

#define FILE_CHANGE_MIDDLE_SHRINK \
	"hey!\n" \
	"i've changed a lot, but left the line\n" \
	"below it!\n"

#define PATCH_ORIGINAL_TO_CHANGE_MIDDLE_SHRINK \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..629cd35 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -1,9 +1,3 @@\n" \
	" hey!\n" \
	"-this is some context!\n" \
	"-around some lines\n" \
	"-that will change\n" \
	"-yes it is!\n" \
	"-(this line is changed)\n" \
	"-and this\n" \
	"-is additional context\n" \
	"+i've changed a lot, but left the line\n" \
	" below it!\n"

#define PATCH_ORIGINAL_TO_MIDDLE_SHRINK_NOCONTEXT \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..629cd35 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -2,7 +2 @@ hey!\n" \
	"-this is some context!\n" \
	"-around some lines\n" \
	"-that will change\n" \
	"-yes it is!\n" \
	"-(this line is changed)\n" \
	"-and this\n" \
	"-is additional context\n" \
	"+i've changed a lot, but left the line\n"

/* A change to the middle where we grow many lines */

#define FILE_CHANGE_MIDDLE_GROW \
	"hey!\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"this line is changed\n" \
	"and this line is added\n" \
	"so is this\n" \
	"(this too)\n" \
	"whee...\n" \
	"and this\n" \
	"is additional context\n" \
	"below it!\n"

#define PATCH_ORIGINAL_TO_CHANGE_MIDDLE_GROW \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..207ebca 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -3,7 +3,11 @@ this is some context!\n" \
	" around some lines\n" \
	" that will change\n" \
	" yes it is!\n" \
	"-(this line is changed)\n" \
	"+this line is changed\n" \
	"+and this line is added\n" \
	"+so is this\n" \
	"+(this too)\n" \
	"+whee...\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n"


#define PATCH_ORIGINAL_TO_MIDDLE_GROW_NOCONTEXT \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..207ebca 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -6 +6,5 @@ yes it is!\n" \
	"-(this line is changed)\n" \
	"+this line is changed\n" \
	"+and this line is added\n" \
	"+so is this\n" \
	"+(this too)\n" \
	"+whee...\n"

/* An insertion at the beginning of the file (and the resultant patch) */

#define FILE_PREPEND \
	"insert at front\n" \
	"hey!\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"(this line is changed)\n" \
	"and this\n" \
	"is additional context\n" \
	"below it!\n"

#define PATCH_ORIGINAL_TO_PREPEND \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..0f39b9a 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -1,3 +1,4 @@\n" \
	"+insert at front\n" \
	" hey!\n" \
	" this is some context!\n" \
	" around some lines\n"

#define PATCH_ORIGINAL_TO_PREPEND_NOCONTEXT \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..0f39b9a 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -0,0 +1 @@\n" \
	"+insert at front\n"

/* An insertion at the end of the file (and the resultant patch) */

#define FILE_APPEND \
	"hey!\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"(this line is changed)\n" \
	"and this\n" \
	"is additional context\n" \
	"below it!\n" \
	"insert at end\n"

#define PATCH_ORIGINAL_TO_APPEND \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..72788bb 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -7,3 +7,4 @@ yes it is!\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n" \
	"+insert at end\n"

#define PATCH_ORIGINAL_TO_APPEND_NOCONTEXT \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..72788bb 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -9,0 +10 @@ below it!\n" \
	"+insert at end\n"

#define PATCH_SIMPLE_COMMIT \
	"commit 15e119375018fba121cf58e02a9f17fe22df0df8\n" \
	"Author: Edward Thomson <ethomson@edwardthomson.com>\n" \
	"Date:   Wed Jun 14 13:31:20 2017 +0200\n" \
	"\n" \
	"    CHANGELOG: document git_filter_init and GIT_FILTER_INIT\n" \
	"\n" \
	"diff --git a/CHANGELOG.md b/CHANGELOG.md\n" \
	"index 1b9e0c90a..24ecba426 100644\n" \
	"--- a/CHANGELOG.md\n" \
	"+++ b/CHANGELOG.md\n" \
	"@@ -96,6 +96,9 @@ v0.26\n" \
	" * `git_transport_smart_proxy_options()' enables you to get the proxy options for\n" \
	"   smart transports.\n" \
	"\n" \
	"+* The `GIT_FILTER_INIT` macro and the `git_filter_init` function are provided\n" \
	"+  to initialize a `git_filter` structure.\n" \
	"+\n" \
	" ### Breaking API changes\n" \
	"\n" \
	" * `clone_checkout_strategy` has been removed from\n"

#define PATCH_MULTIPLE_HUNKS \
	"diff --git a/x b/x\n" \
	"index 0719398..fa0350c 100644\n" \
	"--- a/x\n" \
	"+++ b/x\n" \
	"@@ -1,5 +1,4 @@\n" \
	" 1\n" \
	"-2\n" \
	" 3\n" \
	" 4\n" \
	" 5\n" \
	"@@ -7,3 +6,4 @@\n" \
	" 7\n" \
	" 8\n" \
	" 9\n" \
	"+10\n"

#define PATCH_MULTIPLE_FILES \
	"diff --git a/x b/x\n" \
	"index 8a1218a..7059ba5 100644\n" \
	"--- a/x\n" \
	"+++ b/x\n" \
	"@@ -1,5 +1,4 @@\n" \
	" 1\n" \
	" 2\n" \
	"-3\n" \
	" 4\n" \
	" 5\n" \
	"diff --git a/y b/y\n" \
	"index e006065..9405325 100644\n" \
	"--- a/y\n" \
	"+++ b/y\n" \
	"@@ -1,4 +1,5 @@\n" \
	" a\n" \
	" b\n" \
	"+c\n" \
	" d\n" \
	" e\n"

#define FILE_PREPEND_AND_APPEND \
	"first and\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"(this line is changed)\n" \
	"and this\n" \
	"is additional context\n" \
	"last lines\n"

#define PATCH_ORIGINAL_TO_PREPEND_AND_APPEND \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..f282430 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -1,4 +1,4 @@\n" \
	"-hey!\n" \
	"+first and\n" \
	" this is some context!\n" \
	" around some lines\n" \
	" that will change\n" \
	"@@ -6,4 +6,4 @@ yes it is!\n" \
	" (this line is changed)\n" \
	" and this\n" \
	" is additional context\n" \
	"-below it!\n" \
	"+last lines\n"

#define PATCH_ORIGINAL_TO_EMPTY_FILE \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..e69de29 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -1,9 +0,0 @@\n" \
	"-hey!\n" \
	"-this is some context!\n" \
	"-around some lines\n" \
	"-that will change\n" \
	"-yes it is!\n" \
	"-(this line is changed)\n" \
	"-and this\n" \
	"-is additional context\n" \
	"-below it!\n"

#define PATCH_EMPTY_FILE_TO_ORIGINAL \
	"diff --git a/file.txt b/file.txt\n" \
	"index e69de29..9432026 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -0,0 +1,9 @@\n" \
	"+hey!\n" \
	"+this is some context!\n" \
	"+around some lines\n" \
	"+that will change\n" \
	"+yes it is!\n" \
	"+(this line is changed)\n" \
	"+and this\n" \
	"+is additional context\n" \
	"+below it!\n"

#define PATCH_ADD_ORIGINAL \
	"diff --git a/file.txt b/file.txt\n" \
	"new file mode 100644\n" \
	"index 0000000..9432026\n" \
	"--- /dev/null\n" \
	"+++ b/file.txt\n" \
	"@@ -0,0 +1,9 @@\n" \
	"+hey!\n" \
	"+this is some context!\n" \
	"+around some lines\n" \
	"+that will change\n" \
	"+yes it is!\n" \
	"+(this line is changed)\n" \
	"+and this\n" \
	"+is additional context\n" \
	"+below it!\n"

#define PATCH_DELETE_ORIGINAL \
	"diff --git a/file.txt b/file.txt\n" \
	"deleted file mode 100644\n" \
	"index 9432026..0000000\n" \
	"--- a/file.txt\n" \
	"+++ /dev/null\n" \
	"@@ -1,9 +0,0 @@\n" \
	"-hey!\n" \
	"-this is some context!\n" \
	"-around some lines\n" \
	"-that will change\n" \
	"-yes it is!\n" \
	"-(this line is changed)\n" \
	"-and this\n" \
	"-is additional context\n" \
	"-below it!\n"

#define PATCH_RENAME_EXACT \
	"diff --git a/file.txt b/newfile.txt\n" \
	"similarity index 100%\n" \
	"rename from file.txt\n" \
	"rename to newfile.txt\n"

#define PATCH_RENAME_SIMILAR \
	"diff --git a/file.txt b/newfile.txt\n" \
	"similarity index 77%\n" \
	"rename from file.txt\n" \
	"rename to newfile.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/newfile.txt\n" \
	"@@ -3,7 +3,7 @@ this is some context!\n" \
	" around some lines\n" \
	" that will change\n" \
	" yes it is!\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n"

#define PATCH_RENAME_EXACT_QUOTEDNAME \
	"diff --git a/file.txt \"b/foo\\\"bar.txt\"\n" \
	"similarity index 100%\n" \
	"rename from file.txt\n" \
	"rename to \"foo\\\"bar.txt\"\n"

#define PATCH_RENAME_SIMILAR_QUOTEDNAME \
	"diff --git a/file.txt \"b/foo\\\"bar.txt\"\n" \
	"similarity index 77%\n" \
	"rename from file.txt\n" \
	"rename to \"foo\\\"bar.txt\"\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ \"b/foo\\\"bar.txt\"\n" \
	"@@ -3,7 +3,7 @@ this is some context!\n" \
	" around some lines\n" \
	" that will change\n" \
	" yes it is!\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n"

#define PATCH_MODECHANGE_UNCHANGED \
	"diff --git a/file.txt b/file.txt\n" \
	"old mode 100644\n" \
	"new mode 100755\n"

#define PATCH_MODECHANGE_MODIFIED \
	"diff --git a/file.txt b/file.txt\n" \
	"old mode 100644\n" \
	"new mode 100755\n" \
	"index 9432026..cd8fd12\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -3,7 +3,7 @@ this is some context!\n" \
	" around some lines\n" \
	" that will change\n" \
	" yes it is!\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n"

#define PATCH_NOISY \
	"This is some\nleading noise\n@@ - that\nlooks like a hunk header\n" \
	"but actually isn't and should parse ok\n" \
	PATCH_ORIGINAL_TO_CHANGE_MIDDLE \
	"plus some trailing garbage for good measure\n"

#define PATCH_NOISY_NOCONTEXT \
	"This is some\nleading noise\n@@ - that\nlooks like a hunk header\n" \
	"but actually isn't and should parse ok\n" \
	PATCH_ORIGINAL_TO_CHANGE_MIDDLE_NOCONTEXT \
	"plus some trailing garbage for good measure\n"

#define PATCH_TRUNCATED_1 \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -3,7 +3,7 @@ this is some context!\n" \
	" around some lines\n" \
	" that will change\n" \
	" yes it is!\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n" \
	" and this\n"

#define PATCH_TRUNCATED_2 \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -3,7 +3,7 @@ this is some context!\n" \
	" around some lines\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n"

#define PATCH_TRUNCATED_3 \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -3,7 +3,7 @@ this is some context!\n" \
	" around some lines\n" \
	" that will change\n" \
	" yes it is!\n" \
	"+(THIS line is changed!)\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n"

#define FILE_EMPTY_CONTEXT_ORIGINAL \
	"this\nhas\nan\n\nempty\ncontext\nline\n"

#define FILE_EMPTY_CONTEXT_MODIFIED \
	"this\nhas\nan\n\nempty...\ncontext\nline\n"

#define PATCH_EMPTY_CONTEXT \
	"diff --git a/file.txt b/file.txt\n" \
	"index 398d2df..bb15234 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -2,6 +2,6 @@ this\n" \
	" has\n" \
	" an\n" \
	"\n" \
	"-empty\n" \
	"+empty...\n" \
	" context\n" \
	" line\n"

#define FILE_APPEND_NO_NL \
	"hey!\n" \
	"this is some context!\n" \
	"around some lines\n" \
	"that will change\n" \
	"yes it is!\n" \
	"(this line is changed)\n" \
	"and this\n" \
	"is additional context\n" \
	"below it!\n" \
	"added line with no nl"

#define PATCH_APPEND_NO_NL \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..83759c0 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -7,3 +7,4 @@ yes it is!\n" \
	" and this\n" \
	" is additional context\n" \
	" below it!\n" \
	"+added line with no nl\n" \
	"\\ No newline at end of file\n"

#define PATCH_CORRUPT_GIT_HEADER \
	"diff --git a/file.txt\n" \
	"index 9432026..0f39b9a 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -0,0 +1 @@\n" \
	"+insert at front\n"

#define PATCH_CORRUPT_MISSING_NEW_FILE \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"@@ -6 +6 @@ yes it is!\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n"

#define PATCH_CORRUPT_MISSING_OLD_FILE \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"+++ b/file.txt\n" \
	"@@ -6 +6 @@ yes it is!\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n"

#define PATCH_CORRUPT_NO_CHANGES \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"@@ -0,0 +0,0 @@ yes it is!\n"

#define PATCH_CORRUPT_MISSING_HUNK_HEADER \
	"diff --git a/file.txt b/file.txt\n" \
	"index 9432026..cd8fd12 100644\n" \
	"--- a/file.txt\n" \
	"+++ b/file.txt\n" \
	"-(this line is changed)\n" \
	"+(THIS line is changed!)\n"

#define PATCH_NOT_A_PATCH \
	"+++this is not\n" \
	"--actually even\n" \
	" a legitimate \n" \
	"+patch file\n" \
	"-it's something else\n" \
	" entirely!"

/* binary contents */

#define FILE_BINARY_LITERAL_ORIGINAL "\x00\x00\x0a"
#define FILE_BINARY_LITERAL_ORIGINAL_LEN 3

#define FILE_BINARY_LITERAL_MODIFIED "\x00\x00\x01\x02\x0a"
#define FILE_BINARY_LITERAL_MODIFIED_LEN 5

#define PATCH_BINARY_LITERAL \
	"diff --git a/binary.bin b/binary.bin\n" \
	"index bd474b2519cc15eab801ff851cc7d50f0dee49a1..9ac35ff15cd8864aeafd889e4826a3150f0b06c4 100644\n" \
	"GIT binary patch\n" \
	"literal 5\n" \
	"Mc${NkU}WL~000&M4gdfE\n" \
	"\n" \
	"literal 3\n" \
	"Kc${Nk-~s>u4FC%O\n\n"

#define FILE_BINARY_DELTA_ORIGINAL \
	"\x00\x00\x01\x02\x00\x00\x01\x02\x00\x00\x01\x02\x0a\x54\x68\x69" \
	"\x73\x20\x69\x73\x20\x61\x20\x62\x69\x6e\x61\x72\x79\x20\x66\x69" \
	"\x6c\x65\x2c\x20\x62\x79\x20\x76\x69\x72\x74\x75\x65\x20\x6f\x66" \
	"\x20\x68\x61\x76\x69\x6e\x67\x20\x73\x6f\x6d\x65\x20\x6e\x75\x6c" \
	"\x6c\x73\x2e\x0a\x00\x00\x01\x02\x00\x00\x01\x02\x00\x00\x01\x02" \
	"\x0a\x57\x65\x27\x72\x65\x20\x67\x6f\x69\x6e\x67\x20\x74\x6f\x20" \
	"\x63\x68\x61\x6e\x67\x65\x20\x70\x6f\x72\x74\x69\x6f\x6e\x73\x20" \
	"\x6f\x66\x20\x69\x74\x2e\x0a\x00\x00\x01\x02\x00\x00\x01\x02\x00" \
	"\x00\x01\x02\x0a\x53\x6f\x20\x74\x68\x61\x74\x20\x77\x65\x20\x67" \
	"\x69\x74\x20\x61\x20\x62\x69\x6e\x61\x72\x79\x20\x64\x65\x6c\x74" \
	"\x61\x20\x69\x6e\x73\x74\x65\x61\x64\x20\x6f\x66\x20\x74\x68\x65" \
	"\x20\x64\x65\x66\x6c\x61\x74\x65\x64\x20\x63\x6f\x6e\x74\x65\x6e" \
	"\x74\x73\x2e\x0a\x00\x00\x01\x02\x00\x00\x01\x02\x00\x00\x01\x02" \
	"\x0a"
#define FILE_BINARY_DELTA_ORIGINAL_LEN 209

#define FILE_BINARY_DELTA_MODIFIED \
	"\x00\x00\x01\x02\x00\x00\x01\x02\x00\x00\x01\x02\x0a\x5a\x5a\x5a" \
	"\x5a\x20\x69\x73\x20\x61\x20\x62\x69\x6e\x61\x72\x79\x20\x66\x69" \
	"\x6c\x65\x2c\x20\x62\x79\x20\x76\x69\x72\x74\x75\x65\x20\x6f\x66" \
	"\x20\x68\x61\x76\x69\x6e\x67\x20\x73\x6f\x6d\x65\x20\x6e\x75\x6c" \
	"\x6c\x73\x2e\x0a\x00\x00\x01\x02\x00\x00\x01\x02\x00\x00\x01\x02" \
	"\x0a\x57\x65\x27\x72\x65\x20\x67\x6f\x69\x6e\x67\x20\x74\x6f\x20" \
	"\x63\x68\x61\x6e\x67\x65\x20\x70\x6f\x72\x74\x69\x6f\x6e\x73\x20" \
	"\x6f\x66\x20\x49\x54\x2e\x0a\x00\x00\x01\x02\x00\x00\x01\x02\x00" \
	"\x00\x01\x02\x0a\x53\x4f\x20\x74\x68\x61\x74\x20\x77\x65\x20\x67" \
	"\x69\x74\x20\x61\x20\x62\x69\x6e\x61\x72\x79\x20\x64\x65\x6c\x74" \
	"\x61\x20\x69\x6e\x73\x74\x65\x61\x64\x20\x6f\x66\x20\x74\x68\x65" \
	"\x20\x64\x65\x66\x6c\x61\x74\x65\x64\x20\x63\x6f\x6e\x74\x65\x6e" \
	"\x74\x73\x2e\x0a\x00\x00\x01\x02\x00\x00\x01\x02\x00\x00\x01\x02" \
	"\x0a"
#define FILE_BINARY_DELTA_MODIFIED_LEN 209

#define PATCH_BINARY_DELTA \
	"diff --git a/binary.bin b/binary.bin\n" \
	"index 27184d9883b12c4c9c54b4a31137603586169f51..7c94f9e60bf366033d98e0d551ae37d30faef74a 100644\n" \
	"GIT binary patch\n" \
	"delta 48\n" \
	"kc$~Y)c#%<%fq{_;hPk4EV4`4>uxE%K7m7r%|HL+L0In7XGynhq\n" \
	"\n" \
	"delta 48\n" \
	"mc$~Y)c#%<%fq{_;hPgsAGK(h)CJASj=y9P)1m{m|^9BI99|yz$\n\n"

#define PATCH_BINARY_ADD \
	"diff --git a/binary.bin b/binary.bin\n" \
	"new file mode 100644\n" \
	"index 0000000000000000000000000000000000000000..7c94f9e60bf366033d98e0d551ae37d30faef74a\n" \
	"GIT binary patch\n" \
	"literal 209\n" \
	"zc${60u?oUK5JXSQe8qG&;(u6KC<u0&+$Ohh?#kUJlD{_rLCL^0!@QXgcKh&k^H>C_\n" \
	"zAhe=XX7rNzh<3&##YcwqNHmEKsP<&&m~%Zf;eX@Khr$?aExDmfqyyt+#l^I)3+LMg\n" \
	"kxnAIj9Pfn_|Gh`fP7tlm6j#y{FJYg_IifRlR^R@A08f862mk;8\n" \
	"\n" \
	"literal 0\n" \
	"Hc$@<O00001\n\n"

#define PATCH_BINARY_DELETE \
	"diff --git a/binary.bin b/binary.bin\n" \
	"deleted file mode 100644\n" \
	"index 7c94f9e60bf366033d98e0d551ae37d30faef74a..0000000000000000000000000000000000000000\n" \
	"GIT binary patch\n" \
	"literal 0\n" \
	"Hc$@<O00001\n" \
	"\n" \
	"literal 209\n" \
	"zc${60u?oUK5JXSQe8qG&;(u6KC<u0&+$Ohh?#kUJlD{_rLCL^0!@QXgcKh&k^H>C_\n" \
	"zAhe=XX7rNzh<3&##YcwqNHmEKsP<&&m~%Zf;eX@Khr$?aExDmfqyyt+#l^I)3+LMg\n" \
	"kxnAIj9Pfn_|Gh`fP7tlm6j#y{FJYg_IifRlR^R@A08f862mk;8\n\n"

/* contains an old side that does not match the expected source */
#define PATCH_BINARY_NOT_REVERSIBLE \
	"diff --git a/binary.bin b/binary.bin\n" \
	"index 27184d9883b12c4c9c54b4a31137603586169f51..7c94f9e60bf366033d98e0d551ae37d30faef74a 100644\n" \
	"GIT binary patch\n" \
	"literal 5\n" \
	"Mc${NkU}WL~000&M4gdfE\n" \
	"\n" \
	"delta 48\n" \
	"mc$~Y)c#%<%fq{_;hPgsAGK(h)CJASj=y9P)1m{m|^9BI99|yz$\n\n"

#define PATCH_BINARY_NOT_PRINTED \
	"diff --git a/binary.bin b/binary.bin\n" \
	"index 27184d9..7c94f9e 100644\n" \
	"Binary files a/binary.bin and b/binary.bin differ\n"
