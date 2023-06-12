# libgit2 Coding Style

This documentation describes the preferred coding style for the libgit2 project.
While not all parts of our code base conform to this coding style, the outlined
rules are what we aim for.

Note that in no case do we accept changes that convert huge parts of the code
base to use our coding style. Instead, it is encouraged to modernize small parts
of code you're going to modify anyway for a given change you want to introduce.
A good rule to follow is the Boy Scout Rule: "Leave the campground cleaner than
you found it."

## C Coding Style

The following sections define the coding style for all code files and headers.

### Indentation and Alignment

Code is indented by tabs, where a tab is 8 spaces. Each opening scope increases
the indentation level.

```c
int foobar(int void)
{
	if (condition)
		doit();
	/* Body */
}
```

Switch statements have their `case`s aligned with the `switch` keyword. Case
bodies are indented by an additional level. Case bodies should not open their
own scope to declare variables.

```c
switch (c) {
case 'a':
case 'b':
	return 0;
default:
	return -1;
}
```

Multi-line conditions should be aligned with the opening brace of the current
statement:

```c
if (one_very_long_condition(c) &&
    another_very_long_condition(c))
	doit();
```

### Spaces

There must be no space between the function and its arguments, arguments must be
separated by a space:

```c
int doit(int first_arg, int second_arg);
doit(1, 2);
```

For any binary or ternary operators, the arguments and separator must be
separated by a space:

```c
1 + 2;
x ? x : NULL;
```

Unary operators do not have a space between them and the argument they refer to:

```c
*c
&c
```

The `sizeof` operator always must not have a space and must use braces around
the type:

```
sizeof(int)
```

There must be a space after the keywords `if`, `switch`, `case`, `do` and
`while`.

### Braces

Functions must have their opening brace on the following line:

```c
void foobar(void)
{
	doit();
}
```

For conditions, braces should be placed on the same line as the condition:

```c
if (condition(c)) {
	doit();
	dothat();
}

while (true) {
	doit();
}
```

In case a condition's body has a single line, only, it's allowed to omit braces,
except if any of its `else if` or `else` branches has more than one line:

```c
if (condition(c))
	doit();

if (condition(c))
	doit();
else if (other_condition(c))
	doit();

/* This example must use braces as the `else if` requires them. */
if (condition(c)) {
	doit();
} else if (other_condition(c)) {
	doit();
	dothat();
} else {
	abort();
}
```

### Comments

Comments must use C-style `/* */` comments. C++-style `// `comments are not
allowed in our codebase. This is a strict requirement as libgit2 tries to be
compliant with the ISO C90 standard, which only allows C-style comments.

Single-line comments may have their opening and closing tag on the same line:

```c
/* This is a short comment. */
```

For multi-line comments, the opening and closing tag should be empty:

```c
/*
 * This is a rather long and potentially really unwiedly but informative
 * multiline comment that helps quite a lot.
 */
```

Public functions must have documentation that explain their usage, internal
functions should have a comment. We use Docurium to generate documentation
derived from these comments, which uses syntax similar to Doxygen. The first
line should be a short summary of what the function does. More in-depth
explanation should be separated from that first line by an empty line.
Parameters and return values should be documented via `@return` and `@param`
tags:

```c
/*
 * Froznicate the string.
 *
 * Froznicate the string by foobaring its internal structure into a more obvious
 * translation. Note that the returned string is a newly allocated string that
 * shall be `free`d by the caller.
 *
 * @param s String to froznicate
 * @return A newly allocated string or `NULL` in case an error occurred.
 */
char *froznicate(const char *s);
```

### Variables

Variables must be declared at the beginning of their scope. This is a strict
requirement as libgit2 tries to be compliant with the ISO C90 standard, which
forbids mixed declarations and code:

```c
void foobar(void)
{
	char *c = NULL;
	int a, b;

	a = 0;
	b = 1;

	return c;
}
```

### Naming

Variables must have all-lowercase names. In case a variable name has multiple
words, words should be separated by an underscore `_` character. While
recommended to use descriptive naming, common variable names like `i` for
indices are allowed.

All public functions must have a `git` prefix as well as a prefix indicating
their respective subsystem. E.g. a function that opens a repository should be
called `git_repository_open()`. Functions that are not public but declared in
an internal header file for use by other subsystems should follow the same
naming pattern. File-local static functions must not have a `git` prefix, but
should have a prefix indicating their respective subsystem.

All structures declared in the libgit2 project must have a `typedef`, we do not
use `struct type` variables. Type names follow the same schema as functions.

### Error Handling

The libgit2 project mostly uses error codes to indicate errors. Error codes are
always of type `int`, where `0` indicates success and a negative error code
indicates an error case. In some cases, positive error codes may be used to
indicate special cases. Returned values that are not an error code should be
returned via an out parameter. Out parameters must always come first in the list
of arguments.

```c
int doit(const char **out, int arg)
{
	if (!arg)
		return -1;
	*out = "Got an argument";
	return 0;
}
```

To avoid repetitive and fragile error handling in case a function has resources
that need to be free'd, we use `goto out`s:

```c
int doit(char **out, int arg)
{
	int error = 0;
	char *c;

	c = malloc(strlen("Got an argument") + 1);
	if (!c) {
		error = -1;
		goto out;
	}

	if (!arg) {
		error = -1;
		goto out;
	}

	strcpy(c, "Got an argument")
	*out = c;

out:
	if (error)
		free(c);
	return error;
}
```

When calling functions that return an error code, you should assign the error
code to an `error` variable and, in case an error case is indicated and no
custom error handling is required, return that error code:

```c
int foobar(void)
{
	int error;

	if ((error = doit()) < 0)
		return error;

	return 0;
}
```

When doing multiple function calls where all of the functions return an error
code, it's common practice to chain these calls together:

```c
int doit(void)
{
	int error;

	if ((error = dothis()) < 0 ||
	    (error = dothat()) < 0)
		return error;

	return 0;
}
```

## CMake Coding Style

The following section defines the coding style for our CMake build system.

### Indentation

Code is indented by tabs, where a tab is 8 spaces. Each opening scope increases
the indentation level.

```cmake
if(CONDITION)
	doit()
endif()
```

### Spaces

There must be no space between keywords and their opening brace. While this is
the same as in our C codebase for function calls, this also applies to
conditional keywords. This is done to avoid the awkward-looking `else ()`
statement.

```cmake
if(CONDITION)
	doit()
else()
	dothat()
endif()
```

### Case

While CMake is completely case-insensitive when it comes to function calls, we
want to agree on a common coding style for this. To reduce the danger of
repetitive strain injuries, all function calls should be lower-case (NB: this is
not currently the case yet, but introduced as a new coding style by this
document).

Variables are written all-uppercase. In contrast to functions, variables are
case-sensitive in CMake. As CMake itself uses upper-case variables in all
places, we should follow suit and do the same.

Control flow keywords must be all lowercase. In contrast to that, test keywords
must be all uppercase:

```cmake
if(NOT CONDITION)
        doit()
elseif(FOO AND BAR)
        dothat()
endif()
```

### Targets

CMake code should not use functions that modify the global scope but prefer
their targeted equivalents, instead. E.g. instead of using
`include_directories()`, you must use `target_include_directories()`. An
exception to this rule is setting up global compiler flags like warnings or
flags required to set up the build type.

### Dependencies

Dependencies should not be discovered or set up in the main "CMakeLists.txt"
module. Instead, they should either have their own module in our top-level
"cmake/" directory or have a "CMakeLists.txt" in their respective "deps/"
directory in case it is a vendored library. All dependencies should expose
interface library targets that can be linked against with
`target_link_libraries()`.
