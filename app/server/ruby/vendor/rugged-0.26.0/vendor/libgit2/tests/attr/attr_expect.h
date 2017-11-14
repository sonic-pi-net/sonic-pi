#ifndef __CLAR_TEST_ATTR_EXPECT__
#define __CLAR_TEST_ATTR_EXPECT__

enum attr_expect_t {
	EXPECT_FALSE,
	EXPECT_TRUE,
	EXPECT_UNDEFINED,
	EXPECT_STRING
};

struct attr_expected {
	const char *path;
	const char *attr;
	enum attr_expect_t expected;
	const char *expected_str;
};

GIT_INLINE(void) attr_check_expected(
	enum attr_expect_t expected,
	const char *expected_str,
	const char *name,
	const char *value)
{
	switch (expected) {
	case EXPECT_TRUE:
		cl_assert_(GIT_ATTR_TRUE(value), name);
		break;

	case EXPECT_FALSE:
		cl_assert_(GIT_ATTR_FALSE(value), name);
		break;

	case EXPECT_UNDEFINED:
		cl_assert_(GIT_ATTR_UNSPECIFIED(value), name);
		break;

	case EXPECT_STRING:
		cl_assert_equal_s(expected_str, value);
		break;
	}
}

#endif
