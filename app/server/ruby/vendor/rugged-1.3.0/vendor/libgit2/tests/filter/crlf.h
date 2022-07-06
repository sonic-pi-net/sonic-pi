#ifndef INCLUDE_filter_crlf_h__
#define INCLUDE_filter_crlf_h__

/*
 * file content for files in the resources/crlf repository
 */

#define UTF8_BOM "\xEF\xBB\xBF"

#define ALL_CRLF_TEXT_RAW		"crlf\r\ncrlf\r\ncrlf\r\ncrlf\r\n"
#define ALL_LF_TEXT_RAW			"lf\nlf\nlf\nlf\nlf\n"
#define MORE_CRLF_TEXT_RAW		"crlf\r\ncrlf\r\nlf\ncrlf\r\ncrlf\r\n"
#define MORE_LF_TEXT_RAW		"lf\nlf\ncrlf\r\nlf\nlf\n"

#define ALL_CRLF_TEXT_AS_CRLF	ALL_CRLF_TEXT_RAW
#define ALL_LF_TEXT_AS_CRLF		"lf\r\nlf\r\nlf\r\nlf\r\nlf\r\n"
#define MORE_CRLF_TEXT_AS_CRLF	"crlf\r\ncrlf\r\nlf\r\ncrlf\r\ncrlf\r\n"
#define MORE_LF_TEXT_AS_CRLF	"lf\r\nlf\r\ncrlf\r\nlf\r\nlf\r\n"

#define ALL_CRLF_TEXT_AS_LF		"crlf\ncrlf\ncrlf\ncrlf\n"
#define ALL_LF_TEXT_AS_LF		ALL_LF_TEXT_RAW
#define MORE_CRLF_TEXT_AS_LF	"crlf\ncrlf\nlf\ncrlf\ncrlf\n"
#define MORE_LF_TEXT_AS_LF		"lf\nlf\ncrlf\nlf\nlf\n"

#define FEW_UTF8_CRLF_RAW		"\xe2\x9a\xbdThe rest is ASCII01.\r\nThe rest is ASCII02.\r\nThe rest is ASCII03.\r\nThe rest is ASCII04.\r\nThe rest is ASCII05.\r\nThe rest is ASCII06.\r\nThe rest is ASCII07.\r\nThe rest is ASCII08.\r\nThe rest is ASCII09.\r\nThe rest is ASCII10.\r\nThe rest is ASCII11.\r\nThe rest is ASCII12.\r\nThe rest is ASCII13.\r\nThe rest is ASCII14.\r\nThe rest is ASCII15.\r\nThe rest is ASCII16.\r\nThe rest is ASCII17.\r\nThe rest is ASCII18.\r\nThe rest is ASCII19.\r\nThe rest is ASCII20.\r\nThe rest is ASCII21.\r\nThe rest is ASCII22.\r\n"
#define FEW_UTF8_LF_RAW			"\xe2\x9a\xbdThe rest is ASCII01.\nThe rest is ASCII02.\nThe rest is ASCII03.\nThe rest is ASCII04.\nThe rest is ASCII05.\nThe rest is ASCII06.\nThe rest is ASCII07.\nThe rest is ASCII08.\nThe rest is ASCII09.\nThe rest is ASCII10.\nThe rest is ASCII11.\nThe rest is ASCII12.\nThe rest is ASCII13.\nThe rest is ASCII14.\nThe rest is ASCII15.\nThe rest is ASCII16.\nThe rest is ASCII17.\nThe rest is ASCII18.\nThe rest is ASCII19.\nThe rest is ASCII20.\nThe rest is ASCII21.\nThe rest is ASCII22.\n"
#define MANY_UTF8_CRLF_RAW		"Lets sing!\r\n\xe2\x99\xab\xe2\x99\xaa\xe2\x99\xac\xe2\x99\xa9\r\nEat food\r\n\xf0\x9f\x8d\x85\xf0\x9f\x8d\x95\r\n"
#define MANY_UTF8_LF_RAW		"Lets sing!\n\xe2\x99\xab\xe2\x99\xaa\xe2\x99\xac\xe2\x99\xa9\nEat food\n\xf0\x9f\x8d\x85\xf0\x9f\x8d\x95\n"

#endif
