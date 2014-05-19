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

#endif
