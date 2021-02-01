#pragma once

#include <vector>
#include <string>

namespace SonicPi
{

// String split with multiple delims
void string_split(const std::string& text, const char* delims, std::vector<std::string>& tokens);

std::vector<std::string> string_split(const std::string& text, const char* delims);

std::string string_replace(std::string subject, const std::string& search, const std::string& replace);

// trim from beginning of string (left)
std::string string_left_trim(std::string s, const char* t = " \t\n\r\f\v");

// trim from end of string (right)
std::string string_right_trim(std::string s, const char* t = " \t\n\r\f\v");

// trim from both ends of string (left & right)
std::string string_trim(std::string s, const char* t = " \t\n\r\f\v");

std::string random_string(std::string::size_type length);

std::string string_number_name(int i);
uint32_t string_number_from_name(const std::string& name);

} // namespace SonicPi 
