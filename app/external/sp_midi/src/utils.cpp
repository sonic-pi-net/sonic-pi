// MIT License

// Copyright (c) 2016-2021 Luis Lloret

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <algorithm>
#include <time.h>
#include "utils.h"
#include "monitorlogger.h"

using namespace std;

namespace local_utils{
void replace_chars(string& str, char from, char to)
{
    replace_if(str.begin(), str.end(), [from, to](char c) { return c == from; }, to);
}

void downcase(string& str)
{
    transform(str.begin(), str.end(), str.begin(), ::tolower);
}

void safeOscString(string& str)
{
  /*ASCII characters not allowed in names of OSC paths
    See: https://opensoundcontrol.stanford.edu/spec-1_0.html
    ' ' space             32
    #   number sign       35
    *   asterisk          42
    ,   comma             44
    /   forward slash     47
    ?   question mark     63
    [   open bracket      91
    ]   close bracket     93
    {   open curly brace  123
    }   close curly brace 125
  */

    replace_chars(str, ' ', '_');
    replace_chars(str, '#', '_');
    replace_chars(str, '*', '_');
    replace_chars(str, ',', '_');
    replace_chars(str, '/', '_');
    replace_chars(str, '?', '_');
    replace_chars(str, '[', '_');
    replace_chars(str, ']', '_');
    replace_chars(str, '{', '_');
    replace_chars(str, '}', '_');

    // Colons are used by Sonic Pi to separate source
    // segments, so remove them from MIDI source names
    replace_chars(str, ':', '_');
    downcase(str);
}

void logOSCMessage(const char* data, size_t size)
{
    MonitorLogger::getInstance().trace("sent UDP message: ");
    for (int i = 0; i < size; i++) {
        const unsigned char udata = (unsigned char)(data[i]);
        // is it printable?
        if (udata >= 32 && udata <= 127)
            MonitorLogger::getInstance().trace("{}", udata);
        else
            MonitorLogger::getInstance().trace("[{:02x}]", udata);
    }
}


}
