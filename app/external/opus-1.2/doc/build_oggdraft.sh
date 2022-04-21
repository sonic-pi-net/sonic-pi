#!/bin/sh

# Copyright (c) 2012 Xiph.Org Foundation and Mozilla Corporation
#
#  This file is extracted from RFC6716. Please see that RFC for additional
#  information.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#  - Redistributions of source code must retain the above copyright
#  notice, this list of conditions and the following disclaimer.
#
#  - Redistributions in binary form must reproduce the above copyright
#  notice, this list of conditions and the following disclaimer in the
#  documentation and/or other materials provided with the distribution.
#
#  - Neither the name of Internet Society, IETF or IETF Trust, nor the
#  names of specific contributors, may be used to endorse or promote
#  products derived from this software without specific prior written
#  permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
#  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#Stop on errors
set -e
#Set the CWD to the location of this script
[ -n "${0%/*}" ] && cd "${0%/*}"

if test -z `which xml2rfc 2> /dev/null`; then
  echo "Error: couldn't find xml2rfc."
  echo
  echo "Please install xml2rfc version 2 or later."
  echo "E.g. 'pip install xml2rfc' or follow the instructions"
  echo "on http://pypi.python.org/pypi/xml2rfc/ or tools.ietf.org."
  exit 1
fi

echo running xml2rfc
# version 2 syntax
xml2rfc draft-ietf-codec-oggopus.xml --text --html
