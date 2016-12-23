// --
//  This file is part of Sonic Pi: http://sonic-pi.net
//  Full project source: https://github.com/samaaron/sonic-pi
//  License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
//  Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
//  All rights reserved.
//
//  Permission is granted for use, copying, modification, distribution,
//  and distribution of modified versions of this work as long as this
//  notice is included.
// ++

var editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    mode: "text/x-ruby",
    tabMode: "indent",
    matchBrackets: true,
    indentUnit: 2,
    lineNumbers: true,
    value: "#foo"
});
