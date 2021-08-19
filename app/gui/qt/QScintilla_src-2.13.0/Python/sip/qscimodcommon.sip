// This is the SIP interface definition for the parts of the Qsci module common
// to PyQt5 and PyQt6.
//
// Copyright (c) 2021 Riverbank Computing Limited <info@riverbankcomputing.com>
// 
// This file is part of QScintilla.
// 
// This file may be used under the terms of the GNU General Public License
// version 3.0 as published by the Free Software Foundation and appearing in
// the file LICENSE included in the packaging of this file.  Please review the
// following information to ensure the GNU General Public License version 3.0
// requirements will be met: http://www.gnu.org/copyleft/gpl.html.
// 
// If you do not wish to use this file under the terms of the GPL version 3.0
// then you may purchase a commercial license.  For more information contact
// info@riverbankcomputing.com.
// 
// This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
// WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.


%Copying
Copyright (c) 2021 Riverbank Computing Limited <info@riverbankcomputing.com>

This file is part of QScintilla.

This file may be used under the terms of the GNU General Public License
version 3.0 as published by the Free Software Foundation and appearing in
the file LICENSE included in the packaging of this file.  Please review the
following information to ensure the GNU General Public License version 3.0
requirements will be met: http://www.gnu.org/copyleft/gpl.html.

If you do not wish to use this file under the terms of the GPL version 3.0
then you may purchase a commercial license.  For more information contact
info@riverbankcomputing.com.

This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
%End


%Import QtCore/QtCoremod.sip
%Import QtGui/QtGuimod.sip
%Import QtWidgets/QtWidgetsmod.sip

%If (PyQt_Printer)
%Import QtPrintSupport/QtPrintSupportmod.sip
%End


const int QSCINTILLA_VERSION;
const char *QSCINTILLA_VERSION_STR;


%Include qsciscintillabase.sip
%Include qsciscintilla.sip
%Include qsciabstractapis.sip
%Include qsciapis.sip
%Include qscicommand.sip
%Include qscicommandset.sip
%Include qscidocument.sip
%Include qscilexer.sip
%Include qscilexeravs.sip
%Include qscilexerbash.sip
%Include qscilexerbatch.sip
%Include qscilexercmake.sip
%Include qscilexercoffeescript.sip
%Include qscilexercpp.sip
%Include qscilexercsharp.sip
%Include qscilexercss.sip
%Include qscilexercustom.sip
%Include qscilexerd.sip
%Include qscilexerdiff.sip
%Include qscilexerfortran.sip
%Include qscilexerfortran77.sip
%Include qscilexerhtml.sip
%Include qscilexeridl.sip
%Include qscilexerjava.sip
%Include qscilexerjavascript.sip
%Include qscilexerjson.sip
%Include qscilexerlua.sip
%Include qscilexermakefile.sip
%Include qscilexermarkdown.sip
%Include qscilexermatlab.sip
%Include qscilexeroctave.sip
%Include qscilexerpascal.sip
%Include qscilexerperl.sip
%Include qscilexerpostscript.sip
%Include qscilexerpo.sip
%Include qscilexerpov.sip
%Include qscilexerproperties.sip
%Include qscilexerpython.sip
%Include qscilexerruby.sip
%Include qscilexerspice.sip
%Include qscilexersql.sip
%Include qscilexertcl.sip
%Include qscilexertex.sip
%Include qscilexerverilog.sip
%Include qscilexervhdl.sip
%Include qscilexerxml.sip
%Include qscilexeryaml.sip
%Include qscimacro.sip
%Include qsciprinter.sip
%Include qscistyle.sip
%Include qscistyledtext.sip
