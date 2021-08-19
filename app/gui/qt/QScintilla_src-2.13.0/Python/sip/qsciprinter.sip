// This is the SIP interface definition for QsciPrinter.
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


%If (PyQt_Printer)

class QsciPrinter : QPrinter
{
%TypeHeaderCode
#include <Qsci/qsciprinter.h>
%End

public:
    QsciPrinter(QPrinter::PrinterMode mode = QPrinter::ScreenResolution);
    virtual ~QsciPrinter();

    virtual void formatPage(QPainter &painter, bool drawing, QRect &area,
            int pagenr);
    int magnification() const;
    virtual void setMagnification(int magnification);
    virtual int printRange(QsciScintillaBase *qsb, QPainter &painter,
            int from = -1, int to = -1);
    virtual int printRange(QsciScintillaBase *qsb, int from = -1, int to = -1);
    QsciScintilla::WrapMode wrapMode() const;
    virtual void setWrapMode(QsciScintilla::WrapMode);

private:
    QsciPrinter(const QsciPrinter &);
};

%End
