// This is the SIP interface definition for QsciAbstractAPIs.
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


class QsciAbstractAPIs : QObject
{
%TypeHeaderCode
#include <Qsci/qsciabstractapis.h>
%End

public:
    QsciAbstractAPIs(QsciLexer *lexer /TransferThis/);
    virtual ~QsciAbstractAPIs();

    QsciLexer *lexer() const;

    virtual void updateAutoCompletionList(const QStringList &context,
            QStringList &list /In, Out/) = 0;
    virtual void autoCompletionSelected(const QString &selection);

    virtual QStringList callTips(const QStringList &context, int commas,
            QsciScintilla::CallTipsStyle style, QList<int> &shifts) = 0;

private:
    QsciAbstractAPIs(const QsciAbstractAPIs &);
};
