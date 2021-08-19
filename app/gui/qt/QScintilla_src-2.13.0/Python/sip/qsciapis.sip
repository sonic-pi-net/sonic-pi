// This is the SIP interface definition for QsciAPIs.
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


class QsciAPIs : QsciAbstractAPIs
{
%TypeHeaderCode
#include <Qsci/qsciapis.h>
%End

public:
    QsciAPIs(QsciLexer *lexer /TransferThis/);
    virtual ~QsciAPIs();

    void add(const QString &entry);
    void clear();
    bool load(const QString &fname);
    void remove(const QString &entry);
    void prepare();
    void cancelPreparation();
    QString defaultPreparedName() const;
    bool isPrepared(const QString &filename = QString()) const;
    bool loadPrepared(const QString &filename = QString());
    bool savePrepared(const QString &filename = QString()) const;
    virtual bool event(QEvent *e);
    QStringList installedAPIFiles() const;

    virtual void updateAutoCompletionList(const QStringList &context,
                QStringList &list /In, Out/);
    virtual void autoCompletionSelected(const QString &selection);

    virtual QStringList callTips(const QStringList &context, int commas,
            QsciScintilla::CallTipsStyle style, QList<int> &shifts);

signals:
    void apiPreparationCancelled();
    void apiPreparationStarted();
    void apiPreparationFinished();

private:
    QsciAPIs(const QsciAPIs &);
};
