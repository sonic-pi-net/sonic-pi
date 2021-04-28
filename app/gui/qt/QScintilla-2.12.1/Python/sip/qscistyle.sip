// This is the SIP interface definition for QsciStyle.
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


class QsciStyle
{
%TypeHeaderCode
#include <Qsci/qscistyle.h>
%End

public:
    enum TextCase {
        OriginalCase,
        UpperCase,
        LowerCase
    };

    QsciStyle(int style = -1);
    QsciStyle(int style, const QString &description, const QColor &color,
            const QColor &paper, const QFont &font, bool eolFill = false);

    void setStyle(int style);
    int style() const;

    void setDescription(const QString &description);
    QString description() const;

    void setColor(const QColor &color);
    QColor color() const;

    void setPaper(const QColor &paper);
    QColor paper() const;

    void setFont(const QFont &font);
    QFont font() const;

    void setEolFill(bool fill);
    bool eolFill() const;

    void setTextCase(TextCase text_case);
    TextCase textCase() const;

    void setVisible(bool visible);
    bool visible() const;

    void setChangeable(bool changeable);
    bool changeable() const;

    void setHotspot(bool hotspot);
    bool hotspot() const;

    void refresh();
};
