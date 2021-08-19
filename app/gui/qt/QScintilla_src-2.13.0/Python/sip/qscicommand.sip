// This is the SIP interface definition for QsciCommand.
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


class QsciCommand
{
%TypeHeaderCode
#include <Qsci/qscicommand.h>
%End

public:
    enum Command {
        LineDown,
        LineDownExtend,
        LineDownRectExtend,
        LineScrollDown,
        LineUp,
        LineUpExtend,
        LineUpRectExtend,
        LineScrollUp,
        ScrollToStart,
        ScrollToEnd,
        VerticalCentreCaret,
        ParaDown,
        ParaDownExtend,
        ParaUp,
        ParaUpExtend,
        CharLeft,
        CharLeftExtend,
        CharLeftRectExtend,
        CharRight,
        CharRightExtend,
        CharRightRectExtend,
        WordLeft,
        WordLeftExtend,
        WordRight,
        WordRightExtend,
        WordLeftEnd,
        WordLeftEndExtend,
        WordRightEnd,
        WordRightEndExtend,
        WordPartLeft,
        WordPartLeftExtend,
        WordPartRight,
        WordPartRightExtend,
        Home,
        HomeExtend,
        HomeRectExtend,
        HomeDisplay,
        HomeDisplayExtend,
        HomeWrap,
        HomeWrapExtend,
        VCHome,
        VCHomeExtend,
        VCHomeRectExtend,
        VCHomeWrap,
        VCHomeWrapExtend,
        LineEnd,
        LineEndExtend,
        LineEndRectExtend,
        LineEndDisplay,
        LineEndDisplayExtend,
        LineEndWrap,
        LineEndWrapExtend,
        DocumentStart,
        DocumentStartExtend,
        DocumentEnd,
        DocumentEndExtend,
        PageUp,
        PageUpExtend,
        PageUpRectExtend,
        PageDown,
        PageDownExtend,
        PageDownRectExtend,
        StutteredPageUp,
        StutteredPageUpExtend,
        StutteredPageDown,
        StutteredPageDownExtend,
        Delete,
        DeleteBack,
        DeleteBackNotLine,
        DeleteWordLeft,
        DeleteWordRight,
        DeleteWordRightEnd,
        DeleteLineLeft,
        DeleteLineRight,
        LineDelete,
        LineCut,
        LineCopy,
        LineTranspose,
        LineDuplicate,
        SelectAll,
        MoveSelectedLinesUp,
        MoveSelectedLinesDown,
        SelectionDuplicate,
        SelectionLowerCase,
        SelectionUpperCase,
        SelectionCut,
        SelectionCopy,
        Paste,
        EditToggleOvertype,
        Newline,
        Formfeed,
        Tab,
        Backtab,
        Cancel,
        Undo,
        Redo,
        ZoomIn,
        ZoomOut,
        ReverseLines,
    };

    Command command() const;
    void execute();
    void setKey(int key);
    void setAlternateKey(int altkey);
    int key() const;
    int alternateKey() const;
    static bool validKey(int key);
    QString description() const;

private:
    QsciCommand(QsciScintilla *qs, Command cmd, int key, int altkey,
            const char *desc);
    QsciCommand(const QsciCommand &);
};
