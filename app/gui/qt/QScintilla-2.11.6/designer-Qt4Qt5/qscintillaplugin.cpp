// This implements the QScintilla plugin for Qt Designer.


#include "qscintillaplugin.h"

#include <QtPlugin>

#include <Qsci/qsciscintilla.h>


static const char *qscintilla_pixmap[] = {
    "22 22 35 1",
    "m c #000000",
    "n c #000033",
    "p c #003300",
    "r c #003333",
    "v c #330000",
    "o c #330033",
    "l c #333300",
    "h c #333333",
    "c c #333366",
    "d c #336666",
    "u c #336699",
    "E c #3366cc",
    "k c #663333",
    "i c #663366",
    "b c #666666",
    "e c #666699",
    "A c #6666cc",
    "G c #669966",
    "f c #669999",
    "j c #6699cc",
    "y c #6699ff",
    "t c #996666",
    "a c #999999",
    "g c #9999cc",
    "s c #9999ff",
    "C c #99cc99",
    "x c #99cccc",
    "w c #99ccff",
    "F c #cc99ff",
    "q c #cccccc",
    "# c #ccccff",
    "B c #ccffcc",
    "z c #ccffff",
    "D c #ffffcc",
    ". c none",
    "........#abcda........",
    "......abefghdidcf.....",
    ".....cadhfaehjheck....",
    "....leh.m.ncbehjddo...",
    "...depn.hqhqhr#mccch..",
    "..bb.hcaeh.hqersjhjcd.",
    ".tcm.uqn.hc.uvwxhuygha",
    ".feh.n.hb.hhzemcwhmuAm",
    "Bgehghqqme.eo#wlnysbnj",
    "awhdAzn.engjepswhmuyuj",
    "bCh#m.de.jpqwbmcwemlcz",
    "hcb#xh.nd#qrbswfehwzbm",
    "bd#d.A#zor#qmgbzwgjgws",
    "ajbcuqhqzchwwbemewchmr",
    "Dcn#cwmhgwehgsxbmhEjAc",
    ".uanauFrhbgeahAAbcbuhh",
    ".bohdAegcccfbbebuucmhe",
    "..briuauAediddeclchhh.",
    "...hcbhjccdecbceccch..",
    "....nhcmeccdccephcp...",
    ".....crbhchhhrhhck....",
    "......tcmdhohhcnG....."
};


QScintillaPlugin::QScintillaPlugin(QObject *parent)
    : QObject(parent), initialized(false)
{
}


QScintillaPlugin::~QScintillaPlugin()
{
}


void QScintillaPlugin::initialize(QDesignerFormEditorInterface * /* core */)
{
    initialized = true;
}


bool QScintillaPlugin::isInitialized() const
{
    return initialized;
}


QWidget *QScintillaPlugin::createWidget(QWidget *parent)
{
    return new QsciScintilla(parent);
}


QString QScintillaPlugin::name() const
{
    return "QsciScintilla";
}


QString QScintillaPlugin::group() const
{
    return "Input Widgets";
}


QIcon QScintillaPlugin::icon() const
{
    return QIcon(QPixmap(qscintilla_pixmap));
}


QString QScintillaPlugin::toolTip() const
{
    return "QScintilla Programmer's Editor";
}


QString QScintillaPlugin::whatsThis() const
{
    return "A port to Qt of the Scintilla programmer's editor";
}


bool QScintillaPlugin::isContainer() const
{
    return false;
}


QString QScintillaPlugin::domXml() const
{
    return "<widget class=\"QsciScintilla\" name=\"textEdit\">\n"
        " <property name=\"geometry\">\n"
        "  <rect>\n"
        "   <x>0</x>\n"
        "   <y>0</y>\n"
        "   <width>400</width>\n"
        "   <height>200</height>\n"
        "  </rect>\n"
        " </property>\n"
        " <property name=\"toolTip\" >\n"
        "  <string></string>\n"
        " </property>\n"
        " <property name=\"whatsThis\" >\n"
        "  <string></string>\n"
        " </property>\n"
        "</widget>\n";
}


QString QScintillaPlugin::includeFile() const
{
    return "Qsci/qsciscintilla.h";
}


#if QT_VERSION < 0x050000
Q_EXPORT_PLUGIN2(qscintillaplugin, QScintillaPlugin)
#endif
