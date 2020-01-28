// This defines the QScintilla plugin for Qt Designer.


#ifndef _QSCINTILLAPLUGIN_H
#define _QSCINTILLAPLUGIN_H

#include <QtDesigner>


class QScintillaPlugin : public QObject, public QDesignerCustomWidgetInterface
{
    Q_OBJECT
#if QT_VERSION >= 0x050000
    Q_PLUGIN_METADATA(IID "org.qt-project.Qt.QDesignerCustomWidgetInterface")
#endif
    Q_INTERFACES(QDesignerCustomWidgetInterface)

public:
    QScintillaPlugin(QObject *parent = 0);
    virtual ~QScintillaPlugin();

    bool isContainer() const;
    bool isInitialized() const;
    QIcon icon() const;
    QString domXml() const;
    QString group() const;
    QString includeFile() const;
    QString name() const;
    QString toolTip() const;
    QString whatsThis() const;
    QWidget *createWidget(QWidget *parent);
    void initialize(QDesignerFormEditorInterface *core);

private:
    bool initialized;
};

#endif
