#include <map>

#include <QDir>
#include <QString>
#include <QShortcut>
#include <QAction>
#include <QKeySequence>
#include <QSettings>


#ifndef SONICPI_SHORTCUTS_H
#define SONICPI_SHORTCUTS_H

enum DefaultShortcutSet {
    MACOS = 0,
    WINDOWS = 1,
    EMACS = 2,
    EMACS_MACOS = 3
};

class SonicPiShortcuts : public QObject {
    public:
    SonicPiShortcuts(QString config_path);
    ~SonicPiShortcuts();

    QStringList shortcut_ids;

    public slots:
    QKeySequence getShortcut(const QString& id);
    std::map<QString, QKeySequence> getAllShortcuts();
    void updateShortcut(const QString& id, QKeySequence key_sequence);
    void resetShortcut(const QString& id);

    void resetShortcuts();

    void updateActionText(QAction* action, const QString& desc);
    void assignToAction(const QString& id, QAction* action, const QString& desc);

    void loadUserShortcut(const QString& id);
    void loadUserShortcuts();
    void writeUserShortcuts();

    void loadDefaultShortcuts(DefaultShortcutSet set_id);

    private:
    std::map<QString, QKeySequence> shortcutMap;
    std::map<QString, QKeySequence> defaultShortcutMap;
    QString config_path;
    QString user_base;
    QSettings* shortcut_settings;

    static std::map<QString, QStringList> defaultBindings;

};
#endif
