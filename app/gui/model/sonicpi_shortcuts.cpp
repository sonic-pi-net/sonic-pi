#include <QDir>
#include <QString>
#include <QStringList>
#include <QShortcut>
#include <QAction>
#include <QKeySequence>
#include <QSettings>
#include <qcontainerfwd.h>
#include <qkeysequence.h>

#include "sonicpi_shortcuts.h"

SonicPiShortcuts::SonicPiShortcuts(QString config_path) {
    this->config_path = config_path;
    QString shortcuts_path = config_path + QDir::separator() + "keyboard-shortcuts.ini";
    this->shortcut_settings = new QSettings(shortcuts_path, QSettings::IniFormat);
    this->user_base = shortcut_settings->value("base", "emacs").toString();
}
SonicPiShortcuts::~SonicPiShortcuts() {
}

QKeySequence SonicPiShortcuts::getShortcut(const QString& id) {
    if (shortcutMap.find(id) == shortcutMap.end()) {
        if (defaultShortcutMap.find(id) == defaultShortcutMap.end()) {
            return QKeySequence();
        }
        return defaultShortcutMap[id];
    }
    return shortcutMap[id];
}
void SonicPiShortcuts::updateShortcut(const QString& id, QKeySequence key_sequence) {
    if (key_sequence.isEmpty()) {
        return resetShortcut(id);
    }
    shortcutMap[id].swap(key_sequence);
}
void SonicPiShortcuts::resetShortcut(const QString& id) {
    shortcutMap.erase(id);
}

void SonicPiShortcuts::resetShortcuts() {
    shortcutMap.clear();
}

void SonicPiShortcuts::assignToAction(const QString& id, QAction* action, const QString& desc) {
    action->setShortcut(getShortcut(id));
    updateActionText(action, desc);
}
void SonicPiShortcuts::updateActionText(QAction* action, const QString& desc) {
    QString shortcutDesc = action->shortcut().toString(QKeySequence::PortableText);
    action->setToolTip(desc + " (" + shortcutDesc + ")");
    action->setText(action->iconText());
    action->setStatusTip(desc + " (" + shortcutDesc + ")");
}

void SonicPiShortcuts::loadUserShortcuts() {
    // Determine which shortcuts to load based on the 'base' value
    if (user_base == "none")
    {
        // don't load any shortcuts
    }
    else if (user_base == "mac")
    {
        loadDefaultShortcuts(DefaultShortcutSet::MACOS);
    }
    else if (user_base == "win")
    {
        loadDefaultShortcuts(DefaultShortcutSet::WINDOWS);
    }
    else
    {
        // default
        #ifdef Q_OS_MAC
        loadDefaultShortcuts(DefaultShortcutSet::EMACS_MACOS);
        #else
        loadDefaultShortcuts(DefaultShortcutSet::EMACS);
        #endif
    }

    // Load user shortcuts
    QStringList keys = shortcut_settings->allKeys();
    for (const QString& id : keys)
    {
        if (shortcut_ids.contains(id)) {
            shortcutMap[id] = QKeySequence(shortcut_settings->value(id, "").toString());
        } else {
            if (id == "base") {
                continue;
            }
            shortcut_settings->remove(id);
        }
    }
}
void SonicPiShortcuts::writeUserShortcuts() {
    for (auto const& x : shortcutMap) {
        QString id = x.first;
        QKeySequence sequence = x.second;
        shortcut_settings->setValue(id, sequence.toString());
    }
    shortcut_settings->setValue("base", user_base);
    shortcut_settings->sync();
}

void SonicPiShortcuts::loadDefaultShortcuts(DefaultShortcutSet set_id) {
    // 1: macos
    // 2: windows
    // 3: emacs
    // 4: emacs (macos)
    defaultShortcutMap = {};
    for (auto const& x : defaultBindings) {
        QString id = x.first;
        QStringList binding = x.second;

        defaultShortcutMap[id] = QKeySequence(binding[set_id]);
    }
}
