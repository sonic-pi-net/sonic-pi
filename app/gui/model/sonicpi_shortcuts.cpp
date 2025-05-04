#include "sonicpi_shortcuts.h"

#include <map>
#include <iostream>

#include <QDir>
#include <QString>
#include <QStringList>
#include <QShortcut>
#include <QAction>
#include <QKeySequence>
#include <QSettings>

SonicPiShortcuts::SonicPiShortcuts(QString config_path) {
    this->config_path = config_path;
    QString shortcuts_path = config_path + QDir::separator() + "keyboard-shortcuts.ini";
    this->shortcut_settings = new QSettings(shortcuts_path, QSettings::IniFormat);
    this->user_base = shortcut_settings->value("base", "emacs").toString();

    this->shortcut_ids = {};
    for (const auto& item : defaultBindings) {
        QString id = item.first;
        shortcut_ids.append(id);
    }
    std::cout << "[GUI] [Shortcuts] : " << shortcut_ids.length() << " shortcut IDs loaded" << std::endl;
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
std::map<QString, QKeySequence> SonicPiShortcuts::getAllShortcuts() {
    std::map<QString, QKeySequence> current_shortcuts = {};
    for (const auto& id : shortcut_ids) {
        current_shortcuts[id] = getShortcut(id);
        // std::cout << "[Debug] " << getShortcut(id).toString().toStdString() << std::endl;
    }
    return current_shortcuts;
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
    // 0: macos
    // 1: windows
    // 2: emacs
    // 3: emacs (macos)
    defaultShortcutMap = {};
    for (auto const& x : defaultBindings) {
        QString id = x.first;
        QStringList binding = x.second;

        defaultShortcutMap[id] = QKeySequence(binding[set_id]);
    }
}

std::map<QString, QStringList> SonicPiShortcuts::defaultBindings = {
    {"Run",                     {"Ctrl+R","Alt+R","Alt+R","Ctrl+R"}},
    {"Stop",                    {"Ctrl+S","Alt+S","Alt+S","Ctrl+S"}},
    {"Record",                  {"Ctrl+Shift+R","Alt+Shift+R","Alt+Shift+R","Ctrl+Shift+R"}},
    {"Load",                    {"Meta+O","Ctrl+O","Alt+Shift+O","Ctrl+Shift+O"}},
    {"Align",                   {"Ctrl+M","Alt+M","Alt+M","Ctrl+M"}},
    {"Comment",                 {"Ctrl+/","Alt+/","Alt+/","Ctrl+/"}},
    {"Transpose",               {"Meta+T","Ctrl+T","Ctrl+T","Meta+T"}},
    {"ShiftUp",                 {"Ctrl+Meta+P","Ctrl+Alt+P","Ctrl+alt+P","Ctrl+Meta+P"}},
    {"ShiftDown",               {"Ctrl+Meta+N","Ctrl+Alt+N","Ctrl+alt+N","Ctrl+Meta+N"}},
    {"ContextualDocs",          {"Shift+F1","Shift+F1","Ctrl+I","Meta+I"}},
    {"TextZoomIn",              {"Ctrl+=","Ctrl+=","Alt++","Ctrl++"}},
    {"TextZoomOut",             {"Ctrl+-","Ctrl+-","Alt+-","Ctrl+-"}},
    {"Scope",                   {"Ctrl+O","Alt+O","Alt+O","Ctrl+O"}},
    {"CycleThemes",             {"Ctrl+Shift+M","Alt+Shift+M","Alt+Shift+M","Ctrl+Shift+M"}},
    {"Info",                    {"Ctrl+1","Alt+1","Alt+1","Ctrl+1"}},
    {"Help",                    {"F1","F1","Alt+i","Ctrl+i"}},
    {"Prefs",                   {"Ctrl+p","Alt+p","Alt+p","Ctrl+p"}},
    {"TabPrev",                 {"Ctrl+Shift+[","Alt+Shift+[","Alt+Shift+[","Ctrl+Shift+["}},
    {"TabNext",                 {"Ctrl+Shift+]","Alt+Shift+]","Alt+Shift+]","Ctrl+Shift+]"}},
    {"Tab1",                    {"Ctrl+Shift+1","Alt+Shift+1","Alt+Shift+1","Ctrl+Shift+1"}},
    {"Tab2",                    {"Ctrl+Shift+2","Alt+Shift+2","Alt+Shift+2","Ctrl+Shift+2"}},
    {"Tab3",                    {"Ctrl+Shift+3","Alt+Shift+3","Alt+Shift+3","Ctrl+Shift+3"}},
    {"Tab4",                    {"Ctrl+Shift+4","Alt+Shift+4","Alt+Shift+4","Ctrl+Shift+4"}},
    {"Tab5",                    {"Ctrl+Shift+5","Alt+Shift+5","Alt+Shift+5","Ctrl+Shift+5"}},
    {"Tab6",                    {"Ctrl+Shift+6","Alt+Shift+6","Alt+Shift+6","Ctrl+Shift+6"}},
    {"Tab7",                    {"Ctrl+Shift+7","Alt+Shift+7","Alt+Shift+7","Ctrl+Shift+7"}},
    {"Tab8",                    {"Ctrl+Shift+8","Alt+Shift+8","Alt+Shift+8","Ctrl+Shift+8"}},
    {"Tab9",                    {"Ctrl+Shift+9","Alt+Shift+9","Alt+Shift+9","Ctrl+Shift+9"}},
    {"Tab0",                    {"Ctrl+Shift+0","Alt+Shift+0","Alt+Shift+0","Ctrl+Shift+0"}},
    {"Link",                    {"Ctrl+t","Alt+T","Alt+t","Ctrl+t"}},
    {"TapTempo",                {"Shift+Return","Shift+Return","Shift+Return","Shift+Return"}},
    {"FocusEditor",             {"Meta+Shift+e","Ctrl+Shift+e","Ctrl+Shift+e","Meta+Shift+e"}},
    {"FocusLogs",               {"Meta+Shift+l","Ctrl+Shift+l","Ctrl+Shift+l","Meta+Shift+l"}},
    {"FocusContext",            {"Meta+Shift+t","Ctrl+Shift+t","Ctrl+Shift+t","Meta+Shift+t"}},
    {"FocusCues",               {"Meta+Shift+c","Ctrl+Shift+c","Ctrl+Shift+c","Meta+Shift+c"}},
    {"FocusPrefs",              {"Meta+,","Alt+,","Ctrl+Shift+P","Meta+Shift+P"}},
    {"FocusHelpListing",        {"Meta+Shift+h","Ctrl+Shift+h","Ctrl+Shift+h","Meta+Shift+h"}},
    {"FocusHelpDetails",        {"Meta+Shift+d","Ctrl+Shift+d","Ctrl+Shift+d","Meta+Shift+d"}},
    {"FocusErrors",             {"Meta+Shift+R","Ctrl+Shift+R","Ctrl+Shift+R","Meta+Shift+R"}},
    {"FocusBPMScrubber",        {"Meta+Shift+b","Ctrl+Shift+b","Ctrl+Shift+b","Meta+Shift+b"}},
    {"FocusTimeWarpScrubber",   {"Meta+Shift+w","Ctrl+Shift+w","Ctrl+Shift+w","Meta+Shift+w"}},
    {"ShowButtons",             {"Ctrl+Shift+b","Alt+Shift+b","Alt+Shift+b","Ctrl+Shift+b"}},
    {"ShowCueLog",              {"Ctrl+Shift+c","Alt+Shift+c","Alt+Shift+c","Ctrl+Shift+c"}},
    {"ShowLog",                 {"Ctrl+Shift+l","Alt+Shift+l","Alt+Shift+l","Ctrl+Shift+l"}},
    {"SetMark",                 {"Meta+Space","Ctrl+Space","Ctrl+Space","Meta+Space"}},
    {"LogZoomIn",               {"Meta+=","Ctrl+Alt+=","Ctrl+=","Meta+="}},
    {"LogZoomOut",              {"Meta+-","Ctrl+Alt+-","Ctrl+-","Meta+-"}},
    {"Down",                    {"Meta+n","Ctrl+n","Ctrl+n","Meta+n"}},
    {"Up",                      {"Meta+p","Ctrl+p","Ctrl+p","Meta+p"}},
    {"UpTen",                   {"Ctrl+up","PgUp","Alt+Shift+U","Ctrl+Shift+U"}},
    {"DownTen",                 {"Ctrl+down","PgDown","Alt+Shift+D","Ctrl+Shift+D"}},
    {"CutToEnd",                {"Meta+k","Ctrl+k","Ctrl+k","Meta+k"}},
    {"Copy",                    {"Meta+c","Ctrl+c","Alt+]","Ctrl+]"}},
    {"Cut",                     {"Meta+x","Ctrl+x","Ctrl+]","Meta+]"}},
    {"Paste",                   {"Meta+v","Ctrl+v","Ctrl+y","Meta+y"}},
    {"Right",                   {"Meta+f","Ctrl+f","Ctrl+f","Meta+f"}},
    {"Left",                    {"Meta+b","Ctrl+b","Ctrl+b","Meta+b"}},
    {"DeleteForward",           {"Meta+d","Ctrl+d","Ctrl+d","Meta+d"}},
    {"DeleteBackward",          {"Meta+h","Ctrl+h","Ctrl+h","Meta+h"}},
    {"LineStart",               {"Ctrl+Left","Home","Ctrl+A","Meta+A"}},
    {"LineEnd",                 {"Ctrl+Right","End","Ctrl+E","Meta+E"}},
    {"DocStart",                {"Ctrl+Shift+,","Alt+Shift+,","Alt+Shift+,","Alt+Shift+,"}},
    {"DocEnd",                  {"Ctrl+Shift+.","Alt+Shift+.","Alt+Shift+.","Alt+Shift+."}},
    {"WordRight",               {"Alt+Right","Ctrl+Right","Alt+F","Ctrl+F"}},
    {"WordLeft",                {"Alt+Left","Ctrl+Left","Alt+B","Ctrl+B"}},
    {"CenterVertically",        {"Meta+l","Ctrl+l","Ctrl+l","Meta+l"}},
    {"Undo",                    {"Ctrl+z","Ctrl+z","Alt+z","Ctrl+z"}},
    {"Redo",                    {"Ctrl+Shift+z","Ctrl+Shift+z","Alt+Shift+z","Ctrl+Shift+z"}},
    {"SelectAll",               {"Ctrl+a","Ctrl+a","Alt+a","Ctrl+a"}},
    {"DeleteWordRight",         {"Ctrl+d","Alt+d","Alt+d","Ctrl+d"}},
    {"DeleteWordLeft",          {"Ctrl+Backspace","Alt+Backspace","Alt+Backspace","Ctrl+Backspace"}},
    {"UpcaseWord",              {"Ctrl+u","Alt+u","Alt+u","Ctrl+u"}},
    {"DowncaseWord",            {"Ctrl+l","Alt+l","Alt+l","Ctrl+l"}},
    {"FullScreen",              {"Ctrl+Shift+f","F11","Alt+Shift+F","Ctrl+Shift+F"}},
    {"ReloadServerCode",        {"F8", "F8", "F8", "F8"}},
    {"ToggleFocusMode",         {"F10", "F10", "F10", "F10"}},
    {"ToggleScopePaused",       {"F12", "F12", "F12", "F12"}}
};
