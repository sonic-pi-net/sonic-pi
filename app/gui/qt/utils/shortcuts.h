#include <QShortcut>
#include <QKeySequence>
#include <QString>

#ifndef SHORTCUTS_H
#define SHORTCUTS_H


static inline QKeySequence ctrlKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Meta+%1").arg(key));
#else
    return QKeySequence(QString("Ctrl+%1").arg(key));
#endif
}

// Cmd on Mac, Alt everywhere else
static inline QKeySequence metaKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Ctrl+%1").arg(key));
#else
    return QKeySequence(QString("alt+%1").arg(key));
#endif
}

static inline Qt::Modifier metaKeyModifier()
{
#ifdef Q_OS_MAC
    return Qt::CTRL;
#else
    return Qt::ALT;
#endif
}

static inline QKeySequence shiftMetaKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Shift+Ctrl+%1").arg(key));
#else
    return QKeySequence(QString("Shift+alt+%1").arg(key));
#endif
}

static inline QKeySequence ctrlMetaKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Ctrl+Meta+%1").arg(key));
#else
    return QKeySequence(QString("Ctrl+alt+%1").arg(key));
#endif
}

static inline QKeySequence ctrlShiftMetaKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Shift+Ctrl+Meta+%1").arg(key));
#else
    return QKeySequence(QString("Shift+Ctrl+alt+%1").arg(key));
#endif
}

static inline QKeySequence ctrlShiftKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Shift+Meta+%1").arg(key));
#else
    return QKeySequence(QString("Shift+Ctrl+%1").arg(key));
#endif
}


#endif /* end of include guard: SHORTCUTS_H */
