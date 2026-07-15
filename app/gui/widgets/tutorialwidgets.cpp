//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "tutorialwidgets.h"

#include <QAccessible>
#include <QAccessibleWidget>
#include <QApplication>
#include <QClipboard>
#include <QKeyEvent>
#include <QMenu>
#include <QMouseEvent>

TutProseText::~TutProseText()
{
    if (m_group)
        m_group->remove(this);
}

void TutProseText::mousePressEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton || !m_group)
    {
        QWidget::mousePressEvent(e);
        return;
    }
    // A plain press starts a zero-length selection; a click that never drags
    // leaves it empty, so release still fires the link handler.
    m_group->beginDrag(this, hitTest(e->position()));
}

void TutProseText::mouseMoveEvent(QMouseEvent* e)
{
    if (m_group && m_group->dragging() && (e->buttons() & Qt::LeftButton))
    {
        m_group->extendTo(e->globalPosition().toPoint());
        return;
    }
    QString anchor = m_doc.documentLayout()->anchorAt(e->position());
    setCursor(anchor.isEmpty() ? Qt::IBeamCursor : Qt::PointingHandCursor);
    applyAnchorHover(anchor.startsWith(QLatin1String("opt:")) ? anchor : QString());
}

void TutProseText::leaveEvent(QEvent* e)
{
    applyAnchorHover(QString());
    QWidget::leaveEvent(e);
}

void TutProseText::applyAnchorHover(const QString& href)
{
    if (m_hoverAnchor == href)
        return;
    m_hoverAnchor = href;
    for (QTextBlock block = m_doc.begin(); block != m_doc.end(); block = block.next())
    {
        for (QTextBlock::iterator it = block.begin(); !it.atEnd(); ++it)
        {
            const QTextFragment frag = it.fragment();
            QTextCharFormat fmt = frag.charFormat();
            // Only editable-number anchors: prose hyperlinks keep their own
            // permanent underline from the markup.
            if (!fmt.isAnchor() || !fmt.anchorHref().startsWith(QLatin1String("opt:")))
                continue;
            const bool on = !href.isEmpty() && fmt.anchorHref() == href;
            if (fmt.fontUnderline() == on)
                continue;
            fmt.setFontUnderline(on);
            QTextCursor cursor(&m_doc);
            cursor.setPosition(frag.position());
            cursor.setPosition(frag.position() + frag.length(), QTextCursor::KeepAnchor);
            cursor.setCharFormat(fmt);
        }
    }
    update();
}

void TutProseText::mouseReleaseEvent(QMouseEvent* e)
{
    bool wasSelecting = m_group && m_group->dragging();
    if (m_group)
        m_group->finishDrag();
    // Treat as a link click only when nothing was selected (a plain click).
    if (!m_group || !m_group->hasAnySelection())
    {
        QString anchor = m_doc.documentLayout()->anchorAt(e->position());
        if (!anchor.isEmpty() && m_linkHandler)
            m_linkHandler(anchor);
    }
    Q_UNUSED(wasSelecting);
}

void TutProseText::mouseDoubleClickEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton || !m_group)
        return;
    m_group->clearAll();
    int pos = hitTest(e->position());
    m_group->setAnchor(this, pos);
    QTextCursor c(&m_doc);
    c.setPosition(pos);
    c.select(QTextCursor::WordUnderCursor);
    m_selection = c;
    update();
}

void TutProseText::keyPressEvent(QKeyEvent* e)
{
    if (m_group && e->matches(QKeySequence::Copy))
    {
        m_group->copy();
        e->accept();
        return;
    }
    QWidget::keyPressEvent(e);
}

// ---- assistive technology ----

namespace
{

// Prose blocks read as static text: the whole block's plain text is the
// accessible name, so screen readers speak pages again (the pane replaced a
// QTextBrowser, which provided this for free).
class TutProseTextAccessible : public QAccessibleWidget
{
public:
    explicit TutProseTextAccessible(QWidget* w)
        : QAccessibleWidget(w, QAccessible::StaticText)
    {
    }

    QString text(QAccessible::Text t) const override
    {
        if (t == QAccessible::Name)
            return static_cast<TutProseText*>(widget())->plainText();
        return QAccessibleWidget::text(t);
    }
};

// Dials expose the standard value interface (current/min/max/step) so screen
// readers can read and adjust them like native sliders.
class TutDialAccessible : public QAccessibleWidget, public QAccessibleValueInterface
{
public:
    explicit TutDialAccessible(QWidget* w)
        : QAccessibleWidget(w, QAccessible::Dial)
    {
    }

    void* interface_cast(QAccessible::InterfaceType t) override
    {
        if (t == QAccessible::ValueInterface)
            return static_cast<QAccessibleValueInterface*>(this);
        return QAccessibleWidget::interface_cast(t);
    }

    QString text(QAccessible::Text t) const override
    {
        TutDial* dial = static_cast<TutDial*>(widget());
        if (t == QAccessible::Name)
            return dial->optName();
        if (t == QAccessible::Value)
            return dial->valueText();
        return QAccessibleWidget::text(t);
    }

    QVariant currentValue() const override
    {
        return static_cast<TutDial*>(widget())->value();
    }

    void setCurrentValue(const QVariant& value) override
    {
        static_cast<TutDial*>(widget())->setValue(value.toDouble());
    }

    QVariant maximumValue() const override
    {
        return static_cast<TutDial*>(widget())->maximum();
    }

    QVariant minimumValue() const override
    {
        return static_cast<TutDial*>(widget())->minimum();
    }

    QVariant minimumStepSize() const override
    {
        return static_cast<TutDial*>(widget())->step();
    }
};

QAccessibleInterface* tutorialWidgetAccessibleFactory(const QString& className, QObject* object)
{
    QWidget* widget = qobject_cast<QWidget*>(object);
    if (!widget)
        return nullptr;
    if (className == QLatin1String("TutProseText"))
        return new TutProseTextAccessible(widget);
    if (className == QLatin1String("TutDial"))
        return new TutDialAccessible(widget);
    return nullptr;
}

} // namespace

void registerTutorialWidgetAccessibility()
{
    static bool installed = false;
    if (installed)
        return;
    installed = true;
    QAccessible::installFactory(tutorialWidgetAccessibleFactory);
}
