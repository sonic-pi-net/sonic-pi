//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2022 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "sonicpimetro.h"
#include <QStyle>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QSettings>
#include "qt_api_client.h"
#include "linkaudiostreamswidget.h"
#include <QStyleOption>
#include <QPainter>
#include <QPixmap>
#include <QSvgRenderer>
#include <QEnterEvent>
#include <QThread>
#include "dpi.h"

namespace {

// Tabler icons (MIT, see Tabler-Icons-License.md): ti-ghost-3 (filled) =
// Local/hidden, ti-topology-ring-2 = Network/public. %1 = the render colour.
const char* kGhostSvg =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='%1'>"
    "<path d='M12 3a8 8 0 0 1 8 8v6.954l.009 .103a2.78 2.78 0 0 1 -1.468 2.618l-.163 .08c-1.111 .502 -2.42 .22 -3.266 -.74a.65 .65 0 0 0 -1.024 0a2.65 2.65 0 0 1 -4.176 0a.65 .65 0 0 0 -.512 -.249c-.2 0 -.389 .092 -.55 .296a2.78 2.78 0 0 1 -4.859 -2.005l.01 -.104l.007 -.077l-.008 .074v-6.95l.004 -.25a8 8 0 0 1 7.747 -7.746zm-1.99 6h-.01a1 1 0 1 0 0 2h.01a1 1 0 0 0 0 -2m4 0h-.01a1 1 0 0 0 0 2h.01a1 1 0 0 0 0 -2'/>"
    "</svg>";

const char* kNetworkSvg =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
    "stroke='%1' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'>"
    "<path d='M14 6a2 2 0 1 0 -4 0a2 2 0 0 0 4 0'/>"
    "<path d='M7 18a2 2 0 1 0 -4 0a2 2 0 0 0 4 0'/>"
    "<path d='M21 18a2 2 0 1 0 -4 0a2 2 0 0 0 4 0'/>"
    "<path d='M7 18h10'/>"
    "<path d='M18 16l-5 -8'/>"
    "<path d='M11 8l-5 8'/>"
    "</svg>";

// A metro button that keeps all the app.qss chrome (background, hover, border)
// via QPushButton, but paints its glyph itself so it fills the button rather
// than being capped to the small icon content area a QPushButton allows.
class GlyphButton : public QPushButton
{
public:
    explicit GlyphButton(QWidget* parent = nullptr) : QPushButton(parent) {}
    // color = resting glyph colour; hoverColor = glyph colour while hovered.
    void setGlyph(const char* svg, const QColor& color, const QColor& hoverColor)
    {
        if (m_svg == svg && m_color == color && m_hoverColor == hoverColor) return;
        m_svg = svg;
        m_color = color;
        m_hoverColor = hoverColor;
        m_cache = QPixmap();   // invalidate; re-rendered lazily on next paint
        update();
    }
protected:
    void enterEvent(QEnterEvent*) override { m_hover = true;  m_cache = QPixmap(); update(); }
    void leaveEvent(QEvent*)      override { m_hover = false; m_cache = QPixmap(); update(); }
    void paintEvent(QPaintEvent* e) override
    {
        QPushButton::paintEvent(e);   // qss background / hover / border
        if (!m_svg) return;
        // Glyph fills 60% of the button (not capped to the tiny icon content
        // area). Rendered once per (glyph, colour, size) and cached.
        const int s = int(qMin(width(), height()) * 0.6);
        if (s <= 0) return;
        const qreal dpr = devicePixelRatioF();
        if (m_cache.isNull() || m_cache.size() != QSize(s, s) * dpr) {
            m_cache = QPixmap(QSize(s, s) * dpr);
            m_cache.setDevicePixelRatio(dpr);
            m_cache.fill(Qt::transparent);
            QPainter cp(&m_cache);
            cp.setRenderHint(QPainter::Antialiasing);
            const qreal o = s * 0.10;   // crop Tabler's viewBox margin so it fills
            const QColor c = m_hover ? m_hoverColor : m_color;
            QByteArray bytes =
                QString::fromLatin1(m_svg).arg(c.name(QColor::HexRgb)).toUtf8();
            QSvgRenderer(bytes).render(&cp, QRectF(-o, -o, s + 2 * o, s + 2 * o));
        }
        QPainter p(this);
        p.drawPixmap(QPoint((width() - s) / 2, (height() - s) / 2), m_cache);
    }
private:
    const char* m_svg = nullptr;
    QColor m_color;
    QColor m_hoverColor;
    QPixmap m_cache;
    bool m_hover = false;
};

} // namespace

SonicPiMetro::SonicPiMetro(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, QWidget* parent)
  : QWidget(parent)
  , m_spClient(spClient)
  , m_spAPI(spAPI)
{
  this->theme = theme;
  bool setPosAvailable = isSetPosAvailable();
  mutex = new QMutex;

  // Gated by network visibility (Off/Local/Net) in Preferences > IO:
  // greyed out when Off.
  enableLinkButton = new QPushButton(tr("Link"));
  enableLinkButton->setAutoFillBackground(true);
  enableLinkButton->setObjectName("enableLinkButton");
  enableLinkButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  enableLinkButton->setFlat(true);
  #ifdef Q_OS_MAC
  QString link_shortcut = QKeySequence("Ctrl+t").toString(QKeySequence::NativeText);
  #else
  QString link_shortcut = QKeySequence("alt+t").toString(QKeySequence::NativeText);
  #endif
  enableLinkButton->setProperty("tipTitle", tr("Ableton Link"));
  enableLinkButton->setProperty("tipShortcut", link_shortcut);
  enableLinkButton->setToolTip(tr(
      "Share a tempo with other Link-enabled apps and devices, so everyone "
      "plays in time. Click to join or leave the shared session.\n\n"
      "Link works either on this machine only or across your local "
      "network. Switch modes with the ghost / network button at the end "
      "of this row."));

  tapButton = new QPushButton(tr("Tap"));
  tapButton->setAutoFillBackground(true);
  tapButton->setObjectName("tapButton");
  tapButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  tapButton->setFlat(true);

  tapButton->setProperty("tipTitle", tr("Tap tempo"));
  tapButton->setProperty("tipShortcut", QKeySequence("Shift+Return").toString(QKeySequence::NativeText));
  tapButton->setToolTip(tr("Click repeatedly to the beat to set the BPM manually. Accuracy increases with every additional click."));


  timeWarpSlider = new QSlider(Qt::Horizontal, this);
  timeWarpSlider->setAutoFillBackground(true);
  timeWarpSlider->setObjectName("timeWarpSlider");
  // The row's flexible filler: expands to span the row (min width from app.qss).
  timeWarpSlider->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
  timeWarpSlider->setTickPosition(QSlider::TicksBelow);
  timeWarpSlider->setAccessibleName(tr("Global Time Warp"));
  timeWarpSlider->setProperty("tipTitle", tr("Global Time Warp"));
  timeWarpSlider->setToolTip(tr("Slide to shift the phase of all triggered synths / FX and sent MIDI/OSC events. Negative values trigger everything earlier, positive values trigger things later. The unit is milliseconds."));
  timeWarpSlider->setMinimum(-250);
  timeWarpSlider->setMaximum(999);
  timeWarpSlider->setValue(0);


  timeWarpLineEdit = new TimeWarpEdit(m_spClient, m_spAPI, theme, setPosAvailable);
  timeWarpLineEdit->setAutoFillBackground(true);
  timeWarpLineEdit->setObjectName("timeWarpEdit");
  timeWarpLineEdit->setAccessibleName(tr("Time Warp Scrubber"));
  timeWarpLineEdit->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

  timeWarpLineEdit->setProperty("tipTitle", tr("Global Time Warp"));
  timeWarpLineEdit->setToolTip(tr("Adjust to shift the phase of all triggered synths / FX and sent MIDI/OSC events. Negative values trigger everything earlier, positive values trigger things later. Edit, drag or scroll to modify. Double click to reset to 0. The unit is milliseconds."));
  connect(timeWarpSlider, &QSlider::valueChanged, [this](int value) {
    QSignalBlocker blocker(timeWarpLineEdit);
    timeWarpLineEdit->setDisplayAndWarpToTime(value);
  });

  connect(timeWarpLineEdit, &QLineEdit::textChanged, [this](QString text) {
    QSignalBlocker blocker(timeWarpSlider);
    timeWarpSlider->setValue(timeWarpLineEdit->getTimeWarpValue());
  });

  bpmScrubWidget = new BPMScrubWidget(m_spClient, m_spAPI, theme, setPosAvailable);
  bpmScrubWidget->setObjectName("bpmScrubber");
  bpmScrubWidget->setAccessibleName(tr("BPM Scrubber"));
  bpmScrubWidget->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  bpmScrubWidget->setProperty("tipTitle", tr("Link Tempo (BPM)"));
  bpmScrubWidget->setToolTip(tr("Current Link tempo in beats per minute. Edit, drag or scroll to modify. Double click to reset to 60."));

  // Expand/collapse the inline Link Audio Streams panel. Up-arrow when
  // collapsed, down-arrow when expanded.
  linkStreamsButton = new QPushButton(QString::fromUtf8("\xe2\x86\x91"));  // ↑
  linkStreamsButton->setObjectName("linkStreamsButton");
  linkStreamsButton->setCheckable(true);
  linkStreamsButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  linkStreamsButton->setFlat(true);
  linkStreamsButton->setAccessibleName(tr("Show Link Audio streams panel"));
  linkStreamsButton->setProperty("tipTitle", tr("Link Audio Streams"));
  linkStreamsButton->setToolTip(tr("Show / hide the Link Audio streams panel."));

  // Network-visibility button — a plain QPushButton styled by app.qss exactly
  // like the other metro buttons (background, hover) with a dynamic "public"
  // property for the pink state. Icon switches ghost (Local) / cloud (Network).
  m_rowVisibility = new GlyphButton(this);
  m_rowVisibility->setObjectName("rowVisibility");
  m_rowVisibility->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  m_rowVisibility->setFlat(true);
  m_rowVisibility->setCursor(Qt::PointingHandCursor);
  updateRowVisibility();
  connect(m_rowVisibility, &QPushButton::clicked, this, [this]() {
    const int mode = (static_cast<int>(m_networkMode) == 2) ? 1 : 2;  // toggle
    QSettings().setValue("supersonic/networkVisibility", mode);
    onSupersonicNetworkVisibilityChanged(mode);
  });

  // Single tight row: fixed spacing (never collapses), left-packed via a
  // trailing stretch so the strip stays compact and the dock can narrow.
  QHBoxLayout* metro_row = new QHBoxLayout;
  metro_row->setContentsMargins(0, 0, 0, 0);
  metro_row->setSpacing(ScaleWidthForDPI(14));
  metro_row->addWidget(enableLinkButton);
  metro_row->addWidget(linkStreamsButton);
  metro_row->addWidget(tapButton);
  metro_row->addWidget(bpmScrubWidget);
  metro_row->addWidget(timeWarpSlider, 1);   // flexible filler: spans the row so
  metro_row->addWidget(timeWarpLineEdit);    // the controls reach the full panel
  metro_row->addWidget(m_rowVisibility);     // width (ghost at the right edge).

  QWidget* metroRowWidget = new QWidget(this);
  metroRowWidget->setLayout(metro_row);
  metroRowWidget->setMaximumWidth(640);

  // Hidden by default; toggled by linkStreamsButton.
  linkStreamsWidget = new LinkAudioStreamsWidget(m_spAPI, this);
  linkStreamsWidget->setVisible(false);
  linkStreamsWidget->setVisibilityColors(theme->color("HighlightedBackground"),
                                         theme->color("HighlightedForeground"));

  // Streams panel sits just above the anchored metro row.
  // Stack the streams panel and the metro row in one fixed-width, left-aligned
  // column so they're always the same width — and crucially, so the metro row
  // doesn't change width when the streams panel is shown or hidden (a content-
  // sized column would shrink to the metro row's natural width when collapsed).
  QWidget* linkColumn = new QWidget(this);
  QVBoxLayout* colLayout = new QVBoxLayout(linkColumn);
  colLayout->setContentsMargins(0, 0, 0, 0);
  colLayout->setSpacing(0);
  colLayout->addWidget(linkStreamsWidget);
  colLayout->addWidget(metroRowWidget);
  // Lock the whole column (metro row + expanded panel) to the streams-panel
  // controls width so the collapsed row and the expanded panel are the same
  // width — one cohesive widget. The row's trailing stretch left-packs its
  // controls within that width.
  linkColumn->setFixedWidth(linkStreamsWidget->controlsNaturalWidth());

  QVBoxLayout* metro_layout = new QVBoxLayout;
  metro_layout->addStretch(1);
  metro_layout->addWidget(linkColumn, 0, Qt::AlignLeft);
  setLayout(metro_layout);

  // Restore visibility scope from QSettings (Local default). Link enable
  // is not persisted: joining a mesh is a per-session opt-in.
  {
    QSettings s;
    const int savedNet = s.value("supersonic/networkVisibility", 1).toInt();
    if (savedNet == 1 || savedNet == 2) {
      m_networkMode = static_cast<SonicPi::SonicPiAPI::LinkVisibility>(savedNet);
    }
    m_linkEnabled = false;
    pushLinkConfigToServer();
    updateLinkButtonDisplay();
    updateRowVisibility();
  }

  connect(enableLinkButton, &QPushButton::clicked, [this]() {
    this->toggleLink();
  });

  connect(linkStreamsButton, &QPushButton::clicked, [this]() {
    this->toggleLinkAudioStreams();
  });

  connect(tapButton, &QPushButton::clicked, [this]() {
    this->tapTempo(100);
  });

  connect(m_spClient.get(), &SonicPi::QtAPIClient::UpdateNumActiveLinks, this, &SonicPiMetro::updateActiveLinkCount);
  connect(m_spClient.get(), &SonicPi::QtAPIClient::UpdateBPM, this, &SonicPiMetro::setBPM);

  updateColourTheme();
}

bool SonicPiMetro::isSetPosAvailable()
{
  QPoint pos, new_pos;
  pos = QCursor::pos();
  QGuiApplication::setOverrideCursor(QCursor(Qt::BlankCursor));
  QCursor::setPos(QPoint(0, 0));
  QThread::msleep(250);
  new_pos = QCursor::pos();
  bool available = pos != new_pos;
  QGuiApplication::restoreOverrideCursor();
  QCursor::setPos(pos);
  return available;
}

void SonicPiMetro::onSupersonicNetworkVisibilityChanged(int mode)
{
  if (mode != 1 && mode != 2) return;
  mutex->lock();
  const bool changed = (static_cast<int>(m_networkMode) != mode);
  m_networkMode = static_cast<SonicPi::SonicPiAPI::LinkVisibility>(mode);
  pushLinkConfigToServer();
  updateLinkButtonDisplay();
  mutex->unlock();
  // Keep the streams widget's header/empty-state/slider in sync.
  if (linkStreamsWidget) linkStreamsWidget->applyMasterVisibility(mode);
  updateRowVisibility();
  // Guarded so the ghost button and the streams-panel slider (which both
  // drive this slot) don't repeat the message when nothing changed.
  if (changed) {
    emit statusMessage(mode == 2
        ? tr("Link visibility: public — visible to other devices on your network")
        : tr("Link visibility: local — hidden from the network"));
  }
}

void SonicPiMetro::updateRowVisibility()
{
  if (!m_rowVisibility) return;
  const bool net = (static_cast<int>(m_networkMode) == 2);
  // Bare glyph: ghost (local) / mesh (network); pink when Link is engaged, else
  // the theme foreground (near-white in dark, dark in light) so it stays visible
  // on the transparent button in both themes.
  const QColor glyphColor = m_linkEnabled ? theme->color("HighlightedBackground")
                                          : theme->color("WindowForeground");
  static_cast<GlyphButton*>(m_rowVisibility)
      ->setGlyph(net ? kNetworkSvg : kGhostSvg, glyphColor,
                 theme->color("HighlightedForeground"));  // white on hover
  // Structured tooltip (title + auto-wrapped body — see sonicpitooltip.h).
  m_rowVisibility->setProperty("tipTitle", net
      ? tr("Link Visibility: Local Network")
      : tr("Link Visibility: Local Machine Only"));
  m_rowVisibility->setToolTip(net
      ? tr("Link is visible to other devices on your local network. They can sync tempo and stream audio with this Sonic Pi via Link. Click to switch to Local Machine Only mode to hide Link from the network.")
      : tr("Link is hidden from the local network. Link tempo sync and Link Audio only connect to other apps on this machine. Click to switch to Local Network mode to make Link visible to other machines on your local network."));
  m_rowVisibility->setAccessibleName(net
      ? tr("Network visibility: public")
      : tr("Network visibility: local only"));
}

void SonicPiMetro::linkEnable()
{
  mutex->lock();
  bool turnedOn = false;
  if (!m_linkEnabled) {
    m_linkEnabled = true;
    turnedOn = true;
    pushLinkConfigToServer();
    emit linkEnabled();
  }
  updateLinkButtonDisplay();
  if (linkStreamsWidget) linkStreamsWidget->applyLinkEnabled(true);
  updateRowVisibility();
  mutex->unlock();
  if (turnedOn) {
    // Spell out the visibility consequence: local scope means enabling Link
    // still doesn't expose Sonic Pi to the network.
    emit statusMessage(static_cast<int>(m_networkMode) == 2
        ? tr("Link on — tempo synced with other devices on your network")
        : tr("Link on — local only, hidden from the network"));
  }
}

void SonicPiMetro::linkDisable()
{
  mutex->lock();
  bool turnedOff = false;
  if (m_linkEnabled) {
    m_linkEnabled = false;
    turnedOff = true;
    pushLinkConfigToServer();
    emit linkDisabled();
  }
  updateLinkButtonDisplay();
  if (linkStreamsWidget) linkStreamsWidget->applyLinkEnabled(false);
  updateRowVisibility();
  mutex->unlock();
  if (turnedOff)
    emit statusMessage(tr("Link off"));
}

void SonicPiMetro::toggleLink()
{
  if (m_linkEnabled) linkDisable(); else linkEnable();
}

void SonicPiMetro::pushLinkConfigToServer()
{
  if (!m_spAPI) return;
  // Peer-name first so peers see it. Idempotent on SuperSonic.
  const QString name = QSettings().value("link/peerName", QStringLiteral("Sonic Pi")).toString();
  m_spAPI->SetLinkPeerName(name.toStdString());
  // Link button gates Link; master scope forces Off when Link is disabled.
  const auto effective = m_linkEnabled
      ? m_networkMode
      : SonicPi::SonicPiAPI::LinkVisibility::Off;
  m_spAPI->SetLinkVisibility(effective);
}

void SonicPiMetro::updateActiveLinkCount(int count)
{
  numActiveLinks = count;
  updateLinkButtonDisplay();
}

void SonicPiMetro::updateActiveLinkText()
{
  if (!m_linkEnabled) {
    enableLinkButton->setText(tr("Link"));
  } else if (numActiveLinks == 1) {
    enableLinkButton->setText(tr("1 Link"));
  } else {
    enableLinkButton->setText(tr("%1 Links").arg(numActiveLinks));
  }
}

void SonicPiMetro::updateLinkButtonDisplay()
{
  updateActiveLinkText();
  // Active look driven by the linkOn dynamic property (see app.qss).
  enableLinkButton->setProperty("linkOn", m_linkEnabled);
  enableLinkButton->style()->unpolish(enableLinkButton);
  enableLinkButton->style()->polish(enableLinkButton);
  enableLinkButton->update();

  if (m_linkEnabled) bpmScrubWidget->setLinkEnabled();
  else bpmScrubWidget->setLinkDisabled();
}

void SonicPiMetro::setBPM(double bpm)
{
  bpmScrubWidget->setAndDisplayBPM(bpm);
}

void SonicPiMetro::updateColourTheme()
{
  // Qt re-applies the app-wide stylesheet itself; just re-evaluate
  // state-dependent styling.
  updateLinkButtonDisplay();
  updateRowVisibility();   // glyph colour tracks the theme's ButtonText
  if (linkStreamsWidget)
    linkStreamsWidget->setVisibilityColors(theme->color("HighlightedBackground"),
                                           theme->color("HighlightedForeground"));
}

 void SonicPiMetro::paintEvent(QPaintEvent *)
 {
     QStyleOption opt;
     opt.initFrom(this);
     QPainter p(this);
     style()->drawPrimitive(QStyle::PE_Widget, &opt, &p, this);
 }

void SonicPiMetro::tapTempo(int flashDelay)
{
  qint64 timeStamp = QDateTime::currentMSecsSinceEpoch();

  // Flash driven by the flashing dynamic property (see app.qss).
  auto setFlashing = [this](bool on) {
    tapButton->setProperty("flashing", on);
    tapButton->style()->unpolish(tapButton);
    tapButton->style()->polish(tapButton);
    tapButton->update();
  };
  setFlashing(true);
  QTimer::singleShot(flashDelay, this, [=]() { setFlashing(false); });

  numTaps = numTaps + 1;

  if(numTaps == 1) {
    firstTap = timeStamp;
  } else {

    double timeSinceLastTap = (double)(timeStamp - lastTap);
    double totalTapDistance = (double)(timeStamp - firstTap);
    double avgDistance = totalTapDistance / (numTaps - 1);

    //make sure the first three taps are similarly spaced
    if (((numTaps < 3) &&
         ((timeSinceLastTap > (avgDistance + 30)) ||
          (timeSinceLastTap < (avgDistance - 30)))) ||

        //drop the accuracy requirement for later taps as the error
        //introduced by input timing jitter of the last tap reduces as
        //the tap count increases.
        ((timeSinceLastTap > (avgDistance + 50)) ||
         (timeSinceLastTap < (avgDistance - 50))))
      {
        bpmScrubWidget->displayResetVisualCue();
        numTaps = 1;
        firstTap = timeStamp;

      } else if (numTaps > 2) {
      double newBpm = round(60.0 / (avgDistance / 1000.0));
      if(newBpm != bpmScrubWidget->getBPM()) {
        bpmScrubWidget->setDisplayAndSyncBPM(newBpm);
        bpmScrubWidget->displayBPMChangeVisualCue();
        emit statusMessage(tr("Tap tempo: %1 BPM").arg(newBpm));
      }
    }
  }
  lastTap = timeStamp;
}

void SonicPiMetro::setFocusBPMScrubber()
{
  bpmScrubWidget->setFocusPolicy(Qt::StrongFocus);
  bpmScrubWidget->setFocus();
  bpmScrubWidget->raise();
  bpmScrubWidget->setVisible(true);

}

void SonicPiMetro::setFocusTimeWarpScrubber()
{
  timeWarpLineEdit->setFocusPolicy(Qt::StrongFocus);
  timeWarpLineEdit->setFocus();
  timeWarpLineEdit->raise();
  timeWarpLineEdit->setVisible(true);
}

void SonicPiMetro::toggleLinkAudioStreams()
{
  if (!linkStreamsWidget) return;
  const bool nowVisible = !linkStreamsWidget->isVisible();
  linkStreamsWidget->setVisible(nowVisible);
  linkStreamsButton->setChecked(nowVisible);
  // ↓ when expanded, ↑ when collapsed.
  linkStreamsButton->setText(nowVisible
      ? QString::fromUtf8("\xe2\x86\x93")
      : QString::fromUtf8("\xe2\x86\x91"));
  linkStreamsButton->setAccessibleName(nowVisible
      ? tr("Hide Link Audio streams panel")
      : tr("Show Link Audio streams panel"));
  emit linkAudioStreamsExpandedChanged(nowVisible);
}
