//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron.
// All rights reserved.
//++

#include "linkaudiostreamswidget.h"

#include "linkvisibilitytoggle.h"
#include "api/sonicpi_api.h"
#include "dpi.h"

#include <QCheckBox>
#include <QFormLayout>
#include <QGridLayout>
#include <QSet>
#include <QStringList>
#include <QTableWidgetItem>
#include <algorithm>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QStyle>
#include <QHostAddress>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QSettings>
#include <QSlider>
#include <QTableWidget>
#include <QTimer>
#include <QUdpSocket>
#include <QVBoxLayout>
#include <QtEndian>

#include <cmath>
#include <cstring>

using SonicPi::SonicPiAPI;
using SonicPi::SonicPiPortId;

namespace {

// Append an OSC string + mandatory NUL + pad to 4-byte boundary.
void appendOscString(QByteArray& p, const char* s) {
    p.append(s);
    p.append('\0');
    while (p.size() % 4 != 0) p.append('\0');
}

QByteArray buildOscRequest(const char* addr) {
    QByteArray pkt;
    appendOscString(pkt, addr);
    appendOscString(pkt, ",");
    return pkt;
}

QByteArray buildOscInt32Request(const char* addr, int32_t value) {
    QByteArray pkt;
    appendOscString(pkt, addr);
    appendOscString(pkt, ",i");
    const auto be = qToBigEndian<qint32>(value);
    pkt.append(reinterpret_cast<const char*>(&be), 4);
    return pkt;
}

// /clock/audio/input/latency/set <peer:str> <chan:str> <seconds:float>
QByteArray buildOscInputLatencyRequest(const char* addr, const QString& peer,
                                       const QString& chan, float seconds) {
    QByteArray pkt;
    appendOscString(pkt, addr);
    appendOscString(pkt, ",ssf");
    appendOscString(pkt, peer.toUtf8().constData());
    appendOscString(pkt, chan.toUtf8().constData());
    quint32 bits = 0;
    std::memcpy(&bits, &seconds, sizeof(bits));
    const auto be = qToBigEndian<quint32>(bits);
    pkt.append(reinterpret_cast<const char*>(&be), 4);
    return pkt;
}

// Reader helpers — return false on truncation.
struct OscReader {
    const QByteArray& data;
    int p = 0;
    bool readStr(QString& s) {
        int end = p;
        while (end < data.size() && data[end] != '\0') ++end;
        if (end >= data.size()) return false;
        s = QString::fromUtf8(data.constData() + p, end - p);
        p = ((end + 4) / 4) * 4;
        return true;
    }
    bool readInt32(int32_t& v) {
        if (p + 4 > data.size()) return false;
        v = qFromBigEndian<qint32>(data.constData() + p);
        p += 4;
        return true;
    }
    bool readFloat(float& v) {
        if (p + 4 > data.size()) return false;
        const auto bits = qFromBigEndian<quint32>(data.constData() + p);
        std::memcpy(&v, &bits, sizeof(v));
        p += 4;
        return true;
    }
};

// /clock/audio/channels.reply <count> [channelId channelName peerId peerName]*
// One (peerName, channelName) per channel; Live publishes several per peer.
QVector<LinkAudioStreamsWidget::PeerChannel>
parseChannelsReply(const QByteArray& data) {
    QVector<LinkAudioStreamsWidget::PeerChannel> out;
    OscReader r{data};
    QString addr, typetag;
    if (!r.readStr(addr) || addr != "/clock/audio/channels.reply") return out;
    if (!r.readStr(typetag) || typetag.isEmpty() || typetag[0] != ',') return out;
    int32_t count = 0;
    if (!r.readInt32(count)) return out;
    for (int i = 0; i < count; ++i) {
        QString channelId, channelName, peerId, peerName;
        if (!r.readStr(channelId) || !r.readStr(channelName)
            || !r.readStr(peerId) || !r.readStr(peerName)) break;
        out.push_back({ peerName, channelName });
    }
    // Sort by peer, then channel.
    std::sort(out.begin(), out.end(),
              [](const LinkAudioStreamsWidget::PeerChannel& a,
                 const LinkAudioStreamsWidget::PeerChannel& b) {
        const int p = a.peerName.compare(b.peerName, Qt::CaseInsensitive);
        if (p != 0) return p < 0;
        return a.channelName.compare(b.channelName, Qt::CaseInsensitive) < 0;
    });
    return out;
}

// /clock/audio/inputs.reply <count>
//   [peerName:s channelName:s busIdx:i sampleRate:i sourceNumChannels:i
//    bufferedMs:f connectionState:i droppedSourceBuffers:i
//    networkGapBuffers:i totalSourceBufferCalls:i duplicateCountCalls:i
//    latencySeconds:f]*
// One entry per active subscription; the four diagnostic counters are skipped.
QVector<LinkAudioStreamsWidget::InputStatus>
parseInputsReply(const QByteArray& data) {
    QVector<LinkAudioStreamsWidget::InputStatus> out;
    OscReader r{data};
    QString addr, typetag;
    if (!r.readStr(addr) || addr != "/clock/audio/inputs.reply") return out;
    if (!r.readStr(typetag) || typetag.isEmpty() || typetag[0] != ',') return out;
    int32_t count = 0;
    if (!r.readInt32(count)) return out;
    for (int i = 0; i < count; ++i) {
        LinkAudioStreamsWidget::InputStatus s;
        int32_t busIdx = -1, sampleRate = 0, srcCh = 0, state = 0;
        int32_t dropped = 0, gaps = 0, total = 0, dup = 0;
        float bufferedMs = 0.0f, latencySeconds = 0.0f;
        if (!r.readStr(s.peerName) || !r.readStr(s.channelName)
            || !r.readInt32(busIdx) || !r.readInt32(sampleRate)
            || !r.readInt32(srcCh) || !r.readFloat(bufferedMs)
            || !r.readInt32(state) || !r.readInt32(dropped)
            || !r.readInt32(gaps) || !r.readInt32(total)
            || !r.readInt32(dup) || !r.readFloat(latencySeconds)) break;
        s.busIdx = busIdx;
        s.sampleRate = sampleRate;
        s.numChannels = srcCh;
        s.bufferedMs = bufferedMs;
        s.state = state;
        s.latencySeconds = latencySeconds;
        out.push_back(s);
    }
    return out;
}

} // namespace

LinkAudioStreamsWidget::LinkAudioStreamsWidget(std::shared_ptr<SonicPiAPI> spAPI,
                                               QWidget* parent)
    : QWidget(parent), m_spAPI(std::move(spAPI))
{
    auto* layout = new QVBoxLayout(this);
    // Zero margins so the peer-table border lines up with the Link/Tap/BPM
    // row below; colour/sizing comes from app.qss.
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(2);

    auto makeSectionLabel = [this](const QString& text) {
        auto* l = new QLabel(text.toUpper(), this);
        l->setObjectName("linkSectionLabel");  // subordinate to the widget title
        l->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
        return l;
    };

    // Link Name — peer identity as broadcast over Link.
    auto* nameLabel = makeSectionLabel(tr("Link Name"));
    m_peerNameEdit = new QLineEdit(this);
    m_peerNameEdit->setObjectName("linkPeerName");
    m_peerNameEdit->setAccessibleName(tr("Link Name"));
    m_peerNameEdit->setPlaceholderText(tr("Name visible to other Link peers"));
    m_peerNameEdit->setText(
        QSettings().value("link/peerName", QStringLiteral("Sonic Pi")).toString());
    // Fixed (narrow) width so the column doesn't grab all horizontal slack.
    m_peerNameEdit->setFixedWidth(90);

    // Engine-wide receive latency, 0-2000 ms. SuperSonic's contract is
    // per-input, so this is reconciled onto every subscription (see
    // enforceEngineLatency). Persisted across restarts.
    const int initialLatencyMs =
        QSettings().value("link/audioLatencyMs", 200).toInt();
    auto* latLabel = makeSectionLabel(tr("Latency"));
    m_latencySlider = new QSlider(Qt::Horizontal, this);
    m_latencySlider->setObjectName("linkLatencySlider");
    m_latencySlider->setAccessibleName(tr("Latency"));
    m_latencySlider->setAccessibleDescription(tr("milliseconds"));
    m_latencySlider->setRange(0, 2000);
    m_latencySlider->setValue(initialLatencyMs);
    m_latencySlider->setFixedWidth(110);
    m_latencySlider->setToolTip(tr(
        "Link Audio receive latency in ms. Smaller = closer to real-time "
        "monitoring; larger = more robust against network jitter. "
        "Engine-wide (applies to every link_audio subscription)."));
    m_latencyValueLabel = new QLabel(tr("%1 ms").arg(initialLatencyMs), this);
    m_latencyValueLabel->setObjectName("linkLatencyValue");
    // Fixed width for the widest value ("2000 ms") so the row doesn't
    // wobble as the slider is dragged; right-aligned to pin the digits.
    m_latencyValueLabel->setFixedWidth(56);
    m_latencyValueLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    auto* latencyControl = new QWidget(this);
    latencyControl->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    auto* latencyControlLayout = new QHBoxLayout(latencyControl);
    latencyControlLayout->setContentsMargins(0, 0, 0, 0);
    latencyControlLayout->setSpacing(6);
    latencyControlLayout->addWidget(m_latencySlider);
    latencyControlLayout->addWidget(m_latencyValueLabel);

    // Share-audio as a checkable On/Off pill. Persisted across restarts;
    // only the Link button itself is session-only.
    const bool initialShareAudio =
        QSettings().value("link/audioPublish", false).toBool();
    m_shareAudioBox = new QPushButton(initialShareAudio ? tr("On") : tr("Off"), this);
    m_shareAudioBox->setObjectName("shareAudioToggle");
    m_shareAudioBox->setAccessibleName(tr("Stream Audio"));
    m_shareAudioBox->setCheckable(true);
    m_shareAudioBox->setChecked(initialShareAudio);
    m_shareAudioBox->setFlat(true);
    m_shareAudioBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    m_shareAudioBox->setToolTip(tr(
        "When on, our audio channels are visible to other Link peers and "
        "stream to anyone who subscribes. Preference is remembered between "
        "sessions."));
    m_shareAudioBox->setProperty("muted", true);
    connect(m_shareAudioBox, &QPushButton::toggled, this, [this](bool on) {
        m_shareAudioBox->setText(on ? tr("On") : tr("Off"));
    });
    auto* shareLabel = makeSectionLabel(tr("Stream Audio"));

    // Two-state Local / Network control. The compact icon button lives on the
    // metro row (see SonicPiMetro); this fuller labelled pill lives here in the
    // expanded panel. Both drive supersonic/networkVisibility and stay in sync;
    // the selected half fills pink when on-network (public).
    m_visibilityToggle = new LinkVisibilityToggle(this);
    connect(m_visibilityToggle, &LinkVisibilityToggle::toggled,
            this, [this](bool isNet) {
                emit requestNetworkVisibilityChange(isNet ? 2 : 1);
                if (!m_linkEnabled) flashEmptyMessage();
            });

    // Match Share Audio's height to the visibility toggle. The latency
    // control sizes naturally: macOS's native QSlider needs more vertical
    // room than 25dx to render its track/thumb.
    const int rowH = m_visibilityToggle->sizeHint().height();
    m_shareAudioBox->setFixedHeight(rowH);

    // Two-row grid: titles on row 0 (all at the same height) and controls on
    // row 1, each vertically centred so a shorter control (e.g. the latency
    // slider) doesn't drag its title down. A trailing stretch column left-pins
    // the lot so they never drift right within the capped width.
    auto* idGrid = new QGridLayout;
    idGrid->setContentsMargins(0, 0, 0, 0);
    idGrid->setHorizontalSpacing(20);
    idGrid->setVerticalSpacing(2);
    int gc = 0;
    auto addCol = [&](QLabel* label, QWidget* control) {
        idGrid->addWidget(label, 0, gc, Qt::AlignHCenter | Qt::AlignBottom);
        idGrid->addWidget(control, 1, gc, Qt::AlignHCenter | Qt::AlignVCenter);
        ++gc;
    };
    addCol(nameLabel, m_peerNameEdit);
    addCol(latLabel, latencyControl);
    addCol(shareLabel, m_shareAudioBox);
    addCol(makeSectionLabel(tr("Visibility")), m_visibilityToggle);
    // Distribute the columns across the full width (rather than a trailing
    // stretch) so the identity row spans the same width as the peer table and
    // the metro row below — Link Name at the left, Visibility at the right edge.
    for (int c = 0; c < gc; ++c) idGrid->setColumnStretch(c, 1);
    layout->addLayout(idGrid);
    m_idGrid = idGrid;
    // Extra gap to separate the identity controls from the peer table
    // (panel row-spacing is only 2dx).
    layout->addSpacing(10);

    connect(m_latencySlider, &QSlider::valueChanged,
            this, &LinkAudioStreamsWidget::onLatencySliderChanged);

    m_peersTable = new QTableWidget(0, 6, this);
    // First column header is mode-specific ("Local Peer" / "Network Peer")
    // — updated in renderPeersTable.
    m_peersTable->setHorizontalHeaderLabels({
        tr("Peer"), tr("Channel"), tr("Status"),
        tr("Buffered"), tr("Rate"), tr("Bus") });
    m_peersTable->verticalHeader()->setVisible(false);
    // Tight row pitch; Qt's platform default (~30dx) leaves the table sparse.
    m_peersTable->verticalHeader()->setDefaultSectionSize(ScaleHeightForDPI(20));
    m_peersTable->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
    m_peersTable->setSelectionMode(QAbstractItemView::SingleSelection);
    m_peersTable->setSelectionBehavior(QAbstractItemView::SelectRows);
    m_peersTable->setEditTriggers(QAbstractItemView::NoEditTriggers);
    m_peersTable->setFocusPolicy(Qt::StrongFocus);
    m_peersTable->setAccessibleName(tr("Link Audio peers"));
    m_peersTable->setObjectName("linkPeersTable");
    m_peersTable->setShowGrid(false);
    m_peersTable->setAlternatingRowColors(true);
    // Colour / padding driven by app.qss.
    auto* hdr = m_peersTable->horizontalHeader();
    hdr->setSectionResizeMode(0, QHeaderView::Stretch);
    hdr->setSectionResizeMode(1, QHeaderView::Stretch);
    hdr->setSectionResizeMode(2, QHeaderView::Fixed);
    hdr->setSectionResizeMode(3, QHeaderView::Fixed);
    hdr->setSectionResizeMode(4, QHeaderView::Fixed);
    hdr->setSectionResizeMode(5, QHeaderView::Fixed);
    m_peersTable->setColumnWidth(2, 110);  // "Connected" / "Available"
    m_peersTable->setColumnWidth(3, 90);   // "123.4 ms"
    m_peersTable->setColumnWidth(4, 80);   // "48.0 kHz"
    m_peersTable->setColumnWidth(5, 60);   // "14" (bus index)

    // Open tall enough on first show to display the header plus content (or the
    // centred empty-state message). Without a floor, the 0-row table collapses
    // to roughly the header height and clips the first row before any peers or
    // the empty-state message have populated it.
    m_peersTable->setMinimumHeight(
        m_peersTable->horizontalHeader()->sizeHint().height() + ScaleHeightForDPI(80));

    layout->addWidget(m_peersTable);
    // Matching gap below the table for symmetric padding.
    layout->addSpacing(10);

    connect(m_peerNameEdit, &QLineEdit::editingFinished,
            this, &LinkAudioStreamsWidget::onPeerNameEdited);
    connect(m_shareAudioBox, &QPushButton::toggled,
            this, &LinkAudioStreamsWidget::onShareAudioToggled);

    m_socket = new QUdpSocket(this);
    m_socket->bind(QHostAddress::LocalHost, 0);
    connect(m_socket, &QUdpSocket::readyRead,
            this, &LinkAudioStreamsWidget::readPendingDatagrams);

    m_pollTimer = new QTimer(this);
    m_pollTimer->setInterval(2000);
    connect(m_pollTimer, &QTimer::timeout, this, &LinkAudioStreamsWidget::refresh);

    // Push the loaded publish flag so SuperSonic starts matching the UI.
    if (m_spAPI) m_spAPI->SetLinkAudioPublish(m_shareAudioBox->isChecked());

    // Initial visibility from QSettings (Local default); Link is session-only,
    // always off on launch.
    applyMasterVisibility(
        QSettings().value("supersonic/networkVisibility", 1).toInt());
    applyLinkEnabled(false);
}

void LinkAudioStreamsWidget::setVisibilityColors(const QColor& thumb,
                                                 const QColor& activeIcon)
{
    if (!m_visibilityToggle) return;
    m_visibilityToggle->setAccent(thumb, activeIcon);
}

void LinkAudioStreamsWidget::applyMasterVisibility(int mode)
{
    if (!m_visibilityToggle) return;
    m_currentVisibility = (mode == 2) ? 2 : 1;
    QSignalBlocker block(m_visibilityToggle);
    m_visibilityToggle->setNetwork(m_currentVisibility == 2);
    renderPeersTable();
}

void LinkAudioStreamsWidget::applyLinkEnabled(bool enabled)
{
    m_linkEnabled = enabled;
    // Mute visually when Link is off; controls still respond to clicks so
    // users can pre-set their choice. Muted styling lives in app.qss, so
    // just toggle the dynamic property and re-polish.
    if (m_visibilityToggle) m_visibilityToggle->setMuted(!enabled);
    auto repolish = [](QWidget* w) {
        w->style()->unpolish(w);
        w->style()->polish(w);
        w->update();
    };
    if (m_shareAudioBox) {
        m_shareAudioBox->setProperty("muted", !enabled);
        repolish(m_shareAudioBox);
    }
    if (m_peerNameEdit) {
        // Border goes pink when Link is engaged (see app.qss).
        m_peerNameEdit->setProperty("linkOn", enabled);
        repolish(m_peerNameEdit);
    }
    if (m_peersTable) {
        // Drives the header-section colour swap in app.qss (Link-state chip).
        // The header is a separate styled widget, so re-polish it too.
        m_peersTable->setProperty("linkOn", enabled);
        repolish(m_peersTable);
        if (auto* hdr = m_peersTable->horizontalHeader()) repolish(hdr);
    }
    renderPeersTable();
}

void LinkAudioStreamsWidget::onPeerNameEdited()
{
    const QString name = m_peerNameEdit->text().trimmed();
    if (name.isEmpty()) return;
    QSettings().setValue("link/peerName", name);
    if (m_spAPI) m_spAPI->SetLinkPeerName(name.toStdString());
}

void LinkAudioStreamsWidget::onShareAudioToggled(bool checked)
{
    QSettings().setValue("link/audioPublish", checked);
    if (m_spAPI) m_spAPI->SetLinkAudioPublish(checked);
    if (!m_linkEnabled) flashEmptyMessage();
}

void LinkAudioStreamsWidget::flashEmptyMessage()
{
    // Briefly tint the empty-state row to the highlight colour. Colour
    // only (no weight change) so the row doesn't jump.
    if (m_peersTable->rowCount() != 1) return;
    auto* it = m_peersTable->item(0, 0);
    if (!it) return;
    const QColor orig = it->foreground().color();
    it->setForeground(QBrush(palette().color(QPalette::Highlight)));
    QTimer::singleShot(450, this, [this, orig]() {
        if (m_peersTable->rowCount() != 1) return;
        auto* it2 = m_peersTable->item(0, 0);
        if (!it2) return;
        it2->setForeground(QBrush(orig));
    });
}

LinkAudioStreamsWidget::~LinkAudioStreamsWidget() = default;

int LinkAudioStreamsWidget::controlsNaturalWidth() const
{
    return m_idGrid ? m_idGrid->sizeHint().width() : 0;
}

void LinkAudioStreamsWidget::refresh()
{
    if (!m_spAPI) return;
    const int port = m_spAPI->GetPort(SonicPiPortId::scsynth);
    if (port <= 0) {
        showEmptyMessage(tr("SuperSonic not connected"));
        return;
    }
    // Two queries per poll: announced channel list, and active
    // subscriptions (carries per-input status + latency).
    m_socket->writeDatagram(buildOscRequest("/clock/audio/channels/get"),
                            QHostAddress::LocalHost, static_cast<quint16>(port));
    m_socket->writeDatagram(buildOscRequest("/clock/audio/inputs/get"),
                            QHostAddress::LocalHost, static_cast<quint16>(port));
}

void LinkAudioStreamsWidget::readPendingDatagrams()
{
    while (m_socket->hasPendingDatagrams()) {
        QByteArray buf;
        buf.resize(static_cast<int>(m_socket->pendingDatagramSize()));
        m_socket->readDatagram(buf.data(), buf.size());

        // Dispatch by reply address (two outstanding queries).
        OscReader peek{buf};
        QString addr;
        if (!peek.readStr(addr)) continue;

        if (addr == "/clock/audio/channels.reply") {
            QVector<PeerChannel> incoming = parseChannelsReply(buf);
            if (incoming != m_channels) {
                m_channels = incoming;
                // Surface deduped peer / channel names to the editor autocompletion.
                QStringList peers, channels;
                for (const PeerChannel& pc : m_channels) {
                    const QString p = QStringLiteral("\"%1\"").arg(pc.peerName);
                    const QString c = QStringLiteral("\"%1\"").arg(pc.channelName);
                    if (!peers.contains(p)) peers << p;
                    if (!channels.contains(c)) channels << c;
                }
                emit linkAudioStreamsChanged(peers, channels);
            }
            renderPeersTable();
        } else if (addr == "/clock/audio/inputs.reply") {
            m_inputs = parseInputsReply(buf);
            // Slider is the source of truth; pull any drifted input back
            // (e.g. a stream added by Ruby at SuperSonic's default).
            enforceEngineLatency();
            renderPeersTable();
        }
    }
}

void LinkAudioStreamsWidget::onLatencySliderChanged(int ms)
{
    if (m_latencyValueLabel) {
        m_latencyValueLabel->setText(tr("%1 ms").arg(ms));
    }
    QSettings().setValue("link/audioLatencyMs", ms);
    // Apply now rather than waiting for the next poll.
    enforceEngineLatency();
}

void LinkAudioStreamsWidget::enforceEngineLatency()
{
    if (!m_spAPI || !m_latencySlider) return;
    const int port = m_spAPI->GetPort(SonicPiPortId::scsynth);
    if (port <= 0) return;
    const float target = m_latencySlider->value() / 1000.0f;
    for (const auto& in : m_inputs) {
        // ~1 ms deadband; avoids float round-trips re-pushing every poll.
        if (std::fabs(in.latencySeconds - target) > 0.001f) {
            m_socket->writeDatagram(
                buildOscInputLatencyRequest("/clock/audio/input/latency/set",
                                            in.peerName, in.channelName, target),
                QHostAddress::LocalHost, static_cast<quint16>(port));
        }
    }
}

void LinkAudioStreamsWidget::renderPeersTable()
{
    // Selection is positional; remember its peer/channel identity so rebuilds
    // (2s polling) don't silently move a keyboard user's selection.
    QString selPeer, selChannel;
    const int selRow = m_peersTable->currentRow();
    if (selRow >= 0) {
        if (auto* keyItem = m_peersTable->item(selRow, 0)) {
            selPeer = keyItem->data(Qt::UserRole).toString();
            selChannel = keyItem->data(Qt::UserRole + 1).toString();
        }
    }

    const bool isNet = (m_currentVisibility == 2);
    // Header reflects the current scope.
    m_peersTable->setHorizontalHeaderLabels({
        isNet ? tr("Network Peer") : tr("Local Peer"),
        tr("Channel"), tr("Status"), tr("Buffered"), tr("Rate"), tr("Bus") });

    // Float active subscriptions to the top; within each group the
    // peer-then-channel sort from parseChannelsReply is preserved.
    auto findInput = [this](const PeerChannel& ch) -> const InputStatus* {
        for (const auto& in : m_inputs) {
            if (in.peerName == ch.peerName && in.channelName == ch.channelName)
                return &in;
        }
        return nullptr;
    };
    QVector<PeerChannel> shown;
    shown.reserve(m_channels.size());
    for (const auto& ch : m_channels) {
        if (findInput(ch)) shown.push_back(ch);
    }
    for (const auto& ch : m_channels) {
        if (!findInput(ch)) shown.push_back(ch);
    }

    if (shown.isEmpty()) {
        const QString scope = isNet ? tr("on the network")
                                    : tr("on this machine");
        const QString msg = !m_linkEnabled
            ? tr("Click Link to find other peers %1").arg(scope)
            : tr("Waiting for Link Audio peers %1…").arg(scope);
        showEmptyMessage(msg);
        return;
    }
    m_peersTable->clearSpans();
    m_peersTable->setRowCount(shown.size());
    const int rowH = ScaleHeightForDPI(18);
    QString prevPeer;
    for (int i = 0; i < shown.size(); ++i) {
        m_peersTable->setRowHeight(i, rowH);
        const PeerChannel& ch = shown[i];
        const InputStatus* in = findInput(ch);
        const bool isSub = (in != nullptr);
        // Non-subscribed channels are always "Available".
        QString status = tr("Available");
        if (isSub) {
            switch (in->state) {
            case CS_Connecting: status = tr("Connecting…"); break;
            case CS_Connected:  status = tr("Connected");   break;
            case CS_Dropout:    status = tr("Dropout");     break;
            default:            status = tr("Subscribed");  break;
            }
        }
        const bool showMetrics = isSub
            && (in->state == CS_Connected || in->state == CS_Dropout);
        const QString buffered = showMetrics
            ? tr("%1 ms").arg(in->bufferedMs, 0, 'f', 1)
            : QStringLiteral("—");
        const QString rate     = (showMetrics && in->sampleRate > 0)
            ? tr("%1 kHz").arg(in->sampleRate / 1000.0, 0, 'f', 1)
            : QStringLiteral("—");
        const QString bus = (isSub && in->busIdx >= 0)
            ? QString::number(in->busIdx)
            : QStringLiteral("—");

        // Bold active subscriptions to reinforce the connected-at-top sort.
        auto set = [&](int col, const QString& text, Qt::Alignment align = Qt::AlignLeft | Qt::AlignVCenter) {
            auto* it = new QTableWidgetItem(text);
            it->setFlags(it->flags() & ~Qt::ItemIsEditable);
            it->setTextAlignment(align);
            if (isSub) {
                QFont f = it->font();
                f.setBold(true);
                it->setFont(f);
            }
            m_peersTable->setItem(i, col, it);
        };
        // Peer name only on its first row, grouping its channels under one label.
        set(0, ch.peerName == prevPeer ? QString() : ch.peerName);
        m_peersTable->item(i, 0)->setData(Qt::UserRole, ch.peerName);
        m_peersTable->item(i, 0)->setData(Qt::UserRole + 1, ch.channelName);
        set(1, ch.channelName);
        set(2, status, Qt::AlignHCenter | Qt::AlignVCenter);
        set(3, buffered, Qt::AlignHCenter | Qt::AlignVCenter);
        set(4, rate, Qt::AlignHCenter | Qt::AlignVCenter);
        set(5, bus, Qt::AlignHCenter | Qt::AlignVCenter);
        prevPeer = ch.peerName;
    }

    if (!selChannel.isEmpty()) {
        for (int i = 0; i < shown.size(); ++i) {
            if (shown[i].peerName == selPeer && shown[i].channelName == selChannel) {
                m_peersTable->selectRow(i);
                break;
            }
        }
    }
}

void LinkAudioStreamsWidget::showEmptyMessage(const QString& text)
{
    m_peersTable->setRowCount(1);
    m_peersTable->setSpan(0, 0, 1, m_peersTable->columnCount());
    auto* it = new QTableWidgetItem(text);
    it->setFlags(it->flags() & ~Qt::ItemIsEditable);
    it->setTextAlignment(Qt::AlignCenter);
    // Italic to signal empty/status state.
    QFont f = it->font();
    f.setItalic(true);
    it->setFont(f);
    m_peersTable->setItem(0, 0, it);
    // Stretch the row to fill the viewport so the text sits mid-table.
    const int viewportH = m_peersTable->viewport()->height();
    const int rowH = std::max(ScaleHeightForDPI(80), viewportH);
    m_peersTable->setRowHeight(0, rowH);
}

void LinkAudioStreamsWidget::showEvent(QShowEvent* e)
{
    QWidget::showEvent(e);
    if (m_pollTimer && !m_pollTimer->isActive()) {
        m_pollTimer->start();
        refresh();
    }
}

void LinkAudioStreamsWidget::hideEvent(QHideEvent* e)
{
    QWidget::hideEvent(e);
    if (m_pollTimer) m_pollTimer->stop();
}
