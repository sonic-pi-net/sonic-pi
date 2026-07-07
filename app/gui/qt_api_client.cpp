#include <iostream>

#include "qt_api_client.h"
#include "config.h"
#include "mainwindow.h"
#include "sonicpitheme.h"

#include "sonicpilog.h"

#include <QString>
#include <QRegularExpression>

namespace SonicPi
{

// A one-line code preview of the offending source line with the [cs,ce) byte
// span wrapped in markClass. The raw line is split before escaping so the byte
// columns from Prism/error_highlight line up with the (ASCII) source.
static QString buildErrorSnippet(const QString& rawLine, int cs, int ce, const QString& markClass)
{
    QString line = rawLine;
    while (line.endsWith('\n') || line.endsWith('\r'))
        line.chop(1);
    if (line.trimmed().isEmpty())
        return QString();

    QString inner;
    if (cs >= 0 && ce > cs && cs < line.length())
    {
        int a = qBound(0, cs, line.length());
        int b = qBound(a, ce, line.length());
        inner = line.left(a).toHtmlEscaped()
              + "<span class=\"" + markClass + "\">" + line.mid(a, b - a).toHtmlEscaped() + "</span>"
              + line.mid(b).toHtmlEscaped();
    }
    else
    {
        inner = line.toHtmlEscaped();
    }
    return "<pre class=\"error_snippet\">" + inner + "</pre>";
}

QtAPIClient::QtAPIClient(MainWindow* pMainWindow)
    : m_pMainWindow(pMainWindow)
{
    last_incoming_path_lens.fill(0);
    qRegisterMetaType<SonicPi::AudioDevicesInfo>("SonicPi::AudioDevicesInfo");
    qRegisterMetaType<SonicPi::AudioInputDevicesInfo>("SonicPi::AudioInputDevicesInfo");
    qRegisterMetaType<SonicPi::AudioDeviceConfigInfo>("SonicPi::AudioDeviceConfigInfo");
    qRegisterMetaType<SonicPi::AudioSwitchOutcome>("SonicPi::AudioSwitchOutcome");
}

QtAPIClient::~QtAPIClient()
{
}

void QtAPIClient::ReportGui(const MessageInfo& info)
{
    if (info.type == MessageType::Multi)
    {
        // TODO: No longer need to do this translation; just pass info to log
        SonicPiLog::MultiMessage mm;
        mm.theme = m_pMainWindow->GetTheme();
        mm.job_id = info.jobId;
        mm.thread_name = info.threadName;
        mm.runtime = info.runtime;
        for (auto& msg : info.multi)
        {
            SonicPiLog::Message message;
            message.msg_type = msg.style;
            message.s = msg.text;
            mm.messages.push_back(message);
        }
        m_pMainWindow->GetOutputPane()->handleMultiMessage(mm);
    }
    else if (info.type == MessageType::Info)
    {
        auto pOutput = m_pMainWindow->GetOutputPane();
        auto pTheme = m_pMainWindow->GetTheme();

        if (info.style == 1)
        {
            pOutput->setTextBgFgColors(pTheme->color("LogInfoBackground_1"), pTheme->color("LogInfoForeground_1"));
        }
        else
        {
            pOutput->setTextBgFgColors(pTheme->color("LogInfoBackground"), pTheme->color("LogInfoForeground"));
        }

        pOutput->appendPlainText(QString::fromStdString("=> " + info.text + "\n"));

        pOutput->setTextColor(pTheme->color("LogForeground"));
        pOutput->setTextBackgroundColor(pTheme->color("LogBackground"));
    }
    else if (info.type == MessageType::RuntimeError)
    {
        // error_highlight gives the offending token's exact byte-column span; if
        // it's unavailable, fall back to the identifier named in the message
        // (e.g. undefined method 'pl9ay').
        QString full = QString::fromStdString(info.text);
        static const QRegularExpression reErrTok("'([^']+)'");
        QString errToken;
        QRegularExpressionMatch tm = reErrTok.match(full);
        if (tm.hasMatch())
            errToken = tm.captured(1);
        m_pMainWindow->setLineMarkerinCurrentWorkspace(info.line, false, errToken, info.errorColStart, info.errorColEnd);

        // Present the error as one card: a coloured header (friendly message)
        // over a panel body holding the muted caption, the details toggle and
        // (when expanded) the full backtrace.
        int sep = full.indexOf("\n\n");
        QString headerText = (sep >= 0) ? full.left(sep) : ("Runtime Error: " + full);
        QString caption;
        if (sep >= 0)
        {
            // detail = "location\nexact-error"; the location gets its own style.
            QString det = full.mid(sep + 2);
            int nl = det.indexOf('\n');
            QString loc = (nl >= 0) ? det.left(nl) : det;
            QString rawPart = (nl >= 0) ? det.mid(nl + 1) : QString();
            caption = "<pre class=\"error_detail\"><span class=\"error_loc\">" + loc + "</span>";
            if (!rawPart.isEmpty())
                caption += " &#183; " + rawPart;
            caption += "</pre>";
        }
        QString header = "<tr><td class=\"error_description\"><pre class=\"error_text\">" + headerText + "</pre></td></tr>";
        QString bt = QString::fromStdString(info.backtrace);
        QString snippet = buildErrorSnippet(QString::fromStdString(info.errorLineString), info.errorColStart, info.errorColEnd, "error_mark");

        auto card = [&](const QString& bodyExtra) -> QString {
            QString inner = snippet + caption + bodyExtra;
            QString body = inner.isEmpty()
                ? QString()
                : ("<tr><td class=\"error_body\">" + inner + "</td></tr>");
            return "<table width=\"100%\" cellspacing=\"0\" class=\"error_card\">" + header + body + "</table>";
        };

        if (bt.isEmpty())
        {
            m_pMainWindow->showError(card(QString()));
        }
        else
        {
            QString collapsed = card("<div class=\"error_toggle\"><a href=\"sonicpi:toggle\">Show details</a></div>");
            QString expanded = card("<div class=\"error_toggle\"><a href=\"sonicpi:toggle\">Hide details</a></div><pre class=\"backtrace\">" + bt + "</pre>");
            m_pMainWindow->showToggleableError(collapsed, expanded);
        }
    }
    else if (info.type == MessageType::SyntaxError)
    {
        m_pMainWindow->setLineMarkerinCurrentWorkspace(info.line, true, QString(), info.errorColStart, info.errorColEnd);

        // Same card as runtime errors, but with the blue syntax-error header.
        QString full = QString::fromStdString(info.text);
        int sep = full.indexOf("\n\n");
        QString headerText = (sep >= 0) ? full.left(sep) : ("Syntax Error: " + full);
        QString caption;
        if (sep >= 0)
        {
            QString det = full.mid(sep + 2);
            int nl = det.indexOf('\n');
            QString loc = (nl >= 0) ? det.left(nl) : det;
            QString rawPart = (nl >= 0) ? det.mid(nl + 1) : QString();
            caption = "<pre class=\"error_detail\"><span class=\"error_loc\">" + loc + "</span>";
            if (!rawPart.isEmpty())
                caption += " &#183; " + rawPart;
            caption += "</pre>";
        }
        QString header = "<tr><td class=\"syntax_error_description\"><pre class=\"error_text\">" + headerText + "</pre></td></tr>";
        QString snippet = buildErrorSnippet(QString::fromStdString(info.errorLineString), info.errorColStart, info.errorColEnd, "syntax_error_mark");
        QString inner = snippet + caption;
        QString body = inner.isEmpty() ? QString() : ("<tr><td class=\"error_body\">" + inner + "</td></tr>");
        m_pMainWindow->showError("<table width=\"100%\" cellspacing=\"0\" class=\"error_card\">" + header + body + "</table>");
    }
    else if (info.type == MessageType::StartupError)
    {
        std::cout << std::endl
                  << "[GUI] - Sonic Pi Server failed to start with this error message: " << std::endl;
        std::cout << "      > " << info.text << std::endl;
    }
    else if (info.type == MessageType::InfoText)
    {
        m_pMainWindow->setUpdateInfoText(QString::fromStdString(info.text));
    }
}

void QtAPIClient::CueGui(const CueInfo& cue)
{
    int max_path_len = 0;
    for (int i = 0; i < last_incoming_path_lens.size(); i++)
    {
        if (last_incoming_path_lens[i] > max_path_len)
        {
            max_path_len = last_incoming_path_lens[i];
        }
    }
    int len_diff = max_path_len - int(cue.address.length());
    len_diff = (len_diff < 10) ? len_diff : 0;
    len_diff = std::max(len_diff, 0);
    len_diff = len_diff + 1;
    int idmod = ((cue.id * 3) % 200);
    idmod = 155 + ((idmod < 100) ? idmod : 200 - idmod);

    QString qs_address = QString::fromStdString(cue.address);
    if (!qs_address.startsWith(":"))
    {
        auto bg = m_pMainWindow->GetTheme()->color("CuePathBackground");
        bg.setAlpha(idmod);

        auto pIncoming = m_pMainWindow->GetIncomingPane();
        auto pTheme = m_pMainWindow->GetTheme();
        pIncoming->setTextBgFgColors(bg, pTheme->color("CuePathForeground"));
        pIncoming->appendPlainText(QString(" ") + QString::fromStdString(cue.address));
        pIncoming->insertPlainText(QString::fromStdString(std::string(len_diff, ' ')));
        pIncoming->setTextBgFgColors(pTheme->color("LogBackground"), QColor(Qt::white));
        pIncoming->insertPlainText(QString::fromStdString(" "));
        bg = pTheme->color("CueDataBackground");
        bg.setAlpha(idmod);
        pIncoming->setTextBgFgColors(bg, pTheme->color("CueDataForeground"));
        pIncoming->insertPlainText(QString::fromStdString(cue.args));
        last_incoming_path_lens[cue.id % last_incoming_path_lens.size()] = int(cue.address.length());
    }
    m_pMainWindow->addCuePath(qs_address, QString::fromStdString(cue.args));
}

void QtAPIClient::StatusGui(const StatusInfo& info)
{
    if (info.type == StatusType::Exited)
    {
        // This informs us the server exited
        // Nothing to do here, handled by the osc_handler
    }
    else if (info.type == StatusType::AllComplete)
    {
        m_pMainWindow->allJobsCompleted();
    }
    else if (info.type == StatusType::Ack)
    {
        // This informs us the server started
        // Nothing to do here, handled by the osc_handler
    }
}

void QtAPIClient::MidiGui(const MidiInfo& info)
{
    if (info.type == MidiType::In)
    {
        m_pMainWindow->updateMIDIInPorts(QString::fromStdString(info.portInfo));
    }
    else
    {
        m_pMainWindow->updateMIDIOutPorts(QString::fromStdString(info.portInfo));
    }

}

void QtAPIClient::GamepadDevicesGui(const QString& devices)
{
    m_pMainWindow->updateGamepadDevices(devices);
}

void QtAPIClient::VersionGui(const VersionInfo& info)
{
    QDate date = QDate(info.lastCheckedYear, info.lastCheckedMonth, info.lastCheckedDay);

    m_pMainWindow->updateVersionNumber(QString::fromStdString(info.version), info.num, QString::fromStdString(info.latestVersion), info.latestVersionNum, date, QString::fromStdString(info.platform));
}

void QtAPIClient::BufferGui(const BufferInfo& info)
{
    if (info.type == BufferType::Replace)
    {
        m_pMainWindow->replaceBuffer(QString::fromStdString(info.id), QString::fromStdString(info.content), info.line, info.index, info.lineNumber);
        m_pMainWindow->loaded_workspaces = true; // it's now safe to save the buffers
    }
    else if (info.type == BufferType::ReplaceIndex)
    {
        m_pMainWindow->replaceBufferIdx(info.bufferIndex, QString::fromStdString(info.content), info.line, info.index, info.lineNumber);
    }
    else if (info.type == BufferType::ReplaceLines)
    {
        m_pMainWindow->replaceLines(QString::fromStdString(info.id), QString::fromStdString(info.content), info.startLine, info.finishLine, info.pointLine, info.pointIndex);
    }
    else if (info.type == BufferType::RunIndex)
    {
        m_pMainWindow->runBufferIdx(info.bufferIndex);
    }
}

void QtAPIClient::ScsynthGui(const ScsynthInfo& scsynthInfo)
{
  m_pMainWindow->updateScsynthInfo(QString::fromStdString(scsynthInfo.text));
}

void QtAPIClient::Report(const MessageInfo& info)
{
    QMetaObject::invokeMethod(this, "ReportGui", Qt::QueuedConnection, Q_ARG(SonicPi::MessageInfo, info));
}

void QtAPIClient::Cue(const CueInfo& cue)
{
    QMetaObject::invokeMethod(this, "CueGui", Qt::QueuedConnection, Q_ARG(SonicPi::CueInfo, cue));
}

void QtAPIClient::AudioDataAvailable(ProcessedAudioPtr audio)
{
    emit ConsumeAudioData(audio);
}

void QtAPIClient::Status(const StatusInfo& info)
{
    QMetaObject::invokeMethod(this, "StatusGui", Qt::QueuedConnection, Q_ARG(SonicPi::StatusInfo, info));
}

void QtAPIClient::Midi(const MidiInfo& info)
{
    QMetaObject::invokeMethod(this, "MidiGui", Qt::QueuedConnection, Q_ARG(SonicPi::MidiInfo, info));
}

void QtAPIClient::GamepadDevices(const std::string& devices)
{
    QMetaObject::invokeMethod(this, "GamepadDevicesGui", Qt::QueuedConnection,
                              Q_ARG(QString, QString::fromStdString(devices)));
}

void QtAPIClient::Version(const VersionInfo& info)
{
    QMetaObject::invokeMethod(this, "VersionGui", Qt::QueuedConnection, Q_ARG(SonicPi::VersionInfo, info));
}

void QtAPIClient::Buffer(const BufferInfo& info)
{
    QMetaObject::invokeMethod(this, "BufferGui", Qt::QueuedConnection, Q_ARG(SonicPi::BufferInfo, info));
}

void QtAPIClient::ActiveLinks(const int numLinks)
{
  emit UpdateNumActiveLinks(numLinks);
}

void QtAPIClient::BPM(const double bpm)
{
  emit UpdateBPM(bpm);
}

void QtAPIClient::Scsynth(const ScsynthInfo& scsynthInfo)
{
  QMetaObject::invokeMethod(this, "ScsynthGui", Qt::QueuedConnection, Q_ARG(SonicPi::ScsynthInfo, scsynthInfo));
}

void QtAPIClient::AudioDevices(const AudioDevicesInfo& devicesInfo)
{
  QMetaObject::invokeMethod(this, "AudioDevicesGui", Qt::QueuedConnection, Q_ARG(SonicPi::AudioDevicesInfo, devicesInfo));
}

void QtAPIClient::AudioDeviceConfig(const AudioDeviceConfigInfo& configInfo)
{
  QMetaObject::invokeMethod(this, "AudioDeviceConfigGui", Qt::QueuedConnection, Q_ARG(SonicPi::AudioDeviceConfigInfo, configInfo));
}

void QtAPIClient::AudioDevicesGui(const AudioDevicesInfo& devicesInfo)
{
  m_pMainWindow->updateAudioDevices(devicesInfo);
}

void QtAPIClient::AudioInputDevices(const AudioInputDevicesInfo& devicesInfo)
{
  QMetaObject::invokeMethod(this, "AudioInputDevicesGui", Qt::QueuedConnection, Q_ARG(SonicPi::AudioInputDevicesInfo, devicesInfo));
}

void QtAPIClient::AudioInputDevicesGui(const AudioInputDevicesInfo& devicesInfo)
{
  m_pMainWindow->updateAudioInputDevices(devicesInfo);
}

void QtAPIClient::AudioDeviceConfigGui(const AudioDeviceConfigInfo& configInfo)
{
  m_pMainWindow->updateAudioDeviceConfig(configInfo);
}

void QtAPIClient::SupersonicSetup(int sampleRate, int bufferSize)
{
  emit SupersonicSetupReceived(sampleRate, bufferSize);
}

void QtAPIClient::SpiderReady()
{
  emit SpiderReadyReceived();
}

void QtAPIClient::AudioSwitchDone(const SonicPi::AudioSwitchOutcome& outcome)
{
  emit AudioSwitchDoneReceived(outcome);
}

} // namespace SonicPi
