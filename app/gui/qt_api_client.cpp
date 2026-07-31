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

// Splits the friendly error text ("header\n\nlocation\nreason") into its parts.
static void splitErrorText(const QString& full, const QString& fallbackPrefix,
                           QString& header, QString& location, QString& reason)
{
    int sep = full.indexOf("\n\n");
    header = (sep >= 0) ? full.left(sep) : (fallbackPrefix + full);
    if (sep < 0)
        return;
    QString det = full.mid(sep + 2);
    int nl = det.indexOf('\n');
    location = (nl >= 0) ? det.left(nl) : det;
    reason = (nl >= 0) ? det.mid(nl + 1) : QString();
}

QtAPIClient::QtAPIClient(MainWindow* pMainWindow)
    : m_pMainWindow(pMainWindow)
{
    last_incoming_path_lens.fill(0);
    qRegisterMetaType<SonicPi::AudioDevicesInfo>("SonicPi::AudioDevicesInfo");
    qRegisterMetaType<SonicPi::AudioInputDevicesInfo>("SonicPi::AudioInputDevicesInfo");
    qRegisterMetaType<SonicPi::AudioDeviceTableInfo>("SonicPi::AudioDeviceTableInfo");
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
            pOutput->setTextBgFgColorKeys(pTheme, "LogInfoBackground_1", "LogInfoForeground_1");
        }
        else
        {
            pOutput->setTextBgFgColorKeys(pTheme, "LogInfoBackground", "LogInfoForeground");
        }

        pOutput->appendPlainText(QString::fromStdString("=> " + info.text + "\n"));

        pOutput->setTextColorKey(pTheme, "LogForeground");
        pOutput->setTextBackgroundColorKey(pTheme, "LogBackground");
    }
    else if (info.type == MessageType::RuntimeError)
    {
        // Errors from help-system runs (playground/example/card workspaces)
        // must not mark lines in, or offer a jump into, the editor: their
        // line numbers belong to the generated snippet, not any buffer.
        const bool fromEditor = m_pMainWindow->jobRanFromEditor(info.jobId);

        QString full = QString::fromStdString(info.text);
        QString header, location, reason;
        splitErrorText(full, "Runtime Error: ", header, location, reason);

        // error_highlight gives the offending token's exact byte-column span; if
        // it's unavailable, fall back to the identifier: the backtick-marked
        // name in the friendly header, else the 'quoted' name in the raw reason
        // (the reason has no contraction apostrophes to confuse the match),
        // else the first :symbol in the header ("Value of opt :cutoff must...";
        // the whole-word search still finds it written as `cutoff:` in code).
        // Match against the header's first line only: the structured trailers
        // below it ("Example: ...", "Docs: ...") are full of backticks.
        QString errToken;
        static const QRegularExpression reBacktick("`([^`]+)`");
        static const QRegularExpression reQuoted("'([^']+)'");
        static const QRegularExpression reSymbol(":([A-Za-z_][A-Za-z0-9_]*)");
        const QString headLine = header.section(QLatin1Char('\n'), 0, 0);
        QRegularExpressionMatch tm = reBacktick.match(headLine);
        if (!tm.hasMatch())
            tm = reQuoted.match(reason);
        if (!tm.hasMatch())
            tm = reSymbol.match(headLine);
        if (tm.hasMatch())
            errToken = tm.captured(1);
        if (fromEditor)
            m_pMainWindow->setLineMarkerinCurrentWorkspace(info.line, false, errToken, info.errorColStart, info.errorColEnd);

        // The card draws its squiggle from [colStart,colEnd) alone; when
        // error_highlight gave no span, fall back to the identifier named in
        // the message, the same whole-word search the editor's underline uses
        // (byte-based, so multi-byte characters don't shift it).
        int colStart = info.errorColStart;
        int colEnd = info.errorColEnd;
        if ((colStart < 0 || colEnd <= colStart) && !errToken.isEmpty())
        {
            const QByteArray line = QByteArray::fromStdString(info.errorLineString);
            const QByteArray tok = errToken.toUtf8();
            auto isWord = [](char c) {
                return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                       || (c >= '0' && c <= '9') || c == '_';
            };
            for (int i = line.indexOf(tok); i >= 0; i = line.indexOf(tok, i + 1))
            {
                const bool okL = i == 0 || !isWord(line[i - 1]);
                const int after = i + tok.length();
                const bool okR = after >= line.size() || !isWord(line[after]);
                if (okL && okR)
                {
                    colStart = i;
                    colEnd = after;
                    break;
                }
            }
        }

        m_pMainWindow->showErrorCard(false, header, location, reason,
                                     QString::fromStdString(info.errorLineString), info.line,
                                     colStart, colEnd,
                                     QString::fromStdString(info.backtrace),
                                     fromEditor && info.line > 0);
    }
    else if (info.type == MessageType::SyntaxError)
    {
        const bool fromEditor = m_pMainWindow->jobRanFromEditor(info.jobId);
        if (fromEditor)
            m_pMainWindow->setLineMarkerinCurrentWorkspace(info.line, true, QString(), info.errorColStart, info.errorColEnd);

        QString header, location, reason;
        splitErrorText(QString::fromStdString(info.text), "Syntax Error: ", header, location, reason);
        // The parser's own message ("expected a block beginning with `do` to
        // end with `end`") becomes the headline, shedding its "SyntaxError:"
        // tag.
        QString msg = reason;
        static const QRegularExpression kSynTag(QStringLiteral("^\\s*SyntaxError:\\s*"));
        msg.remove(kSynTag);
        if (!msg.isEmpty())
        {
            msg[0] = msg[0].toUpper();
            header = QStringLiteral("Syntax Error ") + msg;
            reason.clear();
        }
        m_pMainWindow->showErrorCard(true, header, location, reason,
                                     QString::fromStdString(info.errorLineString), info.line,
                                     info.errorColStart, info.errorColEnd,
                                     QString(), fromEditor && info.line > 0);
    }
    else if (info.type == MessageType::StartupError)
    {
        std::cout << std::endl
                  << "[GUI] - Sonic Pi Server failed to start with this error message: " << std::endl;
        std::cout << "      > " << info.text << std::endl;
        // Keep the specific cause for the boot-error dialog shown when the
        // ready-poll gives up — see MainWindow::pollServerReady.
        m_startupErrorText = QString::fromStdString(info.text);
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
        auto pIncoming = m_pMainWindow->GetIncomingPane();
        auto pTheme = m_pMainWindow->GetTheme();
        // idmod is a per-cue alpha (id-striping); stored with the role so the
        // stripe survives a recolour.
        pIncoming->setTextBgFgColorKeys(pTheme, "CuePathBackground", "@contrast", idmod);
        pIncoming->appendPlainText(QString(" ") + QString::fromStdString(cue.address));
        pIncoming->insertPlainText(QString::fromStdString(std::string(len_diff, ' ')));
        pIncoming->setTextBgFgColorKeys(pTheme, "LogBackground", "LogForeground");
        pIncoming->insertPlainText(QString::fromStdString(" "));
        pIncoming->setTextBgFgColorKeys(pTheme, "CueDataBackground", "@contrast", idmod);
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

void QtAPIClient::MixerSettings(double drive, double outputVolume)
{
  emit MixerSettingsReceived(drive, outputVolume);
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

void QtAPIClient::AudioDeviceTable(const AudioDeviceTableInfo& table)
{
  QMetaObject::invokeMethod(this, "AudioDeviceTableGui", Qt::QueuedConnection, Q_ARG(SonicPi::AudioDeviceTableInfo, table));
}

void QtAPIClient::AudioDeviceTableGui(const AudioDeviceTableInfo& table)
{
  m_pMainWindow->updateAudioDeviceTable(table);
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

void QtAPIClient::AudioDeviceReopenReply(bool accepted, const std::string& reason)
{
  emit AudioDeviceReopenReplyReceived(accepted, QString::fromStdString(reason));
}

void QtAPIClient::RunStarted(int jobId, const std::string& workspace)
{
  emit RunStartedReceived(jobId, QString::fromStdString(workspace));
}

void QtAPIClient::RunEnded(int jobId)
{
  emit RunEndedReceived(jobId);
}

void QtAPIClient::Flash(const std::string& workspace, int line)
{
  emit FlashReceived(QString::fromStdString(workspace), line);
}

void QtAPIClient::LiveLoopScope(int jobId, const std::string& name, const std::string& workspace,
                                int line, int scopeNum)
{
  emit LiveLoopScopeReceived(jobId, QString::fromStdString(name),
                             QString::fromStdString(workspace), line, scopeNum);
}

void QtAPIClient::LiveLoopScopeEnded(int jobId, const std::string& name)
{
  emit LiveLoopScopeEndedReceived(jobId, QString::fromStdString(name));
}

} // namespace SonicPi
