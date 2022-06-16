#include <iostream>

#include "qt_api_client.h"
#include "config.h"
#include "mainwindow.h"
#include "sonicpitheme.h"

#include "sonicpilog.h"

namespace SonicPi
{

QtAPIClient::QtAPIClient(MainWindow* pMainWindow)
    : m_pMainWindow(pMainWindow)
{
    last_incoming_path_lens.fill(0);
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
        m_pMainWindow->setLineMarkerinCurrentWorkspace(info.line);
        m_pMainWindow->showError("<h2 class=\"error_description\"><pre>Runtime Error: " + QString::fromStdString(info.text) + "</pre></h2><pre class=\"backtrace\">" + QString::fromStdString(info.backtrace) + "</pre>");
    }
    else if (info.type == MessageType::SyntaxError)
    {
        m_pMainWindow->setLineMarkerinCurrentWorkspace(info.line);

        QString html_response = "<h2 class=\"syntax_error_description\"><pre>Syntax Error: " + QString::fromStdString(info.text) + "</pre></h2><pre class=\"error_msg\">";
        if (info.line == -1)
        {
            html_response = html_response + "</span></pre>";
        }
        else
        {
            html_response = html_response + "[Line " + QString::fromStdString(info.lineNumString) + "]: <span class=\"error_line\">" + QString::fromStdString(info.errorLineString) + "</span></pre>";
        }
        m_pMainWindow->showError(html_response);
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

void QtAPIClient::AudioDataAvailable(const ProcessedAudio& audio)
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

} // namespace SonicPi
