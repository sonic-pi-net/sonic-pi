#pragma once

#include "osc_handler.h"
#include "osc_server.h"

#if 0
namespace SonicPi
{

class OscServerTCP : public OscServer
{
public:
    explicit OscServerTCP(IAPIClient* pClient, std::shared_ptr<OscHandler> spHandler, int port);

    uint32_t blockSize;

public slots:
    void stop();
    void start();
    void readMessage();
    void client();
    void logError(QAbstractSocket::SocketError);

private:
    void handleMessage();

    QTcpServer* tcpServer;
    QTcpSocket* socket;
    std::vector<char> buffer;
};

#endif

} // namespace SonicPi
