#include "phxurlinterceptor.h"
#include <iostream>
#include <QWebEngineUrlRequestInterceptor>


void PhxUrlInterceptor::interceptRequest(QWebEngineUrlRequestInfo &info)
{

    if(info.requestUrl().host().toStdString() != "localhost") {
        std::cout << "[GUI - blocked phx access to non-localhost URL: " << info.requestUrl().url().toStdString() << std::endl;
        info.block(true);
    }
}

