#ifndef PHXURLINTERCEPTOR_H
#define PHXURLINTERCEPTOR_H

#include <QObject>
#include <QWebEngineUrlRequestInterceptor>
#include <QWebEngineUrlRequestInfo>
#include <QtWebEngineCore/qwebengineurlrequestinterceptor.h>
#include <QDebug>


class PhxUrlInterceptor : public QWebEngineUrlRequestInterceptor
{
    Q_OBJECT
public:
    PhxUrlInterceptor(QObject *parent = nullptr) : QWebEngineUrlRequestInterceptor(parent)
    {
    }

    void interceptRequest(QWebEngineUrlRequestInfo &info);
};

#endif // PHXURLINTERCEPTOR_H