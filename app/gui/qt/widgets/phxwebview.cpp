//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2021 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include <QWebEngineView>
#include <QWebEngineScript>
#include <QWebEngineScriptCollection>
#include "phxwebview.h"
#include "phxurlinterceptor.h"


PhxWebView::PhxWebView(QWidget *parent)
    : QWebEngineView(parent)
{

    PhxUrlInterceptor *interceptor = new PhxUrlInterceptor;
    phxProfile = new QWebEngineProfile();
    phxProfile->setUrlRequestInterceptor(interceptor);
    phxPage = new QWebEnginePage(phxProfile);
    phxPage->setParent(this);
    phxProfile->setParent(this);
    setPage(phxPage);
    setContextMenuPolicy(Qt::NoContextMenu);
    setZoomFactor(2.0);

    setAttribute(Qt::WA_TranslucentBackground);
    setStyleSheet("background:transparent");
    setScrollbarColours("#5e5e5e", "black", "#1e90ff");

}

void PhxWebView::setScrollbarColours(QColor foreground, QColor background, QColor hover)
{
    insertStyleSheet("scrollbar",
    QString("/* Width */"
            "::-webkit-scrollbar {"
            "  width: 5px;"
            "}"
            "/* Track */"
            "::-webkit-scrollbar-track {"
            "    background: %1;"
            "}"
            "/* Thumb */"
            "::-webkit-scrollbar-thumb {"
            "  border-radius: 8px;"
            "  background: %2;"
            "}"
            "/* Thumb on hover */"
            "::-webkit-scrollbar-thumb:hover {"
            "  background: %3;"
            "}"
        ).arg(background.name()).arg(foreground.name()).arg(hover.name()));

    }

void PhxWebView::insertStyleSheet(const QString& name, const QString& source)
    {
        QWebEngineScript script;
        QString s = QString::fromLatin1("(function() {"\
            "    css = document.createElement('style');"\
            "    css.type = 'text/css';"\
            "    css.id = '%1';"\
            "    document.head.appendChild(css);"\
            "    css.innerText = '%2';"\
            "})()").arg(name).arg(source.simplified());

        this->page()->runJavaScript(s, QWebEngineScript::ApplicationWorld);

        script.setName(name);
        script.setSourceCode(s);
        script.setInjectionPoint(QWebEngineScript::DocumentReady);
        script.setRunsOnSubFrames(true);
        script.setWorldId(QWebEngineScript::ApplicationWorld);
        this->page()->scripts().insert(script);
    }



