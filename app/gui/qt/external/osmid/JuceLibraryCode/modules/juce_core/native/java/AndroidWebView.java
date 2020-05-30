$$WebViewNativeApi23    private native void webViewReceivedError (long host, WebView view, WebResourceRequest request, WebResourceError error);WebViewNativeApi23$$
$$WebViewNativeApi21    private native void webViewReceivedHttpError (long host, WebView view, WebResourceRequest request, WebResourceResponse errorResponse);WebViewNativeApi21$$

$$WebViewApi1_10
        @Override
        public void onPageStarted (WebView view, String url, Bitmap favicon)
        {
            if (host != 0)
                webViewPageLoadStarted (host, view, url);
        }
WebViewApi1_10$$

$$WebViewApi11_20
        @Override
        public WebResourceResponse shouldInterceptRequest (WebView view, String url)
        {
            synchronized (hostLock)
            {
                if (host != 0)
                {
                    boolean shouldLoad = webViewPageLoadStarted (host, view, url);

                    if (shouldLoad)
                        return null;
                }
            }

            return new WebResourceResponse ("text/html", null, null);
        }
WebViewApi11_20$$

$$WebViewApi21
        @Override
        public WebResourceResponse shouldInterceptRequest (WebView view, WebResourceRequest request)
        {
            synchronized (hostLock)
            {
                if (host != 0)
                {
                    boolean shouldLoad = webViewPageLoadStarted (host, view, request.getUrl().toString());

                    if (shouldLoad)
                        return null;
                }
            }

            return new WebResourceResponse ("text/html", null, null);
        }
WebViewApi21$$

$$WebViewApi23
        @Override
        public void onReceivedError (WebView view, WebResourceRequest request, WebResourceError error)
        {
            if (host == 0)
                return;

            webViewReceivedError (host, view, request, error);
        }

        @Override
        public void onReceivedHttpError (WebView view, WebResourceRequest request, WebResourceResponse errorResponse)
        {
            if (host == 0)
                return;

            webViewReceivedHttpError (host, view, request, errorResponse);
        }
WebViewApi23$$
