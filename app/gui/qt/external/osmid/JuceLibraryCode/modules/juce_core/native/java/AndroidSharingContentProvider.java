package com.juce;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.res.AssetFileDescriptor;
import android.content.res.Resources;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.os.FileObserver;
import android.os.ParcelFileDescriptor;
import java.lang.String;

public final class SharingContentProvider extends ContentProvider
{
    private Object lock = new Object();

    private native void contentSharerFileObserverEvent (long host, int event, String path);

    private native Cursor contentSharerQuery (Uri uri, String[] projection, String selection,
                                              String[] selectionArgs, String sortOrder);

    private native void contentSharerCursorClosed (long host);

    private native AssetFileDescriptor contentSharerOpenFile (Uri uri, String mode);
    private native String[] contentSharerGetStreamTypes (Uri uri, String mimeTypeFilter);

    public final class ProviderFileObserver extends FileObserver
    {
        public ProviderFileObserver (long hostToUse, String path, int mask)
        {
            super (path, mask);

            host = hostToUse;
        }

        public void onEvent (int event, String path)
        {
            contentSharerFileObserverEvent (host, event, path);
        }

        private long host;
    }

    public final class ProviderCursor extends MatrixCursor
    {
        ProviderCursor (long hostToUse, String[] columnNames)
        {
            super (columnNames);

            host = hostToUse;
        }

        @Override
        public void close()
        {
            super.close();

            contentSharerCursorClosed (host);
        }

        private long host;
    }

    @Override
    public boolean onCreate()
    {
        return true;
    }

    @Override
    public Cursor query (Uri url, String[] projection, String selection,
                         String[] selectionArgs, String sortOrder)
    {
        synchronized (lock)
        {
            return contentSharerQuery (url, projection, selection, selectionArgs, sortOrder);
        }
    }

    @Override
    public Uri insert (Uri uri, ContentValues values)
    {
        return null;
    }

    @Override
    public int update (Uri uri, ContentValues values, String selection,
                       String[] selectionArgs)
    {
        return 0;
    }

    @Override
    public int delete (Uri uri, String selection, String[] selectionArgs)
    {
        return 0;
    }

    @Override
    public String getType (Uri uri)
    {
        return null;
    }

    @Override
    public AssetFileDescriptor openAssetFile (Uri uri, String mode)
    {
        synchronized (lock)
        {
            return contentSharerOpenFile (uri, mode);
        }
    }

    @Override
    public ParcelFileDescriptor openFile (Uri uri, String mode)
    {
        synchronized (lock)
        {
            AssetFileDescriptor result = contentSharerOpenFile (uri, mode);

            if (result != null)
                return result.getParcelFileDescriptor();

            return null;
        }
    }
$$ContentProviderApi11
    @Override
    public String[] getStreamTypes (Uri uri, String mimeTypeFilter)
    {
        synchronized (lock)
        {
            return contentSharerGetStreamTypes (uri, mimeTypeFilter);
        }
    }
ContentProviderApi11$$
}
