/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

package com.juce;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.os.Looper;
import android.os.Handler;
import android.os.ParcelUuid;
import android.os.Environment;
import android.view.*;
import android.view.inputmethod.BaseInputConnection;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputMethodManager;
import android.graphics.*;
import android.text.ClipboardManager;
import android.text.InputType;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.Pair;
import java.lang.Runnable;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.io.*;
import java.net.URL;
import java.net.HttpURLConnection;
import android.media.AudioManager;
import android.Manifest;
import java.util.concurrent.CancellationException;
import java.util.concurrent.Future;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.atomic.*;
$$JuceAndroidMidiImports$$         // If you get an error here, you need to re-save your project with the Projucer!


//==============================================================================
public class JuceAppActivity   extends Activity
{
    //==============================================================================
    static
    {
        System.loadLibrary ("juce_jni");
    }

    //==============================================================================
    public boolean isPermissionDeclaredInManifest (int permissionID)
    {
        String permissionToCheck = getAndroidPermissionName(permissionID);

        try
        {
            PackageInfo info = getPackageManager().getPackageInfo(getApplicationContext().getPackageName(), PackageManager.GET_PERMISSIONS);

            if (info.requestedPermissions != null)
                for (String permission : info.requestedPermissions)
                    if (permission.equals (permissionToCheck))
                        return true;
        }
        catch (PackageManager.NameNotFoundException e)
        {
            Log.d ("JUCE", "isPermissionDeclaredInManifest: PackageManager.NameNotFoundException = " + e.toString());
        }

        Log.d ("JUCE", "isPermissionDeclaredInManifest: could not find requested permission " + permissionToCheck);
        return false;
    }

    //==============================================================================
    // these have to match the values of enum PermissionID in C++ class RuntimePermissions:
    private static final int JUCE_PERMISSIONS_RECORD_AUDIO = 1;
    private static final int JUCE_PERMISSIONS_BLUETOOTH_MIDI = 2;
    private static final int JUCE_PERMISSIONS_READ_EXTERNAL_STORAGE = 3;
    private static final int JUCE_PERMISSIONS_WRITE_EXTERNAL_STORAGE = 4;

    private static String getAndroidPermissionName (int permissionID)
    {
        switch (permissionID)
        {
            case JUCE_PERMISSIONS_RECORD_AUDIO:           return Manifest.permission.RECORD_AUDIO;
            case JUCE_PERMISSIONS_BLUETOOTH_MIDI:         return Manifest.permission.ACCESS_COARSE_LOCATION;
                                                          // use string value as this is not defined in SDKs < 16
            case JUCE_PERMISSIONS_READ_EXTERNAL_STORAGE:  return "android.permission.READ_EXTERNAL_STORAGE";
            case JUCE_PERMISSIONS_WRITE_EXTERNAL_STORAGE: return Manifest.permission.WRITE_EXTERNAL_STORAGE;
        }

        // unknown permission ID!
        assert false;
        return new String();
    }

    public boolean isPermissionGranted (int permissionID)
    {
        return getApplicationContext().checkCallingOrSelfPermission (getAndroidPermissionName (permissionID)) == PackageManager.PERMISSION_GRANTED;
    }

    private Map<Integer, Long> permissionCallbackPtrMap;

    public void requestRuntimePermission (int permissionID, long ptrToCallback)
    {
        String permissionName = getAndroidPermissionName (permissionID);

        if (getApplicationContext().checkCallingOrSelfPermission (permissionName) != PackageManager.PERMISSION_GRANTED)
        {
            // remember callbackPtr, request permissions, and let onRequestPermissionResult call callback asynchronously
            permissionCallbackPtrMap.put (permissionID, ptrToCallback);
            requestPermissionsCompat (new String[]{permissionName}, permissionID);
        }
        else
        {
            // permissions were already granted before, we can call callback directly
            androidRuntimePermissionsCallback (true, ptrToCallback);
        }
    }

    private native void androidRuntimePermissionsCallback (boolean permissionWasGranted, long ptrToCallback);

    $$JuceAndroidRuntimePermissionsCode$$ // If you get an error here, you need to re-save your project with the Projucer!

    //==============================================================================
    public interface JuceMidiPort
    {
        boolean isInputPort();

        // start, stop does nothing on an output port
        void start();
        void stop();

        void close();

        // send will do nothing on an input port
        void sendMidi (byte[] msg, int offset, int count);
    }

    //==============================================================================
    $$JuceAndroidMidiCode$$ // If you get an error here, you need to re-save your project with the Projucer!

    //==============================================================================
    @Override
    public void onCreate (Bundle savedInstanceState)
    {
        super.onCreate (savedInstanceState);

        isScreenSaverEnabled = true;
        hideActionBar();
        viewHolder = new ViewHolder (this);
        setContentView (viewHolder);

        setVolumeControlStream (AudioManager.STREAM_MUSIC);

        permissionCallbackPtrMap = new HashMap<Integer, Long>();
    }

    @Override
    protected void onDestroy()
    {
        quitApp();
        super.onDestroy();

        clearDataCache();
    }

    @Override
    protected void onPause()
    {
        suspendApp();

        try
        {
            Thread.sleep (1000); // This is a bit of a hack to avoid some hard-to-track-down
                                 // openGL glitches when pausing/resuming apps..
        } catch (InterruptedException e) {}

        super.onPause();
    }

    @Override
    protected void onResume()
    {
        super.onResume();
        resumeApp();
    }

    @Override
    public void onConfigurationChanged (Configuration cfg)
    {
        super.onConfigurationChanged (cfg);
        setContentView (viewHolder);
    }

    private void callAppLauncher()
    {
        launchApp (getApplicationInfo().publicSourceDir,
                   getApplicationInfo().dataDir);
    }

    //==============================================================================
    private void hideActionBar()
    {
        // get "getActionBar" method
        java.lang.reflect.Method getActionBarMethod = null;
        try
        {
            getActionBarMethod = this.getClass().getMethod ("getActionBar");
        }
        catch (SecurityException e)     { return; }
        catch (NoSuchMethodException e) { return; }
        if (getActionBarMethod == null) return;

        // invoke "getActionBar" method
        Object actionBar = null;
        try
        {
            actionBar = getActionBarMethod.invoke (this);
        }
        catch (java.lang.IllegalArgumentException e) { return; }
        catch (java.lang.IllegalAccessException e) { return; }
        catch (java.lang.reflect.InvocationTargetException e) { return; }
        if (actionBar == null) return;

        // get "hide" method
        java.lang.reflect.Method actionBarHideMethod = null;
        try
        {
            actionBarHideMethod = actionBar.getClass().getMethod ("hide");
        }
        catch (SecurityException e)     { return; }
        catch (NoSuchMethodException e) { return; }
        if (actionBarHideMethod == null) return;

        // invoke "hide" method
        try
        {
            actionBarHideMethod.invoke (actionBar);
        }
        catch (java.lang.IllegalArgumentException e) {}
        catch (java.lang.IllegalAccessException e) {}
        catch (java.lang.reflect.InvocationTargetException e) {}
    }

    void requestPermissionsCompat (String[] permissions, int requestCode)
    {
        Method requestPermissionsMethod = null;
        try
        {
            requestPermissionsMethod = this.getClass().getMethod ("requestPermissions",
                                                                  String[].class, int.class);
        }
        catch (SecurityException e)     { return; }
        catch (NoSuchMethodException e) { return; }
        if (requestPermissionsMethod == null) return;

        try
        {
            requestPermissionsMethod.invoke (this, permissions, requestCode);
        }
        catch (java.lang.IllegalArgumentException e) {}
        catch (java.lang.IllegalAccessException e) {}
        catch (java.lang.reflect.InvocationTargetException e) {}
    }

    //==============================================================================
    private native void launchApp (String appFile, String appDataDir);
    private native void quitApp();
    private native void suspendApp();
    private native void resumeApp();
    private native void setScreenSize (int screenWidth, int screenHeight, int dpi);
    private native void appActivityResult (int requestCode, int resultCode, Intent data);
    private native void appNewIntent (Intent intent);

    //==============================================================================
    private ViewHolder viewHolder;
    private MidiDeviceManager midiDeviceManager = null;
    private BluetoothManager bluetoothManager = null;
    private boolean isScreenSaverEnabled;
    private java.util.Timer keepAliveTimer;

    public final ComponentPeerView createNewView (boolean opaque, long host)
    {
        ComponentPeerView v = new ComponentPeerView (this, opaque, host);
        viewHolder.addView (v);
        return v;
    }

    public final void deleteView (ComponentPeerView view)
    {
        view.host = 0;

        ViewGroup group = (ViewGroup) (view.getParent());

        if (group != null)
            group.removeView (view);
    }

    public final void deleteNativeSurfaceView (NativeSurfaceView view)
    {
        ViewGroup group = (ViewGroup) (view.getParent());

        if (group != null)
            group.removeView (view);
    }

    final class ViewHolder  extends ViewGroup
    {
        public ViewHolder (Context context)
        {
            super (context);
            setDescendantFocusability (ViewGroup.FOCUS_AFTER_DESCENDANTS);
            setFocusable (false);
        }

        protected final void onLayout (boolean changed, int left, int top, int right, int bottom)
        {
            setScreenSize (getWidth(), getHeight(), getDPI());

            if (isFirstResize)
            {
                isFirstResize = false;
                callAppLauncher();
            }
        }

        private final int getDPI()
        {
            DisplayMetrics metrics = new DisplayMetrics();
            getWindowManager().getDefaultDisplay().getMetrics (metrics);
            return metrics.densityDpi;
        }

        private boolean isFirstResize = true;
    }

    public final void excludeClipRegion (android.graphics.Canvas canvas, float left, float top, float right, float bottom)
    {
        canvas.clipRect (left, top, right, bottom, android.graphics.Region.Op.DIFFERENCE);
    }

    //==============================================================================
    public final void setScreenSaver (boolean enabled)
    {
        if (isScreenSaverEnabled != enabled)
        {
            isScreenSaverEnabled = enabled;

            if (keepAliveTimer != null)
            {
                keepAliveTimer.cancel();
                keepAliveTimer = null;
            }

            if (enabled)
            {
                getWindow().clearFlags (WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            }
            else
            {
                getWindow().addFlags (WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);

                // If no user input is received after about 3 seconds, the OS will lower the
                // task's priority, so this timer forces it to be kept active.
                keepAliveTimer = new java.util.Timer();

                keepAliveTimer.scheduleAtFixedRate (new TimerTask()
                {
                    @Override
                    public void run()
                    {
                        android.app.Instrumentation instrumentation = new android.app.Instrumentation();

                        try
                        {
                            instrumentation.sendKeyDownUpSync (KeyEvent.KEYCODE_UNKNOWN);
                        }
                        catch (Exception e)
                        {
                        }
                    }
                }, 2000, 2000);
            }
        }
    }

    public final boolean getScreenSaver()
    {
        return isScreenSaverEnabled;
    }

    //==============================================================================
    public final String getClipboardContent()
    {
        ClipboardManager clipboard = (ClipboardManager) getSystemService (CLIPBOARD_SERVICE);
        return clipboard.getText().toString();
    }

    public final void setClipboardContent (String newText)
    {
        ClipboardManager clipboard = (ClipboardManager) getSystemService (CLIPBOARD_SERVICE);
        clipboard.setText (newText);
    }

    //==============================================================================
    public final void showMessageBox (String title, String message, final long callback)
    {
        AlertDialog.Builder builder = new AlertDialog.Builder (this);
        builder.setTitle (title)
               .setMessage (message)
               .setCancelable (true)
               .setOnCancelListener (new DialogInterface.OnCancelListener()
                    {
                        public void onCancel (DialogInterface dialog)
                        {
                            JuceAppActivity.this.alertDismissed (callback, 0);
                        }
                    })
               .setPositiveButton ("OK", new DialogInterface.OnClickListener()
                    {
                        public void onClick (DialogInterface dialog, int id)
                        {
                            dialog.dismiss();
                            JuceAppActivity.this.alertDismissed (callback, 0);
                        }
                    });

        builder.create().show();
    }

    public final void showOkCancelBox (String title, String message, final long callback,
                                       String okButtonText, String cancelButtonText)
    {
        AlertDialog.Builder builder = new AlertDialog.Builder (this);
        builder.setTitle (title)
               .setMessage (message)
               .setCancelable (true)
               .setOnCancelListener (new DialogInterface.OnCancelListener()
                    {
                        public void onCancel (DialogInterface dialog)
                        {
                            JuceAppActivity.this.alertDismissed (callback, 0);
                        }
                    })
               .setPositiveButton (okButtonText.isEmpty() ? "OK" : okButtonText, new DialogInterface.OnClickListener()
                    {
                        public void onClick (DialogInterface dialog, int id)
                        {
                            dialog.dismiss();
                            JuceAppActivity.this.alertDismissed (callback, 1);
                        }
                    })
               .setNegativeButton (cancelButtonText.isEmpty() ? "Cancel" : cancelButtonText, new DialogInterface.OnClickListener()
                    {
                        public void onClick (DialogInterface dialog, int id)
                        {
                            dialog.dismiss();
                            JuceAppActivity.this.alertDismissed (callback, 0);
                        }
                    });

        builder.create().show();
    }

    public final void showYesNoCancelBox (String title, String message, final long callback)
    {
        AlertDialog.Builder builder = new AlertDialog.Builder (this);
        builder.setTitle (title)
               .setMessage (message)
               .setCancelable (true)
               .setOnCancelListener (new DialogInterface.OnCancelListener()
                    {
                        public void onCancel (DialogInterface dialog)
                        {
                            JuceAppActivity.this.alertDismissed (callback, 0);
                        }
                    })
               .setPositiveButton ("Yes", new DialogInterface.OnClickListener()
                    {
                        public void onClick (DialogInterface dialog, int id)
                        {
                            dialog.dismiss();
                            JuceAppActivity.this.alertDismissed (callback, 1);
                        }
                    })
               .setNegativeButton ("No", new DialogInterface.OnClickListener()
                    {
                        public void onClick (DialogInterface dialog, int id)
                        {
                            dialog.dismiss();
                            JuceAppActivity.this.alertDismissed (callback, 2);
                        }
                    })
               .setNeutralButton ("Cancel", new DialogInterface.OnClickListener()
                    {
                        public void onClick (DialogInterface dialog, int id)
                        {
                            dialog.dismiss();
                            JuceAppActivity.this.alertDismissed (callback, 0);
                        }
                    });

        builder.create().show();
    }

    public native void alertDismissed (long callback, int id);

    //==============================================================================
    public final class ComponentPeerView extends ViewGroup
                                         implements View.OnFocusChangeListener
    {
        public ComponentPeerView (Context context, boolean opaque_, long host)
        {
            super (context);
            this.host = host;
            setWillNotDraw (false);
            opaque = opaque_;

            setFocusable (true);
            setFocusableInTouchMode (true);
            setOnFocusChangeListener (this);

            // swap red and blue colours to match internal opengl texture format
            ColorMatrix colorMatrix = new ColorMatrix();

            float[] colorTransform = { 0,    0,    1.0f, 0,    0,
                                       0,    1.0f, 0,    0,    0,
                                       1.0f, 0,    0,    0,    0,
                                       0,    0,    0,    1.0f, 0 };

            colorMatrix.set (colorTransform);
            paint.setColorFilter (new ColorMatrixColorFilter (colorMatrix));
        }

        //==============================================================================
        private native void handlePaint (long host, Canvas canvas, Paint paint);

        @Override
        public void onDraw (Canvas canvas)
        {
            if (host == 0)
                return;

            handlePaint (host, canvas, paint);
        }

        @Override
        public boolean isOpaque()
        {
            return opaque;
        }

        private boolean opaque;
        private long host;
        private Paint paint = new Paint();

        //==============================================================================
        private native void handleMouseDown (long host, int index, float x, float y, long time);
        private native void handleMouseDrag (long host, int index, float x, float y, long time);
        private native void handleMouseUp   (long host, int index, float x, float y, long time);

        @Override
        public boolean onTouchEvent (MotionEvent event)
        {
            if (host == 0)
                return false;

            int action = event.getAction();
            long time = event.getEventTime();

            switch (action & MotionEvent.ACTION_MASK)
            {
                case MotionEvent.ACTION_DOWN:
                    handleMouseDown (host, event.getPointerId(0), event.getX(), event.getY(), time);
                    return true;

                case MotionEvent.ACTION_CANCEL:
                case MotionEvent.ACTION_UP:
                    handleMouseUp (host, event.getPointerId(0), event.getX(), event.getY(), time);
                    return true;

                case MotionEvent.ACTION_MOVE:
                {
                    int n = event.getPointerCount();
                    for (int i = 0; i < n; ++i)
                        handleMouseDrag (host, event.getPointerId(i), event.getX(i), event.getY(i), time);

                    return true;
                }

                case MotionEvent.ACTION_POINTER_UP:
                {
                    int i = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT;
                    handleMouseUp (host, event.getPointerId(i), event.getX(i), event.getY(i), time);
                    return true;
                }

                case MotionEvent.ACTION_POINTER_DOWN:
                {
                    int i = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT;
                    handleMouseDown (host, event.getPointerId(i), event.getX(i), event.getY(i), time);
                    return true;
                }

                default:
                    break;
            }

            return false;
        }

        //==============================================================================
        private native void handleKeyDown (long host, int keycode, int textchar);
        private native void handleKeyUp (long host, int keycode, int textchar);
        private native void handleBackButton (long host);

        public void showKeyboard (String type)
        {
            InputMethodManager imm = (InputMethodManager) getSystemService (Context.INPUT_METHOD_SERVICE);

            if (imm != null)
            {
                if (type.length() > 0)
                {
                    imm.showSoftInput (this, android.view.inputmethod.InputMethodManager.SHOW_IMPLICIT);
                    imm.setInputMethod (getWindowToken(), type);
                }
                else
                {
                    imm.hideSoftInputFromWindow (getWindowToken(), 0);
                }
            }
        }

        @Override
        public boolean onKeyDown (int keyCode, KeyEvent event)
        {
            if (host == 0)
                return false;

            switch (keyCode)
            {
                case KeyEvent.KEYCODE_VOLUME_UP:
                case KeyEvent.KEYCODE_VOLUME_DOWN:
                    return super.onKeyDown (keyCode, event);
                case KeyEvent.KEYCODE_BACK:
                {
                    handleBackButton (host);
                    return true;
                }

                default:
                    break;
            }

            handleKeyDown (host, keyCode, event.getUnicodeChar());
            return true;
        }

        @Override
        public boolean onKeyUp (int keyCode, KeyEvent event)
        {
            if (host == 0)
                return false;

            handleKeyUp (host, keyCode, event.getUnicodeChar());
            return true;
        }

        @Override
        public boolean onKeyMultiple (int keyCode, int count, KeyEvent event)
        {
            if (host == 0)
                return false;

            if (keyCode != KeyEvent.KEYCODE_UNKNOWN || event.getAction() != KeyEvent.ACTION_MULTIPLE)
                return super.onKeyMultiple (keyCode, count, event);

            if (event.getCharacters() != null)
            {
                int utf8Char = event.getCharacters().codePointAt (0);
                handleKeyDown (host, utf8Char, utf8Char);
                return true;
            }

            return false;
        }

        // this is here to make keyboard entry work on a Galaxy Tab2 10.1
        @Override
        public InputConnection onCreateInputConnection (EditorInfo outAttrs)
        {
            outAttrs.actionLabel = "";
            outAttrs.hintText = "";
            outAttrs.initialCapsMode = 0;
            outAttrs.initialSelEnd = outAttrs.initialSelStart = -1;
            outAttrs.label = "";
            outAttrs.imeOptions = EditorInfo.IME_ACTION_DONE | EditorInfo.IME_FLAG_NO_EXTRACT_UI;
            outAttrs.inputType = InputType.TYPE_NULL;

            return new BaseInputConnection (this, false);
        }

        //==============================================================================
        @Override
        protected void onSizeChanged (int w, int h, int oldw, int oldh)
        {
            if (host == 0)
                return;

            super.onSizeChanged (w, h, oldw, oldh);
            viewSizeChanged (host);
        }

        @Override
        protected void onLayout (boolean changed, int left, int top, int right, int bottom)
        {
            for (int i = getChildCount(); --i >= 0;)
                requestTransparentRegion (getChildAt (i));
        }

        private native void viewSizeChanged (long host);

        @Override
        public void onFocusChange (View v, boolean hasFocus)
        {
            if (host == 0)
                return;

            if (v == this)
                focusChanged (host, hasFocus);
        }

        private native void focusChanged (long host, boolean hasFocus);

        public void setViewName (String newName)    {}

        public void setSystemUiVisibilityCompat (int visibility)
        {
            Method systemUIVisibilityMethod = null;
            try
            {
                systemUIVisibilityMethod = this.getClass().getMethod ("setSystemUiVisibility", int.class);
            }
            catch (SecurityException e)     { return; }
            catch (NoSuchMethodException e) { return; }
            if (systemUIVisibilityMethod == null) return;

            try
            {
                systemUIVisibilityMethod.invoke (this, visibility);
            }
            catch (java.lang.IllegalArgumentException e) {}
            catch (java.lang.IllegalAccessException e) {}
            catch (java.lang.reflect.InvocationTargetException e) {}
        }

        public boolean isVisible()                  { return getVisibility() == VISIBLE; }
        public void setVisible (boolean b)          { setVisibility (b ? VISIBLE : INVISIBLE); }

        public boolean containsPoint (int x, int y)
        {
            return true; //xxx needs to check overlapping views
        }
    }

    //==============================================================================
    public static class NativeSurfaceView    extends SurfaceView
                                          implements SurfaceHolder.Callback
    {
        private long nativeContext = 0;

        NativeSurfaceView (Context context, long nativeContextPtr)
        {
            super (context);
            nativeContext = nativeContextPtr;
        }

        public Surface getNativeSurface()
        {
            Surface retval = null;

            SurfaceHolder holder = getHolder();
            if (holder != null)
                retval = holder.getSurface();

            return retval;
        }

        //==============================================================================
        @Override
        public void surfaceChanged (SurfaceHolder holder, int format, int width, int height)
        {
            surfaceChangedNative (nativeContext, holder, format, width, height);
        }

        @Override
        public void surfaceCreated (SurfaceHolder holder)
        {
            surfaceCreatedNative (nativeContext, holder);
        }

        @Override
        public void surfaceDestroyed (SurfaceHolder holder)
        {
            surfaceDestroyedNative (nativeContext, holder);
        }

        @Override
        protected void dispatchDraw (Canvas canvas)
        {
            super.dispatchDraw (canvas);
            dispatchDrawNative (nativeContext, canvas);
        }

        //==============================================================================
        @Override
        protected void onAttachedToWindow ()
        {
            super.onAttachedToWindow();
            getHolder().addCallback (this);
        }

        @Override
        protected void onDetachedFromWindow ()
        {
            super.onDetachedFromWindow();
            getHolder().removeCallback (this);
        }

        //==============================================================================
        private native void dispatchDrawNative (long nativeContextPtr, Canvas canvas);
        private native void surfaceCreatedNative (long nativeContextptr, SurfaceHolder holder);
        private native void surfaceDestroyedNative (long nativeContextptr, SurfaceHolder holder);
        private native void surfaceChangedNative (long nativeContextptr, SurfaceHolder holder,
                                                  int format, int width, int height);
    }

    public NativeSurfaceView createNativeSurfaceView (long nativeSurfacePtr)
    {
        return new NativeSurfaceView (this, nativeSurfacePtr);
    }

    //==============================================================================
    public final int[] renderGlyph (char glyph1, char glyph2, Paint paint, android.graphics.Matrix matrix, Rect bounds)
    {
        Path p = new Path();

        char[] str = { glyph1, glyph2 };
        paint.getTextPath (str, 0, (glyph2 != 0 ? 2 : 1), 0.0f, 0.0f, p);

        RectF boundsF = new RectF();
        p.computeBounds (boundsF, true);
        matrix.mapRect (boundsF);

        boundsF.roundOut (bounds);
        bounds.left--;
        bounds.right++;

        final int w = bounds.width();
        final int h = Math.max (1, bounds.height());

        Bitmap bm = Bitmap.createBitmap (w, h, Bitmap.Config.ARGB_8888);

        Canvas c = new Canvas (bm);
        matrix.postTranslate (-bounds.left, -bounds.top);
        c.setMatrix (matrix);
        c.drawPath (p, paint);

        final int sizeNeeded = w * h;
        if (cachedRenderArray.length < sizeNeeded)
            cachedRenderArray = new int [sizeNeeded];

        bm.getPixels (cachedRenderArray, 0, w, 0, 0, w, h);
        bm.recycle();
        return cachedRenderArray;
    }

    private int[] cachedRenderArray = new int [256];

    //==============================================================================
    public static class NativeInvocationHandler implements InvocationHandler
    {
        public NativeInvocationHandler (long nativeContextRef)
        {
            nativeContext = nativeContextRef;
        }

        @Override
        public void finalize()
        {
            dispatchFinalize (nativeContext);
        }

        @Override
        public Object invoke (Object proxy, Method method, Object[] args) throws Throwable
        {
            return dispatchInvoke (nativeContext, proxy, method, args);
        }

        //==============================================================================
        private long nativeContext = 0;

        private native void dispatchFinalize (long nativeContextRef);
        private native Object dispatchInvoke (long nativeContextRef, Object proxy, Method method, Object[] args);
    }

    public static InvocationHandler createInvocationHandler (long nativeContextRef)
    {
        return new NativeInvocationHandler (nativeContextRef);
    }

    //==============================================================================
    public static class HTTPStream
    {
        public HTTPStream (String address, boolean isPostToUse, byte[] postDataToUse,
                           String headersToUse, int timeOutMsToUse,
                           int[] statusCodeToUse, StringBuffer responseHeadersToUse,
                           int numRedirectsToFollowToUse, String httpRequestCmdToUse) throws IOException
        {
            isPost = isPostToUse;
            postData = postDataToUse;
            headers = headersToUse;
            timeOutMs = timeOutMsToUse;
            statusCode = statusCodeToUse;
            responseHeaders = responseHeadersToUse;
            totalLength = -1;
            numRedirectsToFollow = numRedirectsToFollowToUse;
            httpRequestCmd = httpRequestCmdToUse;

            connection = createConnection (address, isPost, postData, headers, timeOutMs, httpRequestCmd);
        }

        private final HttpURLConnection createConnection (String address, boolean isPost, byte[] postData,
                                                          String headers, int timeOutMs, String httpRequestCmdToUse) throws IOException
        {
            HttpURLConnection newConnection = (HttpURLConnection) (new URL(address).openConnection());

            try
            {
                newConnection.setInstanceFollowRedirects (false);
                newConnection.setConnectTimeout (timeOutMs);
                newConnection.setReadTimeout (timeOutMs);

                // headers - if not empty, this string is appended onto the headers that are used for the request. It must therefore be a valid set of HTML header directives, separated by newlines.
                // So convert headers string to an array, with an element for each line
                String headerLines[] = headers.split("\\n");

                // Set request headers
                for (int i = 0; i < headerLines.length; ++i)
                {
                    int pos = headerLines[i].indexOf (":");

                    if (pos > 0 && pos < headerLines[i].length())
                    {
                        String field = headerLines[i].substring (0, pos);
                        String value = headerLines[i].substring (pos + 1);

                        if (value.length() > 0)
                            newConnection.setRequestProperty (field, value);
                    }
                }

                newConnection.setRequestMethod (httpRequestCmd);

                if (isPost)
                {
                    newConnection.setDoOutput (true);

                    if (postData != null)
                    {
                        OutputStream out = newConnection.getOutputStream();
                        out.write(postData);
                        out.flush();
                    }
                }

                return newConnection;
            }
            catch (Throwable e)
            {
                newConnection.disconnect();
                throw new IOException ("Connection error");
            }
        }

        private final InputStream getCancellableStream (final boolean isInput) throws ExecutionException
        {
            synchronized (createFutureLock)
            {
                if (hasBeenCancelled.get())
                    return null;

                streamFuture = executor.submit (new Callable<BufferedInputStream>()
                {
                    @Override
                    public BufferedInputStream call() throws IOException
                    {
                        return new BufferedInputStream (isInput ? connection.getInputStream()
                                                                : connection.getErrorStream());
                    }
                });
            }

            try
            {
                return streamFuture.get();
            }
            catch (InterruptedException e)
            {
                return null;
            }
            catch (CancellationException e)
            {
                return null;
            }
        }

        public final boolean connect()
        {
            boolean result = false;
            int numFollowedRedirects = 0;

            while (true)
            {
                result = doConnect();

                if (! result)
                    return false;

                if (++numFollowedRedirects > numRedirectsToFollow)
                    break;

                int status = statusCode[0];

                if (status == 301 || status == 302 || status == 303 || status == 307)
                {
                    // Assumes only one occurrence of "Location"
                    int pos1 = responseHeaders.indexOf ("Location:") + 10;
                    int pos2 = responseHeaders.indexOf ("\n", pos1);

                    if (pos2 > pos1)
                    {
                        String currentLocation = connection.getURL().toString();
                        String newLocation = responseHeaders.substring (pos1, pos2);

                        try
                        {
                            // Handle newLocation whether it's absolute or relative
                            URL baseUrl = new URL (currentLocation);
                            URL newUrl  = new URL (baseUrl, newLocation);
                            String transformedNewLocation = newUrl.toString();

                            if (transformedNewLocation != currentLocation)
                            {
                                // Clear responseHeaders before next iteration
                                responseHeaders.delete (0, responseHeaders.length());

                                synchronized (createStreamLock)
                                {
                                    if (hasBeenCancelled.get())
                                        return false;

                                    connection.disconnect();

                                    try
                                    {
                                        connection = createConnection (transformedNewLocation, isPost,
                                                                       postData, headers, timeOutMs,
                                                                       httpRequestCmd);
                                    }
                                    catch (Throwable e)
                                    {
                                        return false;
                                    }
                                }
                            }
                            else
                            {
                                break;
                            }
                        }
                        catch (Throwable e)
                        {
                            return false;
                        }
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }

            return result;
        }

        private final boolean doConnect()
        {
            synchronized (createStreamLock)
            {
                if (hasBeenCancelled.get())
                    return false;

                try
                {
                    try
                    {
                        inputStream = getCancellableStream (true);
                    }
                    catch (ExecutionException e)
                    {
                        if (connection.getResponseCode() < 400)
                        {
                            statusCode[0] = connection.getResponseCode();
                            connection.disconnect();
                            return false;
                        }
                    }
                    finally
                    {
                        statusCode[0] = connection.getResponseCode();
                    }

                    try
                    {
                        if (statusCode[0] >= 400)
                            inputStream = getCancellableStream (false);
                        else
                            inputStream = getCancellableStream (true);
                    }
                    catch (ExecutionException e)
                    {}

                    for (java.util.Map.Entry<String, java.util.List<String>> entry : connection.getHeaderFields().entrySet())
                    {
                        if (entry.getKey() != null && entry.getValue() != null)
                        {
                            responseHeaders.append(entry.getKey() + ": "
                                                   + android.text.TextUtils.join(",", entry.getValue()) + "\n");

                            if (entry.getKey().compareTo ("Content-Length") == 0)
                                totalLength = Integer.decode (entry.getValue().get (0));
                        }
                    }

                    return true;
                }
                catch (IOException e)
                {
                    return false;
                }
            }
        }

        static class DisconnectionRunnable implements Runnable
        {
            public DisconnectionRunnable (HttpURLConnection theConnection,
                                          InputStream theInputStream,
                                          ReentrantLock theCreateStreamLock,
                                          Object theCreateFutureLock,
                                          Future<BufferedInputStream> theStreamFuture)
            {
                connectionToDisconnect = theConnection;
                inputStream = theInputStream;
                createStreamLock = theCreateStreamLock;
                createFutureLock = theCreateFutureLock;
                streamFuture = theStreamFuture;
            }

            public void run()
            {
                try
                {
                    if (! createStreamLock.tryLock())
                    {
                        synchronized (createFutureLock)
                        {
                            if (streamFuture != null)
                                streamFuture.cancel (true);
                        }

                        createStreamLock.lock();
                    }

                    if (connectionToDisconnect != null)
                        connectionToDisconnect.disconnect();

                    if (inputStream != null)
                        inputStream.close();
                }
                catch (IOException e)
                {}
                finally
                {
                    createStreamLock.unlock();
                }
            }

            private HttpURLConnection connectionToDisconnect;
            private InputStream inputStream;
            private ReentrantLock createStreamLock;
            private Object createFutureLock;
            Future<BufferedInputStream> streamFuture;
        }

        public final void release()
        {
            DisconnectionRunnable disconnectionRunnable = new DisconnectionRunnable (connection,
                                                                                     inputStream,
                                                                                     createStreamLock,
                                                                                     createFutureLock,
                                                                                     streamFuture);

            synchronized (createStreamLock)
            {
                hasBeenCancelled.set (true);

                connection = null;
            }

            Thread disconnectionThread = new Thread(disconnectionRunnable);
            disconnectionThread.start();
        }

        public final int read (byte[] buffer, int numBytes)
        {
            int num = 0;

            try
            {
                synchronized (createStreamLock)
                {
                    if (inputStream != null)
                        num = inputStream.read (buffer, 0, numBytes);
                }
            }
            catch (IOException e)
            {}

            if (num > 0)
                position += num;

            return num;
        }

        public final long getPosition()                 { return position; }
        public final long getTotalLength()              { return totalLength; }
        public final boolean isExhausted()              { return false; }
        public final boolean setPosition (long newPos)  { return false; }

        private boolean isPost;
        private byte[] postData;
        private String headers;
        private int timeOutMs;
        String httpRequestCmd;
        private HttpURLConnection connection;
        private int[] statusCode;
        private StringBuffer responseHeaders;
        private int totalLength;
        private int numRedirectsToFollow;
        private InputStream inputStream;
        private long position;
        private final ReentrantLock createStreamLock = new ReentrantLock();
        private final Object createFutureLock = new Object();
        private AtomicBoolean hasBeenCancelled = new AtomicBoolean();

        private final ExecutorService executor = Executors.newCachedThreadPool (Executors.defaultThreadFactory());
        Future<BufferedInputStream> streamFuture;
    }

    public static final HTTPStream createHTTPStream (String address, boolean isPost, byte[] postData,
                                                     String headers, int timeOutMs, int[] statusCode,
                                                     StringBuffer responseHeaders, int numRedirectsToFollow,
                                                     String httpRequestCmd)
    {
        // timeout parameter of zero for HttpUrlConnection is a blocking connect (negative value for juce::URL)
        if (timeOutMs < 0)
            timeOutMs = 0;
        else if (timeOutMs == 0)
            timeOutMs = 30000;

        for (;;)
        {
            try
            {
                HTTPStream httpStream = new HTTPStream (address, isPost, postData, headers,
                                                        timeOutMs, statusCode, responseHeaders,
                                                        numRedirectsToFollow, httpRequestCmd);

                return httpStream;
            }
            catch (Throwable e) {}

            return null;
        }
    }

    public final void launchURL (String url)
    {
        startActivity (new Intent (Intent.ACTION_VIEW, Uri.parse (url)));
    }

    public static final String getLocaleValue (boolean isRegion)
    {
        java.util.Locale locale = java.util.Locale.getDefault();

        return isRegion ? locale.getCountry()
                        : locale.getLanguage();
    }

    private static final String getFileLocation (String type)
    {
        return Environment.getExternalStoragePublicDirectory (type).getAbsolutePath();
    }

    public static final String getDocumentsFolder()
    {
        if (getAndroidSDKVersion() >= 19)
            return getFileLocation ("Documents");

        return Environment.getDataDirectory().getAbsolutePath();
    }

    public static final String getPicturesFolder()   { return getFileLocation (Environment.DIRECTORY_PICTURES); }
    public static final String getMusicFolder()      { return getFileLocation (Environment.DIRECTORY_MUSIC); }
    public static final String getMoviesFolder()     { return getFileLocation (Environment.DIRECTORY_MOVIES); }
    public static final String getDownloadsFolder()  { return getFileLocation (Environment.DIRECTORY_DOWNLOADS); }

    //==============================================================================
    @Override
    protected void onActivityResult (int requestCode, int resultCode, Intent data)
    {
        appActivityResult (requestCode, resultCode, data);
    }

    @Override
    protected void onNewIntent (Intent intent)
    {
        super.onNewIntent(intent);
        setIntent(intent);

        appNewIntent (intent);
    }

    //==============================================================================
    public final Typeface getTypeFaceFromAsset (String assetName)
    {
        try
        {
            return Typeface.createFromAsset (this.getResources().getAssets(), assetName);
        }
        catch (Throwable e) {}

        return null;
    }

    final protected static char[] hexArray = "0123456789ABCDEF".toCharArray();

    public static String bytesToHex (byte[] bytes)
    {
        char[] hexChars = new char[bytes.length * 2];

        for (int j = 0; j < bytes.length; ++j)
        {
            int v = bytes[j] & 0xff;
            hexChars[j * 2]     = hexArray[v >>> 4];
            hexChars[j * 2 + 1] = hexArray[v & 0x0f];
        }

        return new String (hexChars);
    }

    final private java.util.Map dataCache = new java.util.HashMap();

    synchronized private final File getDataCacheFile (byte[] data)
    {
        try
        {
            java.security.MessageDigest digest = java.security.MessageDigest.getInstance ("MD5");
            digest.update (data);

            String key = bytesToHex (digest.digest());

            if (dataCache.containsKey (key))
                return (File) dataCache.get (key);

            File f = new File (this.getCacheDir(), "bindata_" + key);
            f.delete();
            FileOutputStream os = new FileOutputStream (f);
            os.write (data, 0, data.length);
            dataCache.put (key, f);
            return f;
        }
        catch (Throwable e) {}

        return null;
    }

    private final void clearDataCache()
    {
        java.util.Iterator it = dataCache.values().iterator();

        while (it.hasNext())
        {
            File f = (File) it.next();
            f.delete();
        }
    }

    public final Typeface getTypeFaceFromByteArray (byte[] data)
    {
        try
        {
            File f = getDataCacheFile (data);

            if (f != null)
                return Typeface.createFromFile (f);
        }
        catch (Exception e)
        {
            Log.e ("JUCE", e.toString());
        }

        return null;
    }

    public static final int getAndroidSDKVersion()
    {
        return android.os.Build.VERSION.SDK_INT;
    }

    public final String audioManagerGetProperty (String property)
    {
        Object obj = getSystemService (AUDIO_SERVICE);
        if (obj == null)
            return null;

        java.lang.reflect.Method method;

        try
        {
            method = obj.getClass().getMethod ("getProperty", String.class);
        }
        catch (SecurityException e)     { return null; }
        catch (NoSuchMethodException e) { return null; }

        if (method == null)
            return null;

        try
        {
            return (String) method.invoke (obj, property);
        }
        catch (java.lang.IllegalArgumentException e) {}
        catch (java.lang.IllegalAccessException e) {}
        catch (java.lang.reflect.InvocationTargetException e) {}

        return null;
    }

    public final boolean hasSystemFeature (String property)
    {
        return getPackageManager().hasSystemFeature (property);
    }
}
