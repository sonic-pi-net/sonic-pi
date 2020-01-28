$$CameraApi21
    //==============================================================================
    public class CameraDeviceStateCallback  extends CameraDevice.StateCallback
    {
        private native void cameraDeviceStateClosed       (long host, CameraDevice camera);
        private native void cameraDeviceStateDisconnected (long host, CameraDevice camera);
        private native void cameraDeviceStateError        (long host, CameraDevice camera, int error);
        private native void cameraDeviceStateOpened       (long host, CameraDevice camera);

        CameraDeviceStateCallback (long hostToUse)
        {
            host = hostToUse;
        }

        @Override
        public void onClosed (CameraDevice camera)
        {
            cameraDeviceStateClosed (host, camera);
        }

        @Override
        public void onDisconnected (CameraDevice camera)
        {
            cameraDeviceStateDisconnected (host, camera);
        }

        @Override
        public void onError (CameraDevice camera, int error)
        {
            cameraDeviceStateError (host, camera, error);
        }

        @Override
        public void onOpened (CameraDevice camera)
        {
            cameraDeviceStateOpened (host, camera);
        }

        private long host;
    }

    //==============================================================================
    public class CameraCaptureSessionStateCallback  extends CameraCaptureSession.StateCallback
    {
        private native void cameraCaptureSessionActive          (long host, CameraCaptureSession session);
        private native void cameraCaptureSessionClosed          (long host, CameraCaptureSession session);
        private native void cameraCaptureSessionConfigureFailed (long host, CameraCaptureSession session);
        private native void cameraCaptureSessionConfigured      (long host, CameraCaptureSession session);
        private native void cameraCaptureSessionReady           (long host, CameraCaptureSession session);

        CameraCaptureSessionStateCallback (long hostToUse)
        {
            host = hostToUse;
        }

        @Override
        public void onActive (CameraCaptureSession session)
        {
            cameraCaptureSessionActive (host, session);
        }

        @Override
        public void onClosed (CameraCaptureSession session)
        {
            cameraCaptureSessionClosed (host, session);
        }

        @Override
        public void onConfigureFailed (CameraCaptureSession session)
        {
            cameraCaptureSessionConfigureFailed (host, session);
        }

        @Override
        public void onConfigured (CameraCaptureSession session)
        {
            cameraCaptureSessionConfigured (host, session);
        }

        @Override
        public void onReady (CameraCaptureSession session)
        {
            cameraCaptureSessionReady (host, session);
        }

        private long host;
    }

    //==============================================================================
    public class CameraCaptureSessionCaptureCallback    extends CameraCaptureSession.CaptureCallback
    {
        private native void cameraCaptureSessionCaptureCompleted  (long host, boolean isPreview, CameraCaptureSession session, CaptureRequest request, TotalCaptureResult result);
        private native void cameraCaptureSessionCaptureFailed     (long host, boolean isPreview, CameraCaptureSession session, CaptureRequest request, CaptureFailure failure);
        private native void cameraCaptureSessionCaptureProgressed (long host, boolean isPreview, CameraCaptureSession session, CaptureRequest request, CaptureResult partialResult);
        private native void cameraCaptureSessionCaptureStarted    (long host, boolean isPreview, CameraCaptureSession session, CaptureRequest request, long timestamp, long frameNumber);
        private native void cameraCaptureSessionCaptureSequenceAborted   (long host, boolean isPreview, CameraCaptureSession session, int sequenceId);
        private native void cameraCaptureSessionCaptureSequenceCompleted (long host, boolean isPreview, CameraCaptureSession session, int sequenceId, long frameNumber);

        CameraCaptureSessionCaptureCallback (long hostToUse, boolean shouldBePreview)
        {
            host = hostToUse;
            preview = shouldBePreview;
        }

        @Override
        public void onCaptureCompleted (CameraCaptureSession session, CaptureRequest request,
                                        TotalCaptureResult result)
        {
            cameraCaptureSessionCaptureCompleted (host, preview, session, request, result);
        }

        @Override
        public void onCaptureFailed (CameraCaptureSession session, CaptureRequest request, CaptureFailure failure)
        {
            cameraCaptureSessionCaptureFailed (host, preview, session, request, failure);
        }

        @Override
        public void onCaptureProgressed (CameraCaptureSession session, CaptureRequest request,
                                         CaptureResult partialResult)
        {
            cameraCaptureSessionCaptureProgressed (host, preview, session, request, partialResult);
        }

        @Override
        public void onCaptureSequenceAborted (CameraCaptureSession session, int sequenceId)
        {
            cameraCaptureSessionCaptureSequenceAborted (host, preview, session, sequenceId);
        }

        @Override
        public void onCaptureSequenceCompleted (CameraCaptureSession session, int sequenceId, long frameNumber)
        {
            cameraCaptureSessionCaptureSequenceCompleted (host, preview, session, sequenceId, frameNumber);
        }

        @Override
        public void onCaptureStarted (CameraCaptureSession session, CaptureRequest request, long timestamp,
                                      long frameNumber)
        {
            cameraCaptureSessionCaptureStarted (host, preview, session, request, timestamp, frameNumber);
        }

        private long host;
        private boolean preview;
    }

    //==============================================================================
    public class JuceOrientationEventListener    extends OrientationEventListener
    {
        private native void deviceOrientationChanged (long host, int orientation);

        public JuceOrientationEventListener (long hostToUse, Context context, int rate)
        {
            super (context, rate);

            host = hostToUse;
        }

        @Override
        public void onOrientationChanged (int orientation)
        {
            deviceOrientationChanged (host, orientation);
        }

        private long host;
    }

CameraApi21$$
