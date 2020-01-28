package com.juce;

import com.google.firebase.iid.*;

public final class JuceFirebaseInstanceIdService extends FirebaseInstanceIdService
{
    private native void firebaseInstanceIdTokenRefreshed (String token);

    @Override
    public void onTokenRefresh()
    {
        String token = FirebaseInstanceId.getInstance().getToken();

        firebaseInstanceIdTokenRefreshed (token);
    }
}
