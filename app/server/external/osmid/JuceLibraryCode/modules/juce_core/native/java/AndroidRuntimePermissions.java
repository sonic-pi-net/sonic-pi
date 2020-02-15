    @Override
    public void onRequestPermissionsResult (int permissionID, String permissions[], int[] grantResults)
    {
        boolean permissionsGranted = (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED);

        if (! permissionsGranted)
            Log.d ("JUCE", "onRequestPermissionsResult: runtime permission was DENIED: " + getAndroidPermissionName (permissionID));

        Long ptrToCallback = permissionCallbackPtrMap.get (permissionID);
        permissionCallbackPtrMap.remove (permissionID);
        androidRuntimePermissionsCallback (permissionsGranted, ptrToCallback);
    }
