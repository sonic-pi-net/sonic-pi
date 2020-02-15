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

package com.android.vending.billing;
/**
 * InAppBillingService is the service that provides in-app billing version 3 and beyond.
 * This service provides the following features:
 * 1. Provides a new API to get details of in-app items published for the app including
 *    price, type, title and description.
 * 2. The purchase flow is synchronous and purchase information is available immediately
 *    after it completes.
 * 3. Purchase information of in-app purchases is maintained within the Google Play system
 *    till the purchase is consumed.
 * 4. An API to consume a purchase of an inapp item. All purchases of one-time
 *    in-app items are consumable and thereafter can be purchased again.
 * 5. An API to get current purchases of the user immediately. This will not contain any
 *    consumed purchases.
 *
 * All calls will give a response code with the following possible values
 * RESULT_OK = 0 - success
 * RESULT_USER_CANCELED = 1 - User pressed back or canceled a dialog
 * RESULT_SERVICE_UNAVAILABLE = 2 - The network connection is down
 * RESULT_BILLING_UNAVAILABLE = 3 - This billing API version is not supported for the type requested
 * RESULT_ITEM_UNAVAILABLE = 4 - Requested SKU is not available for purchase
 * RESULT_DEVELOPER_ERROR = 5 - Invalid arguments provided to the API
 * RESULT_ERROR = 6 - Fatal error during the API action
 * RESULT_ITEM_ALREADY_OWNED = 7 - Failure to purchase since item is already owned
 * RESULT_ITEM_NOT_OWNED = 8 - Failure to consume since item is not owned
 */
public interface IInAppBillingService extends android.os.IInterface
    {
        /** Local-side IPC implementation stub class. */
        public static abstract class Stub extends android.os.Binder implements com.android.vending.billing.IInAppBillingService
        {
            private static final java.lang.String DESCRIPTOR = "com.android.vending.billing.IInAppBillingService";
            /** Construct the stub at attach it to the interface. */
            public Stub()
            {
                this.attachInterface(this, DESCRIPTOR);
            }
            /**
             * Cast an IBinder object into an com.android.vending.billing.IInAppBillingService interface,
             * generating a proxy if needed.
             */
            public static com.android.vending.billing.IInAppBillingService asInterface(android.os.IBinder obj)
            {
                if ((obj==null)) {
                    return null;
                }
                android.os.IInterface iin = obj.queryLocalInterface(DESCRIPTOR);
                if (((iin!=null)&&(iin instanceof com.android.vending.billing.IInAppBillingService))) {
                    return ((com.android.vending.billing.IInAppBillingService)iin);
                }
                return new com.android.vending.billing.IInAppBillingService.Stub.Proxy(obj);
            }
            @Override public android.os.IBinder asBinder()
            {
                return this;
            }
            @Override public boolean onTransact(int code, android.os.Parcel data, android.os.Parcel reply, int flags) throws android.os.RemoteException
            {
                switch (code)
                {
                    case INTERFACE_TRANSACTION:
                    {
                        reply.writeString(DESCRIPTOR);
                        return true;
                    }
                    case TRANSACTION_isBillingSupported:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        int _result = this.isBillingSupported(_arg0, _arg1, _arg2);
                        reply.writeNoException();
                        reply.writeInt(_result);
                        return true;
                    }
                    case TRANSACTION_getSkuDetails:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        android.os.Bundle _arg3;
                        if ((0!=data.readInt())) {
                            _arg3 = android.os.Bundle.CREATOR.createFromParcel(data);
                        }
                        else {
                            _arg3 = null;
                        }
                        android.os.Bundle _result = this.getSkuDetails(_arg0, _arg1, _arg2, _arg3);
                        reply.writeNoException();
                        if ((_result!=null)) {
                            reply.writeInt(1);
                            _result.writeToParcel(reply, android.os.Parcelable.PARCELABLE_WRITE_RETURN_VALUE);
                        }
                        else {
                            reply.writeInt(0);
                        }
                        return true;
                    }
                    case TRANSACTION_getBuyIntent:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        java.lang.String _arg3;
                        _arg3 = data.readString();
                        java.lang.String _arg4;
                        _arg4 = data.readString();
                        android.os.Bundle _result = this.getBuyIntent(_arg0, _arg1, _arg2, _arg3, _arg4);
                        reply.writeNoException();
                        if ((_result!=null)) {
                            reply.writeInt(1);
                            _result.writeToParcel(reply, android.os.Parcelable.PARCELABLE_WRITE_RETURN_VALUE);
                        }
                        else {
                            reply.writeInt(0);
                        }
                        return true;
                    }
                    case TRANSACTION_getPurchases:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        java.lang.String _arg3;
                        _arg3 = data.readString();
                        android.os.Bundle _result = this.getPurchases(_arg0, _arg1, _arg2, _arg3);
                        reply.writeNoException();
                        if ((_result!=null)) {
                            reply.writeInt(1);
                            _result.writeToParcel(reply, android.os.Parcelable.PARCELABLE_WRITE_RETURN_VALUE);
                        }
                        else {
                            reply.writeInt(0);
                        }
                        return true;
                    }
                    case TRANSACTION_consumePurchase:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        int _result = this.consumePurchase(_arg0, _arg1, _arg2);
                        reply.writeNoException();
                        reply.writeInt(_result);
                        return true;
                    }
                    case TRANSACTION_stub:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        int _result = this.stub(_arg0, _arg1, _arg2);
                        reply.writeNoException();
                        reply.writeInt(_result);
                        return true;
                    }
                    case TRANSACTION_getBuyIntentToReplaceSkus:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.util.List<java.lang.String> _arg2;
                        _arg2 = data.createStringArrayList();
                        java.lang.String _arg3;
                        _arg3 = data.readString();
                        java.lang.String _arg4;
                        _arg4 = data.readString();
                        java.lang.String _arg5;
                        _arg5 = data.readString();
                        android.os.Bundle _result = this.getBuyIntentToReplaceSkus(_arg0, _arg1, _arg2, _arg3, _arg4, _arg5);
                        reply.writeNoException();
                        if ((_result!=null)) {
                            reply.writeInt(1);
                            _result.writeToParcel(reply, android.os.Parcelable.PARCELABLE_WRITE_RETURN_VALUE);
                        }
                        else {
                            reply.writeInt(0);
                        }
                        return true;
                    }
                    case TRANSACTION_getBuyIntentExtraParams:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        java.lang.String _arg3;
                        _arg3 = data.readString();
                        java.lang.String _arg4;
                        _arg4 = data.readString();
                        android.os.Bundle _arg5;
                        if ((0!=data.readInt())) {
                            _arg5 = android.os.Bundle.CREATOR.createFromParcel(data);
                        }
                        else {
                            _arg5 = null;
                        }
                        android.os.Bundle _result = this.getBuyIntentExtraParams(_arg0, _arg1, _arg2, _arg3, _arg4, _arg5);
                        reply.writeNoException();
                        if ((_result!=null)) {
                            reply.writeInt(1);
                            _result.writeToParcel(reply, android.os.Parcelable.PARCELABLE_WRITE_RETURN_VALUE);
                        }
                        else {
                            reply.writeInt(0);
                        }
                        return true;
                    }
                    case TRANSACTION_getPurchaseHistory:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        java.lang.String _arg3;
                        _arg3 = data.readString();
                        android.os.Bundle _arg4;
                        if ((0!=data.readInt())) {
                            _arg4 = android.os.Bundle.CREATOR.createFromParcel(data);
                        }
                        else {
                            _arg4 = null;
                        }
                        android.os.Bundle _result = this.getPurchaseHistory(_arg0, _arg1, _arg2, _arg3, _arg4);
                        reply.writeNoException();
                        if ((_result!=null)) {
                            reply.writeInt(1);
                            _result.writeToParcel(reply, android.os.Parcelable.PARCELABLE_WRITE_RETURN_VALUE);
                        }
                        else {
                            reply.writeInt(0);
                        }
                        return true;
                    }
                    case TRANSACTION_isBillingSupportedExtraParams:
                    {
                        data.enforceInterface(DESCRIPTOR);
                        int _arg0;
                        _arg0 = data.readInt();
                        java.lang.String _arg1;
                        _arg1 = data.readString();
                        java.lang.String _arg2;
                        _arg2 = data.readString();
                        android.os.Bundle _arg3;
                        if ((0!=data.readInt())) {
                            _arg3 = android.os.Bundle.CREATOR.createFromParcel(data);
                        }
                        else {
                            _arg3 = null;
                        }
                        int _result = this.isBillingSupportedExtraParams(_arg0, _arg1, _arg2, _arg3);
                        reply.writeNoException();
                        reply.writeInt(_result);
                        return true;
                    }
                }
                return super.onTransact(code, data, reply, flags);
            }
            private static class Proxy implements com.android.vending.billing.IInAppBillingService
            {
                private android.os.IBinder mRemote;
                Proxy(android.os.IBinder remote)
                {
                    mRemote = remote;
                }
                @Override public android.os.IBinder asBinder()
                {
                    return mRemote;
                }
                public java.lang.String getInterfaceDescriptor()
                {
                    return DESCRIPTOR;
                }
                @Override public int isBillingSupported(int apiVersion, java.lang.String packageName, java.lang.String type) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    int _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(type);
                        mRemote.transact(Stub.TRANSACTION_isBillingSupported, _data, _reply, 0);
                        _reply.readException();
                        _result = _reply.readInt();
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                /**
                 * Provides details of a list of SKUs
                 * Given a list of SKUs of a valid type in the skusBundle, this returns a bundle
                 * with a list JSON strings containing the productId, price, title and description.
                 * This API can be called with a maximum of 20 SKUs.
                 * @param apiVersion billing API version that the app is using
                 * @param packageName the package name of the calling app
                 * @param type of the in-app items ("inapp" for one-time purchases
                 *        and "subs" for subscriptions)
                 * @param skusBundle bundle containing a StringArrayList of SKUs with key "ITEM_ID_LIST"
                 * @return Bundle containing the following key-value pairs
                 *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response codes
                 *                         on failures.
                 *         "DETAILS_LIST" with a StringArrayList containing purchase information
                 *                        in JSON format similar to:
                 *                        '{ "productId" : "exampleSku",
                 *                           "type" : "inapp",
                 *                           "price" : "$5.00",
                 *                           "price_currency": "USD",
                 *                           "price_amount_micros": 5000000,
                 *                           "title : "Example Title",
                 *                           "description" : "This is an example description" }'
                 */
                @Override public android.os.Bundle getSkuDetails(int apiVersion, java.lang.String packageName, java.lang.String type, android.os.Bundle skusBundle) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    android.os.Bundle _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(type);
                        if ((skusBundle!=null)) {
                            _data.writeInt(1);
                            skusBundle.writeToParcel(_data, 0);
                        }
                        else {
                            _data.writeInt(0);
                        }
                        mRemote.transact(Stub.TRANSACTION_getSkuDetails, _data, _reply, 0);
                        _reply.readException();
                        if ((0!=_reply.readInt())) {
                            _result = android.os.Bundle.CREATOR.createFromParcel(_reply);
                        }
                        else {
                            _result = null;
                        }
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                /**
                 * Returns a pending intent to launch the purchase flow for an in-app item by providing a SKU,
                 * the type, a unique purchase token and an optional developer payload.
                 * @param apiVersion billing API version that the app is using
                 * @param packageName package name of the calling app
                 * @param sku the SKU of the in-app item as published in the developer console
                 * @param type of the in-app item being purchased ("inapp" for one-time purchases
                 *        and "subs" for subscriptions)
                 * @param developerPayload optional argument to be sent back with the purchase information
                 * @return Bundle containing the following key-value pairs
                 *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response codes
                 *                         on failures.
                 *         "BUY_INTENT" - PendingIntent to start the purchase flow
                 *
                 * The Pending intent should be launched with startIntentSenderForResult. When purchase flow
                 * has completed, the onActivityResult() will give a resultCode of OK or CANCELED.
                 * If the purchase is successful, the result data will contain the following key-value pairs
                 *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response
                 *                         codes on failures.
                 *         "INAPP_PURCHASE_DATA" - String in JSON format similar to
                 *                                 '{"orderId":"12999763169054705758.1371079406387615",
                 *                                   "packageName":"com.example.app",
                 *                                   "productId":"exampleSku",
                 *                                   "purchaseTime":1345678900000,
                 *                                   "purchaseToken" : "122333444455555",
                 *                                   "developerPayload":"example developer payload" }'
                 *         "INAPP_DATA_SIGNATURE" - String containing the signature of the purchase data that
                 *                                  was signed with the private key of the developer
                 */
                @Override public android.os.Bundle getBuyIntent(int apiVersion, java.lang.String packageName, java.lang.String sku, java.lang.String type, java.lang.String developerPayload) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    android.os.Bundle _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(sku);
                        _data.writeString(type);
                        _data.writeString(developerPayload);
                        mRemote.transact(Stub.TRANSACTION_getBuyIntent, _data, _reply, 0);
                        _reply.readException();
                        if ((0!=_reply.readInt())) {
                            _result = android.os.Bundle.CREATOR.createFromParcel(_reply);
                        }
                        else {
                            _result = null;
                        }
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                /**
                 * Returns the current SKUs owned by the user of the type and package name specified along with
                 * purchase information and a signature of the data to be validated.
                 * This will return all SKUs that have been purchased in V3 and managed items purchased using
                 * V1 and V2 that have not been consumed.
                 * @param apiVersion billing API version that the app is using
                 * @param packageName package name of the calling app
                 * @param type of the in-app items being requested ("inapp" for one-time purchases
                 *        and "subs" for subscriptions)
                 * @param continuationToken to be set as null for the first call, if the number of owned
                 *        skus are too many, a continuationToken is returned in the response bundle.
                 *        This method can be called again with the continuation token to get the next set of
                 *        owned skus.
                 * @return Bundle containing the following key-value pairs
                 *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response codes
                 on failures.
                 *         "INAPP_PURCHASE_ITEM_LIST" - StringArrayList containing the list of SKUs
                 *         "INAPP_PURCHASE_DATA_LIST" - StringArrayList containing the purchase information
                 *         "INAPP_DATA_SIGNATURE_LIST"- StringArrayList containing the signatures
                 *                                      of the purchase information
                 *         "INAPP_CONTINUATION_TOKEN" - String containing a continuation token for the
                 *                                      next set of in-app purchases. Only set if the
                 *                                      user has more owned skus than the current list.
                 */
                @Override public android.os.Bundle getPurchases(int apiVersion, java.lang.String packageName, java.lang.String type, java.lang.String continuationToken) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    android.os.Bundle _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(type);
                        _data.writeString(continuationToken);
                        mRemote.transact(Stub.TRANSACTION_getPurchases, _data, _reply, 0);
                        _reply.readException();
                        if ((0!=_reply.readInt())) {
                            _result = android.os.Bundle.CREATOR.createFromParcel(_reply);
                        }
                        else {
                            _result = null;
                        }
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                @Override public int consumePurchase(int apiVersion, java.lang.String packageName, java.lang.String purchaseToken) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    int _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(purchaseToken);
                        mRemote.transact(Stub.TRANSACTION_consumePurchase, _data, _reply, 0);
                        _reply.readException();
                        _result = _reply.readInt();
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                @Override public int stub(int apiVersion, java.lang.String packageName, java.lang.String type) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    int _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(type);
                        mRemote.transact(Stub.TRANSACTION_stub, _data, _reply, 0);
                        _reply.readException();
                        _result = _reply.readInt();
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                /**
                 * Returns a pending intent to launch the purchase flow for upgrading or downgrading a
                 * subscription. The existing owned SKU(s) should be provided along with the new SKU that
                 * the user is upgrading or downgrading to.
                 * @param apiVersion billing API version that the app is using, must be 5 or later
                 * @param packageName package name of the calling app
                 * @param oldSkus the SKU(s) that the user is upgrading or downgrading from,
                 *        if null or empty this method will behave like {@link #getBuyIntent}
                 * @param newSku the SKU that the user is upgrading or downgrading to
                 * @param type of the item being purchased, currently must be "subs"
                 * @param developerPayload optional argument to be sent back with the purchase information
                 * @return Bundle containing the following key-value pairs
                 *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response codes
                 *                         on failures.
                 *         "BUY_INTENT" - PendingIntent to start the purchase flow
                 *
                 * The Pending intent should be launched with startIntentSenderForResult. When purchase flow
                 * has completed, the onActivityResult() will give a resultCode of OK or CANCELED.
                 * If the purchase is successful, the result data will contain the following key-value pairs
                 *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response
                 *                         codes on failures.
                 *         "INAPP_PURCHASE_DATA" - String in JSON format similar to
                 *                                 '{"orderId":"12999763169054705758.1371079406387615",
                 *                                   "packageName":"com.example.app",
                 *                                   "productId":"exampleSku",
                 *                                   "purchaseTime":1345678900000,
                 *                                   "purchaseToken" : "122333444455555",
                 *                                   "developerPayload":"example developer payload" }'
                 *         "INAPP_DATA_SIGNATURE" - String containing the signature of the purchase data that
                 *                                  was signed with the private key of the developer
                 */
                @Override public android.os.Bundle getBuyIntentToReplaceSkus(int apiVersion, java.lang.String packageName, java.util.List<java.lang.String> oldSkus, java.lang.String newSku, java.lang.String type, java.lang.String developerPayload) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    android.os.Bundle _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeStringList(oldSkus);
                        _data.writeString(newSku);
                        _data.writeString(type);
                        _data.writeString(developerPayload);
                        mRemote.transact(Stub.TRANSACTION_getBuyIntentToReplaceSkus, _data, _reply, 0);
                        _reply.readException();
                        if ((0!=_reply.readInt())) {
                            _result = android.os.Bundle.CREATOR.createFromParcel(_reply);
                        }
                        else {
                            _result = null;
                        }
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                /**
                 * Returns a pending intent to launch the purchase flow for an in-app item. This method is
                 * a variant of the {@link #getBuyIntent} method and takes an additional {@code extraParams}
                 * parameter. This parameter is a Bundle of optional keys and values that affect the
                 * operation of the method.
                 * @param apiVersion billing API version that the app is using, must be 6 or later
                 * @param packageName package name of the calling app
                 * @param sku the SKU of the in-app item as published in the developer console
                 * @param type of the in-app item being purchased ("inapp" for one-time purchases
                 *        and "subs" for subscriptions)
                 * @param developerPayload optional argument to be sent back with the purchase information
                 * @extraParams a Bundle with the following optional keys:
                 *        "skusToReplace" - List<String> - an optional list of SKUs that the user is
                 *                          upgrading or downgrading from.
                 *                          Pass this field if the purchase is upgrading or downgrading
                 *                          existing subscriptions.
                 *                          The specified SKUs are replaced with the SKUs that the user is
                 *                          purchasing. Google Play replaces the specified SKUs at the start of
                 *                          the next billing cycle.
                 * "replaceSkusProration" - Boolean - whether the user should be credited for any unused
                 *                          subscription time on the SKUs they are upgrading or downgrading.
                 *                          If you set this field to true, Google Play swaps out the old SKUs
                 *                          and credits the user with the unused value of their subscription
                 *                          time on a pro-rated basis.
                 *                          Google Play applies this credit to the new subscription, and does
                 *                          not begin billing the user for the new subscription until after
                 *                          the credit is used up.
                 *                          If you set this field to false, the user does not receive credit for
                 *                          any unused subscription time and the recurrence date does not
                 *                          change.
                 *                          Default value is true. Ignored if you do not pass skusToReplace.
                 *            "accountId" - String - an optional obfuscated string that is uniquely
                 *                          associated with the user's account in your app.
                 *                          If you pass this value, Google Play can use it to detect irregular
                 *                          activity, such as many devices making purchases on the same
                 *                          account in a short period of time.
                 *                          Do not use the developer ID or the user's Google ID for this field.
                 *                          In addition, this field should not contain the user's ID in
                 *                          cleartext.
                 *                          We recommend that you use a one-way hash to generate a string from
                 *                          the user's ID, and store the hashed string in this field.
                 *                   "vr" - Boolean - an optional flag indicating whether the returned intent
                 *                          should start a VR purchase flow. The apiVersion must also be 7 or
                 *                          later to use this flag.
                 */
                @Override public android.os.Bundle getBuyIntentExtraParams(int apiVersion, java.lang.String packageName, java.lang.String sku, java.lang.String type, java.lang.String developerPayload, android.os.Bundle extraParams) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    android.os.Bundle _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(sku);
                        _data.writeString(type);
                        _data.writeString(developerPayload);
                        if ((extraParams!=null)) {
                            _data.writeInt(1);
                            extraParams.writeToParcel(_data, 0);
                        }
                        else {
                            _data.writeInt(0);
                        }
                        mRemote.transact(Stub.TRANSACTION_getBuyIntentExtraParams, _data, _reply, 0);
                        _reply.readException();
                        if ((0!=_reply.readInt())) {
                            _result = android.os.Bundle.CREATOR.createFromParcel(_reply);
                        }
                        else {
                            _result = null;
                        }
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                /**
                 * Returns the most recent purchase made by the user for each SKU, even if that purchase is
                 * expired, canceled, or consumed.
                 * @param apiVersion billing API version that the app is using, must be 6 or later
                 * @param packageName package name of the calling app
                 * @param type of the in-app items being requested ("inapp" for one-time purchases
                 *        and "subs" for subscriptions)
                 * @param continuationToken to be set as null for the first call, if the number of owned
                 *        skus is too large, a continuationToken is returned in the response bundle.
                 *        This method can be called again with the continuation token to get the next set of
                 *        owned skus.
                 * @param extraParams a Bundle with extra params that would be appended into http request
                 *        query string. Not used at this moment. Reserved for future functionality.
                 * @return Bundle containing the following key-value pairs
                 *         "RESPONSE_CODE" with int value: RESULT_OK(0) if success,
                 *         {@link IabHelper#BILLING_RESPONSE_RESULT_*} response codes on failures.
                 *
                 *         "INAPP_PURCHASE_ITEM_LIST" - ArrayList<String> containing the list of SKUs
                 *         "INAPP_PURCHASE_DATA_LIST" - ArrayList<String> containing the purchase information
                 *         "INAPP_DATA_SIGNATURE_LIST"- ArrayList<String> containing the signatures
                 *                                      of the purchase information
                 *         "INAPP_CONTINUATION_TOKEN" - String containing a continuation token for the
                 *                                      next set of in-app purchases. Only set if the
                 *                                      user has more owned skus than the current list.
                 */
                @Override public android.os.Bundle getPurchaseHistory(int apiVersion, java.lang.String packageName, java.lang.String type, java.lang.String continuationToken, android.os.Bundle extraParams) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    android.os.Bundle _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(type);
                        _data.writeString(continuationToken);
                        if ((extraParams!=null)) {
                            _data.writeInt(1);
                            extraParams.writeToParcel(_data, 0);
                        }
                        else {
                            _data.writeInt(0);
                        }
                        mRemote.transact(Stub.TRANSACTION_getPurchaseHistory, _data, _reply, 0);
                        _reply.readException();
                        if ((0!=_reply.readInt())) {
                            _result = android.os.Bundle.CREATOR.createFromParcel(_reply);
                        }
                        else {
                            _result = null;
                        }
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
                @Override public int isBillingSupportedExtraParams(int apiVersion, java.lang.String packageName, java.lang.String type, android.os.Bundle extraParams) throws android.os.RemoteException
                {
                    android.os.Parcel _data = android.os.Parcel.obtain();
                    android.os.Parcel _reply = android.os.Parcel.obtain();
                    int _result;
                    try {
                        _data.writeInterfaceToken(DESCRIPTOR);
                        _data.writeInt(apiVersion);
                        _data.writeString(packageName);
                        _data.writeString(type);
                        if ((extraParams!=null)) {
                            _data.writeInt(1);
                            extraParams.writeToParcel(_data, 0);
                        }
                        else {
                            _data.writeInt(0);
                        }
                        mRemote.transact(Stub.TRANSACTION_isBillingSupportedExtraParams, _data, _reply, 0);
                        _reply.readException();
                        _result = _reply.readInt();
                    }
                    finally {
                        _reply.recycle();
                        _data.recycle();
                    }
                    return _result;
                }
            }
            static final int TRANSACTION_isBillingSupported = (android.os.IBinder.FIRST_CALL_TRANSACTION + 0);
            static final int TRANSACTION_getSkuDetails = (android.os.IBinder.FIRST_CALL_TRANSACTION + 1);
            static final int TRANSACTION_getBuyIntent = (android.os.IBinder.FIRST_CALL_TRANSACTION + 2);
            static final int TRANSACTION_getPurchases = (android.os.IBinder.FIRST_CALL_TRANSACTION + 3);
            static final int TRANSACTION_consumePurchase = (android.os.IBinder.FIRST_CALL_TRANSACTION + 4);
            static final int TRANSACTION_stub = (android.os.IBinder.FIRST_CALL_TRANSACTION + 5);
            static final int TRANSACTION_getBuyIntentToReplaceSkus = (android.os.IBinder.FIRST_CALL_TRANSACTION + 6);
            static final int TRANSACTION_getBuyIntentExtraParams = (android.os.IBinder.FIRST_CALL_TRANSACTION + 7);
            static final int TRANSACTION_getPurchaseHistory = (android.os.IBinder.FIRST_CALL_TRANSACTION + 8);
            static final int TRANSACTION_isBillingSupportedExtraParams = (android.os.IBinder.FIRST_CALL_TRANSACTION + 9);
        }
        public int isBillingSupported(int apiVersion, java.lang.String packageName, java.lang.String type) throws android.os.RemoteException;
        /**
         * Provides details of a list of SKUs
         * Given a list of SKUs of a valid type in the skusBundle, this returns a bundle
         * with a list JSON strings containing the productId, price, title and description.
         * This API can be called with a maximum of 20 SKUs.
         * @param apiVersion billing API version that the app is using
         * @param packageName the package name of the calling app
         * @param type of the in-app items ("inapp" for one-time purchases
         *        and "subs" for subscriptions)
         * @param skusBundle bundle containing a StringArrayList of SKUs with key "ITEM_ID_LIST"
         * @return Bundle containing the following key-value pairs
         *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response codes
         *                         on failures.
         *         "DETAILS_LIST" with a StringArrayList containing purchase information
         *                        in JSON format similar to:
         *                        '{ "productId" : "exampleSku",
         *                           "type" : "inapp",
         *                           "price" : "$5.00",
         *                           "price_currency": "USD",
         *                           "price_amount_micros": 5000000,
         *                           "title : "Example Title",
         *                           "description" : "This is an example description" }'
         */
        public android.os.Bundle getSkuDetails(int apiVersion, java.lang.String packageName, java.lang.String type, android.os.Bundle skusBundle) throws android.os.RemoteException;
        /**
         * Returns a pending intent to launch the purchase flow for an in-app item by providing a SKU,
         * the type, a unique purchase token and an optional developer payload.
         * @param apiVersion billing API version that the app is using
         * @param packageName package name of the calling app
         * @param sku the SKU of the in-app item as published in the developer console
         * @param type of the in-app item being purchased ("inapp" for one-time purchases
         *        and "subs" for subscriptions)
         * @param developerPayload optional argument to be sent back with the purchase information
         * @return Bundle containing the following key-value pairs
         *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response codes
         *                         on failures.
         *         "BUY_INTENT" - PendingIntent to start the purchase flow
         *
         * The Pending intent should be launched with startIntentSenderForResult. When purchase flow
         * has completed, the onActivityResult() will give a resultCode of OK or CANCELED.
         * If the purchase is successful, the result data will contain the following key-value pairs
         *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response
         *                         codes on failures.
         *         "INAPP_PURCHASE_DATA" - String in JSON format similar to
         *                                 '{"orderId":"12999763169054705758.1371079406387615",
         *                                   "packageName":"com.example.app",
         *                                   "productId":"exampleSku",
         *                                   "purchaseTime":1345678900000,
         *                                   "purchaseToken" : "122333444455555",
         *                                   "developerPayload":"example developer payload" }'
         *         "INAPP_DATA_SIGNATURE" - String containing the signature of the purchase data that
         *                                  was signed with the private key of the developer
         */
        public android.os.Bundle getBuyIntent(int apiVersion, java.lang.String packageName, java.lang.String sku, java.lang.String type, java.lang.String developerPayload) throws android.os.RemoteException;
        /**
         * Returns the current SKUs owned by the user of the type and package name specified along with
         * purchase information and a signature of the data to be validated.
         * This will return all SKUs that have been purchased in V3 and managed items purchased using
         * V1 and V2 that have not been consumed.
         * @param apiVersion billing API version that the app is using
         * @param packageName package name of the calling app
         * @param type of the in-app items being requested ("inapp" for one-time purchases
         *        and "subs" for subscriptions)
         * @param continuationToken to be set as null for the first call, if the number of owned
         *        skus are too many, a continuationToken is returned in the response bundle.
         *        This method can be called again with the continuation token to get the next set of
         *        owned skus.
         * @return Bundle containing the following key-value pairs
         *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response codes
         on failures.
         *         "INAPP_PURCHASE_ITEM_LIST" - StringArrayList containing the list of SKUs
         *         "INAPP_PURCHASE_DATA_LIST" - StringArrayList containing the purchase information
         *         "INAPP_DATA_SIGNATURE_LIST"- StringArrayList containing the signatures
         *                                      of the purchase information
         *         "INAPP_CONTINUATION_TOKEN" - String containing a continuation token for the
         *                                      next set of in-app purchases. Only set if the
         *                                      user has more owned skus than the current list.
         */
        public android.os.Bundle getPurchases(int apiVersion, java.lang.String packageName, java.lang.String type, java.lang.String continuationToken) throws android.os.RemoteException;
        public int consumePurchase(int apiVersion, java.lang.String packageName, java.lang.String purchaseToken) throws android.os.RemoteException;
        public int stub(int apiVersion, java.lang.String packageName, java.lang.String type) throws android.os.RemoteException;
        /**
         * Returns a pending intent to launch the purchase flow for upgrading or downgrading a
         * subscription. The existing owned SKU(s) should be provided along with the new SKU that
         * the user is upgrading or downgrading to.
         * @param apiVersion billing API version that the app is using, must be 5 or later
         * @param packageName package name of the calling app
         * @param oldSkus the SKU(s) that the user is upgrading or downgrading from,
         *        if null or empty this method will behave like {@link #getBuyIntent}
         * @param newSku the SKU that the user is upgrading or downgrading to
         * @param type of the item being purchased, currently must be "subs"
         * @param developerPayload optional argument to be sent back with the purchase information
         * @return Bundle containing the following key-value pairs
         *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response codes
         *                         on failures.
         *         "BUY_INTENT" - PendingIntent to start the purchase flow
         *
         * The Pending intent should be launched with startIntentSenderForResult. When purchase flow
         * has completed, the onActivityResult() will give a resultCode of OK or CANCELED.
         * If the purchase is successful, the result data will contain the following key-value pairs
         *         "RESPONSE_CODE" with int value, RESULT_OK(0) if success, appropriate response
         *                         codes on failures.
         *         "INAPP_PURCHASE_DATA" - String in JSON format similar to
         *                                 '{"orderId":"12999763169054705758.1371079406387615",
         *                                   "packageName":"com.example.app",
         *                                   "productId":"exampleSku",
         *                                   "purchaseTime":1345678900000,
         *                                   "purchaseToken" : "122333444455555",
         *                                   "developerPayload":"example developer payload" }'
         *         "INAPP_DATA_SIGNATURE" - String containing the signature of the purchase data that
         *                                  was signed with the private key of the developer
         */
        public android.os.Bundle getBuyIntentToReplaceSkus(int apiVersion, java.lang.String packageName, java.util.List<java.lang.String> oldSkus, java.lang.String newSku, java.lang.String type, java.lang.String developerPayload) throws android.os.RemoteException;
        /**
         * Returns a pending intent to launch the purchase flow for an in-app item. This method is
         * a variant of the {@link #getBuyIntent} method and takes an additional {@code extraParams}
         * parameter. This parameter is a Bundle of optional keys and values that affect the
         * operation of the method.
         * @param apiVersion billing API version that the app is using, must be 6 or later
         * @param packageName package name of the calling app
         * @param sku the SKU of the in-app item as published in the developer console
         * @param type of the in-app item being purchased ("inapp" for one-time purchases
         *        and "subs" for subscriptions)
         * @param developerPayload optional argument to be sent back with the purchase information
         * @extraParams a Bundle with the following optional keys:
         *        "skusToReplace" - List<String> - an optional list of SKUs that the user is
         *                          upgrading or downgrading from.
         *                          Pass this field if the purchase is upgrading or downgrading
         *                          existing subscriptions.
         *                          The specified SKUs are replaced with the SKUs that the user is
         *                          purchasing. Google Play replaces the specified SKUs at the start of
         *                          the next billing cycle.
         * "replaceSkusProration" - Boolean - whether the user should be credited for any unused
         *                          subscription time on the SKUs they are upgrading or downgrading.
         *                          If you set this field to true, Google Play swaps out the old SKUs
         *                          and credits the user with the unused value of their subscription
         *                          time on a pro-rated basis.
         *                          Google Play applies this credit to the new subscription, and does
         *                          not begin billing the user for the new subscription until after
         *                          the credit is used up.
         *                          If you set this field to false, the user does not receive credit for
         *                          any unused subscription time and the recurrence date does not
         *                          change.
         *                          Default value is true. Ignored if you do not pass skusToReplace.
         *            "accountId" - String - an optional obfuscated string that is uniquely
         *                          associated with the user's account in your app.
         *                          If you pass this value, Google Play can use it to detect irregular
         *                          activity, such as many devices making purchases on the same
         *                          account in a short period of time.
         *                          Do not use the developer ID or the user's Google ID for this field.
         *                          In addition, this field should not contain the user's ID in
         *                          cleartext.
         *                          We recommend that you use a one-way hash to generate a string from
         *                          the user's ID, and store the hashed string in this field.
         *                   "vr" - Boolean - an optional flag indicating whether the returned intent
         *                          should start a VR purchase flow. The apiVersion must also be 7 or
         *                          later to use this flag.
         */
        public android.os.Bundle getBuyIntentExtraParams(int apiVersion, java.lang.String packageName, java.lang.String sku, java.lang.String type, java.lang.String developerPayload, android.os.Bundle extraParams) throws android.os.RemoteException;
        /**
         * Returns the most recent purchase made by the user for each SKU, even if that purchase is
         * expired, canceled, or consumed.
         * @param apiVersion billing API version that the app is using, must be 6 or later
         * @param packageName package name of the calling app
         * @param type of the in-app items being requested ("inapp" for one-time purchases
         *        and "subs" for subscriptions)
         * @param continuationToken to be set as null for the first call, if the number of owned
         *        skus is too large, a continuationToken is returned in the response bundle.
         *        This method can be called again with the continuation token to get the next set of
         *        owned skus.
         * @param extraParams a Bundle with extra params that would be appended into http request
         *        query string. Not used at this moment. Reserved for future functionality.
         * @return Bundle containing the following key-value pairs
         *         "RESPONSE_CODE" with int value: RESULT_OK(0) if success,
         *         {@link IabHelper#BILLING_RESPONSE_RESULT_*} response codes on failures.
         *
         *         "INAPP_PURCHASE_ITEM_LIST" - ArrayList<String> containing the list of SKUs
         *         "INAPP_PURCHASE_DATA_LIST" - ArrayList<String> containing the purchase information
         *         "INAPP_DATA_SIGNATURE_LIST"- ArrayList<String> containing the signatures
         *                                      of the purchase information
         *         "INAPP_CONTINUATION_TOKEN" - String containing a continuation token for the
         *                                      next set of in-app purchases. Only set if the
         *                                      user has more owned skus than the current list.
         */
        public android.os.Bundle getPurchaseHistory(int apiVersion, java.lang.String packageName, java.lang.String type, java.lang.String continuationToken, android.os.Bundle extraParams) throws android.os.RemoteException;
        public int isBillingSupportedExtraParams(int apiVersion, java.lang.String packageName, java.lang.String type, android.os.Bundle extraParams) throws android.os.RemoteException;
    }
