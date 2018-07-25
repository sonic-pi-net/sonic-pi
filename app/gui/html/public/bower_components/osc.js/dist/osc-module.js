/*! osc.js 2.2.0, Copyright 2017 Colin Clark | github.com/colinbdclark/osc.js */

(function (root, factory) {
    if (typeof exports === "object") {
        // We're in a CommonJS-style loader.
        root.osc = exports;
        factory(exports, require("slip"), require("EventEmitter"), require("long"));
    } else if (typeof define === "function" && define.amd) {
        // We're in an AMD-style loader.
        define(["exports", "slip", "EventEmitter", "long"], function (exports, slip, EventEmitter, Long) {
            root.osc = exports;
            return (root.osc, factory(exports, slip, EventEmitter, Long));
        });
    } else {
        // Plain old browser.
        root.osc = {};
        factory(root.osc, slip, EventEmitter);
    }
}(this, function (exports, slip, EventEmitter, Long) {
;/*
 * osc.js: An Open Sound Control library for JavaScript that works in both the browser and Node.js
 *
 * Copyright 2014-2016, Colin Clark
 * Licensed under the MIT and GPL 3 licenses.
 */

/* global require, module, process, Buffer, dcodeIO */

var osc = osc || {};

(function () {

    "use strict";

    osc.SECS_70YRS = 2208988800;
    osc.TWO_32 = 4294967296;

    osc.defaults = {
        metadata: false,
        unpackSingleArgs: true
    };

    // Unsupported, non-API property.
    osc.isCommonJS = typeof module !== "undefined" && module.exports ? true : false;

    // Unsupported, non-API property.
    osc.isNode = osc.isCommonJS && typeof window === "undefined";

    // Unsupported, non-API property.
    osc.isElectron = typeof process !== "undefined" &&
        process.versions && process.versions.electron ? true : false;

    // Unsupported, non-API property.
    osc.isBufferEnv = osc.isNode || osc.isElectron;

    // Unsupported, non-API function.
    osc.isArray = function (obj) {
        return obj && Object.prototype.toString.call(obj) === "[object Array]";
    };

    // Unsupported, non-API function
    osc.isTypedArrayView = function (obj) {
        return obj.buffer && obj.buffer instanceof ArrayBuffer;
    };

    // Unsupported, non-API function
    osc.isBuffer = function (obj) {
        return osc.isBufferEnv && obj instanceof Buffer;
    };

    // Private instance of the optional Long dependency.
    var Long = typeof dcodeIO !== "undefined" ? dcodeIO.Long :
        typeof Long !== "undefined" ? Long :
        osc.isNode ? require("long") : undefined;

    /**
     * Wraps the specified object in a DataView.
     *
     * @param {Array-like} obj the object to wrap in a DataView instance
     * @return {DataView} the DataView object
     */
    // Unsupported, non-API function.
    osc.dataView = function (obj, offset, length) {
        if (obj.buffer) {
            return new DataView(obj.buffer, offset, length);
        }

        if (obj instanceof ArrayBuffer) {
            return new DataView(obj, offset, length);
        }

        return new DataView(new Uint8Array(obj), offset, length);
    };

    /**
     * Takes an ArrayBuffer, TypedArray, DataView, Buffer, or array-like object
     * and returns a Uint8Array view of it.
     *
     * Throws an error if the object isn't suitably array-like.
     *
     * @param {Array-like or Array-wrapping} obj an array-like or array-wrapping object
     * @returns {Uint8Array} a typed array of octets
     */
    // Unsupported, non-API function.
    osc.byteArray = function (obj) {
        if (obj instanceof Uint8Array) {
            return obj;
        }

        var buf = obj.buffer ? obj.buffer : obj;

        if (!(buf instanceof ArrayBuffer) && (typeof buf.length === "undefined" || typeof buf === "string")) {
            throw new Error("Can't wrap a non-array-like object as Uint8Array. Object was: " +
                JSON.stringify(obj, null, 2));
        }


        // TODO gh-39: This is a potentially unsafe algorithm;
        // if we're getting anything other than a TypedArrayView (such as a DataView),
        // we really need to determine the range of the view it is viewing.
        return new Uint8Array(buf);
    };

    /**
     * Takes an ArrayBuffer, TypedArray, DataView, or array-like object
     * and returns a native buffer object
     * (i.e. in Node.js, a Buffer object and in the browser, a Uint8Array).
     *
     * Throws an error if the object isn't suitably array-like.
     *
     * @param {Array-like or Array-wrapping} obj an array-like or array-wrapping object
     * @returns {Buffer|Uint8Array} a buffer object
     */
    // Unsupported, non-API function.
    osc.nativeBuffer = function (obj) {
        if (osc.isBufferEnv) {
            return osc.isBuffer(obj) ? obj :
                new Buffer(obj.buffer ? obj : new Uint8Array(obj));
        }

        return osc.isTypedArrayView(obj) ? obj : new Uint8Array(obj);
    };

    // Unsupported, non-API function
    osc.copyByteArray = function (source, target, offset) {
        if (osc.isTypedArrayView(source) && osc.isTypedArrayView(target)) {
            target.set(source, offset);
        } else {
            var start = offset === undefined ? 0 : offset,
                len = Math.min(target.length - offset, source.length);

            for (var i = 0, j = start; i < len; i++, j++) {
                target[j] = source[i];
            }
        }

        return target;
    };

    /**
     * Reads an OSC-formatted string.
     *
     * @param {DataView} dv a DataView containing the raw bytes of the OSC string
     * @param {Object} offsetState an offsetState object used to store the current offset index
     * @return {String} the JavaScript String that was read
     */
    osc.readString = function (dv, offsetState) {
        var charCodes = [],
            idx = offsetState.idx;

        for (; idx < dv.byteLength; idx++) {
            var charCode = dv.getUint8(idx);
            if (charCode !== 0) {
                charCodes.push(charCode);
            } else {
                idx++;
                break;
            }
        }

        // Round to the nearest 4-byte block.
        idx = (idx + 3) & ~0x03;
        offsetState.idx = idx;

        return String.fromCharCode.apply(null, charCodes);
    };

    /**
     * Writes a JavaScript string as an OSC-formatted string.
     *
     * @param {String} str the string to write
     * @return {Uint8Array} a buffer containing the OSC-formatted string
     */
    osc.writeString = function (str) {
        var terminated = str + "\u0000",
            len = terminated.length,
            paddedLen = (len + 3) & ~0x03,
            arr = new Uint8Array(paddedLen);

        for (var i = 0; i < terminated.length; i++) {
            var charCode = terminated.charCodeAt(i);
            arr[i] = charCode;
        }

        return arr;
    };

    // Unsupported, non-API function.
    osc.readPrimitive = function (dv, readerName, numBytes, offsetState) {
        var val = dv[readerName](offsetState.idx, false);
        offsetState.idx += numBytes;

        return val;
    };

    // Unsupported, non-API function.
    osc.writePrimitive = function (val, dv, writerName, numBytes, offset) {
        offset = offset === undefined ? 0 : offset;

        var arr;
        if (!dv) {
            arr = new Uint8Array(numBytes);
            dv = new DataView(arr.buffer);
        } else {
            arr = new Uint8Array(dv.buffer);
        }

        dv[writerName](offset, val, false);

        return arr;
    };

    /**
     * Reads an OSC int32 ("i") value.
     *
     * @param {DataView} dv a DataView containing the raw bytes
     * @param {Object} offsetState an offsetState object used to store the current offset index into dv
     * @return {Number} the number that was read
     */
    osc.readInt32 = function (dv, offsetState) {
        return osc.readPrimitive(dv, "getInt32", 4, offsetState);
    };

    /**
     * Writes an OSC int32 ("i") value.
     *
     * @param {Number} val the number to write
     * @param {DataView} [dv] a DataView instance to write the number into
     * @param {Number} [offset] an offset into dv
     */
    osc.writeInt32 = function (val, dv, offset) {
        return osc.writePrimitive(val, dv, "setInt32", 4, offset);
    };

    /**
     * Reads an OSC int64 ("h") value.
     *
     * @param {DataView} dv a DataView containing the raw bytes
     * @param {Object} offsetState an offsetState object used to store the current offset index into dv
     * @return {Number} the number that was read
     */
    osc.readInt64 = function (dv, offsetState) {
        var high = osc.readPrimitive(dv, "getInt32", 4, offsetState),
            low = osc.readPrimitive(dv, "getInt32", 4, offsetState);

        if (Long) {
            return new Long(low, high);
        } else {
            return {
                high: high,
                low: low,
                unsigned: false
            };
        }
    };

    /**
     * Writes an OSC int64 ("h") value.
     *
     * @param {Number} val the number to write
     * @param {DataView} [dv] a DataView instance to write the number into
     * @param {Number} [offset] an offset into dv
     */
    osc.writeInt64 = function (val, dv, offset) {
        var arr = new Uint8Array(8);
        arr.set(osc.writePrimitive(val.high, dv, "setInt32", 4, offset), 0);
        arr.set(osc.writePrimitive(val.low,  dv, "setInt32", 4, offset + 4), 4);
        return arr;
    };

    /**
     * Reads an OSC float32 ("f") value.
     *
     * @param {DataView} dv a DataView containing the raw bytes
     * @param {Object} offsetState an offsetState object used to store the current offset index into dv
     * @return {Number} the number that was read
     */
    osc.readFloat32 = function (dv, offsetState) {
        return osc.readPrimitive(dv, "getFloat32", 4, offsetState);
    };

    /**
     * Writes an OSC float32 ("f") value.
     *
     * @param {Number} val the number to write
     * @param {DataView} [dv] a DataView instance to write the number into
     * @param {Number} [offset] an offset into dv
     */
    osc.writeFloat32 = function (val, dv, offset) {
        return osc.writePrimitive(val, dv, "setFloat32", 4, offset);
    };

    /**
     * Reads an OSC float64 ("d") value.
     *
     * @param {DataView} dv a DataView containing the raw bytes
     * @param {Object} offsetState an offsetState object used to store the current offset index into dv
     * @return {Number} the number that was read
     */
    osc.readFloat64 = function (dv, offsetState) {
        return osc.readPrimitive(dv, "getFloat64", 8, offsetState);
    };

    /**
     * Writes an OSC float64 ("d") value.
     *
     * @param {Number} val the number to write
     * @param {DataView} [dv] a DataView instance to write the number into
     * @param {Number} [offset] an offset into dv
     */
    osc.writeFloat64 = function (val, dv, offset) {
        return osc.writePrimitive(val, dv, "setFloat64", 8, offset);
    };

    /**
     * Reads an OSC 32-bit ASCII character ("c") value.
     *
     * @param {DataView} dv a DataView containing the raw bytes
     * @param {Object} offsetState an offsetState object used to store the current offset index into dv
     * @return {String} a string containing the read character
     */
    osc.readChar32 = function (dv, offsetState) {
        var charCode = osc.readPrimitive(dv, "getUint32", 4, offsetState);
        return String.fromCharCode(charCode);
    };

    /**
     * Writes an OSC 32-bit ASCII character ("c") value.
     *
     * @param {String} str the string from which the first character will be written
     * @param {DataView} [dv] a DataView instance to write the character into
     * @param {Number} [offset] an offset into dv
     * @return {String} a string containing the read character
     */
    osc.writeChar32 = function (str, dv, offset) {
        var charCode = str.charCodeAt(0);
        if (charCode === undefined || charCode < -1) {
            return undefined;
        }

        return osc.writePrimitive(charCode, dv, "setUint32", 4, offset);
    };

    /**
     * Reads an OSC blob ("b") (i.e. a Uint8Array).
     *
     * @param {DataView} dv a DataView instance to read from
     * @param {Object} offsetState an offsetState object used to store the current offset index into dv
     * @return {Uint8Array} the data that was read
     */
    osc.readBlob = function (dv, offsetState) {
        var len = osc.readInt32(dv, offsetState),
            paddedLen = (len + 3) & ~0x03,
            blob = new Uint8Array(dv.buffer, offsetState.idx, len);

        offsetState.idx += paddedLen;

        return blob;
    };

    /**
     * Writes a raw collection of bytes to a new ArrayBuffer.
     *
     * @param {Array-like} data a collection of octets
     * @return {ArrayBuffer} a buffer containing the OSC-formatted blob
     */
    osc.writeBlob = function (data) {
        data = osc.byteArray(data);

        var len = data.byteLength,
            paddedLen = (len + 3) & ~0x03,
            offset = 4, // Extra 4 bytes is for the size.
            blobLen = paddedLen + offset,
            arr = new Uint8Array(blobLen),
            dv = new DataView(arr.buffer);

        // Write the size.
        osc.writeInt32(len, dv);

        // Since we're writing to a real ArrayBuffer,
        // we don't need to pad the remaining bytes.
        arr.set(data, offset);

        return arr;
    };

    /**
     * Reads an OSC 4-byte MIDI message.
     *
     * @param {DataView} dv the DataView instance to read from
     * @param {Object} offsetState an offsetState object used to store the current offset index into dv
     * @return {Uint8Array} an array containing (in order) the port ID, status, data1 and data1 bytes
     */
    osc.readMIDIBytes = function (dv, offsetState) {
        var midi = new Uint8Array(dv.buffer, offsetState.idx, 4);
        offsetState.idx += 4;

        return midi;
    };

    /**
     * Writes an OSC 4-byte MIDI message.
     *
     * @param {Array-like} bytes a 4-element array consisting of the port ID, status, data1 and data1 bytes
     * @return {Uint8Array} the written message
     */
    osc.writeMIDIBytes = function (bytes) {
        bytes = osc.byteArray(bytes);

        var arr = new Uint8Array(4);
        arr.set(bytes);

        return arr;
    };

    /**
     * Reads an OSC RGBA colour value.
     *
     * @param {DataView} dv the DataView instance to read from
     * @param {Object} offsetState an offsetState object used to store the current offset index into dv
     * @return {Object} a colour object containing r, g, b, and a properties
     */
    osc.readColor = function (dv, offsetState) {
        var bytes = new Uint8Array(dv.buffer, offsetState.idx, 4),
            alpha = bytes[3] / 255;

        offsetState.idx += 4;

        return {
            r: bytes[0],
            g: bytes[1],
            b: bytes[2],
            a: alpha
        };
    };

    /**
     * Writes an OSC RGBA colour value.
     *
     * @param {Object} color a colour object containing r, g, b, and a properties
     * @return {Uint8Array} a byte array containing the written color
     */
    osc.writeColor = function (color) {
        var alpha = Math.round(color.a * 255),
            arr = new Uint8Array([color.r, color.g, color.b, alpha]);

        return arr;
    };

    /**
     * Reads an OSC true ("T") value by directly returning the JavaScript Boolean "true".
     */
    osc.readTrue = function () {
        return true;
    };

    /**
     * Reads an OSC false ("F") value by directly returning the JavaScript Boolean "false".
     */
    osc.readFalse = function () {
        return false;
    };

    /**
     * Reads an OSC nil ("N") value by directly returning the JavaScript "null" value.
     */
    osc.readNull = function () {
        return null;
    };

    /**
     * Reads an OSC impulse/bang/infinitum ("I") value by directly returning 1.0.
     */
    osc.readImpulse = function () {
        return 1.0;
    };

    /**
     * Reads an OSC time tag ("t").
     *
     * @param {DataView} dv the DataView instance to read from
     * @param {Object} offsetState an offset state object containing the current index into dv
     * @param {Object} a time tag object containing both the raw NTP as well as the converted native (i.e. JS/UNIX) time
     */
    osc.readTimeTag = function (dv, offsetState) {
        var secs1900 = osc.readPrimitive(dv, "getUint32", 4, offsetState),
            frac = osc.readPrimitive(dv, "getUint32", 4, offsetState),
            native = (secs1900 === 0 && frac === 1) ? Date.now() : osc.ntpToJSTime(secs1900, frac);

        return {
            raw: [secs1900, frac],
            native: native
        };
    };

    /**
     * Writes an OSC time tag ("t").
     *
     * Takes, as its argument, a time tag object containing either a "raw" or "native property."
     * The raw timestamp must conform to the NTP standard representation, consisting of two unsigned int32
     * values. The first represents the number of seconds since January 1, 1900; the second, fractions of a second.
     * "Native" JavaScript timestamps are specified as a Number representing milliseconds since January 1, 1970.
     *
     * @param {Object} timeTag time tag object containing either a native JS timestamp (in ms) or a NTP timestamp pair
     * @return {Uint8Array} raw bytes for the written time tag
     */
    osc.writeTimeTag = function (timeTag) {
        var raw = timeTag.raw ? timeTag.raw : osc.jsToNTPTime(timeTag.native),
            arr = new Uint8Array(8), // Two Unit32s.
            dv = new DataView(arr.buffer);

        osc.writeInt32(raw[0], dv, 0);
        osc.writeInt32(raw[1], dv, 4);

        return arr;
    };

    /**
     * Produces a time tag containing a raw NTP timestamp
     * relative to now by the specified number of seconds.
     *
     * @param {Number} secs the number of seconds relative to now (i.e. + for the future, - for the past)
     * @param {Number} now the number of milliseconds since epoch to use as the current time. Defaults to Date.now()
     * @return {Object} the time tag
     */
    osc.timeTag = function (secs, now) {
        secs = secs || 0;
        now = now || Date.now();

        var nowSecs = now / 1000,
            nowWhole = Math.floor(nowSecs),
            nowFracs = nowSecs - nowWhole,
            secsWhole = Math.floor(secs),
            secsFracs = secs - secsWhole,
            fracs = nowFracs + secsFracs;

        if (fracs > 1) {
            var fracsWhole = Math.floor(fracs),
                fracsFracs = fracs - fracsWhole;

            secsWhole += fracsWhole;
            fracs = fracsFracs;
        }

        var ntpSecs = nowWhole + secsWhole + osc.SECS_70YRS,
            ntpFracs = Math.round(osc.TWO_32 * fracs);

        return {
            raw: [ntpSecs, ntpFracs]
        };
    };

    /**
     * Converts OSC's standard time tag representation (which is the NTP format)
     * into the JavaScript/UNIX format in milliseconds.
     *
     * @param {Number} secs1900 the number of seconds since 1900
     * @param {Number} frac the number of fractions of a second (between 0 and 2^32)
     * @return {Number} a JavaScript-compatible timestamp in milliseconds
     */
    osc.ntpToJSTime = function (secs1900, frac) {
        var secs1970 = secs1900 - osc.SECS_70YRS,
            decimals = frac / osc.TWO_32,
            msTime = (secs1970 + decimals) * 1000;

        return msTime;
    };

    osc.jsToNTPTime = function (jsTime) {
        var secs = jsTime / 1000,
            secsWhole = Math.floor(secs),
            secsFrac = secs - secsWhole,
            ntpSecs = secsWhole + osc.SECS_70YRS,
            ntpFracs = Math.round(osc.TWO_32 * secsFrac);

        return [ntpSecs, ntpFracs];
    };

    /**
     * Reads the argument portion of an OSC message.
     *
     * @param {DataView} dv a DataView instance to read from
     * @param {Object} offsetState the offsetState object that stores the current offset into dv
     * @param {Oobject} [options] read options
     * @return {Array} an array of the OSC arguments that were read
     */
    osc.readArguments = function (dv, options, offsetState) {
        var typeTagString = osc.readString(dv, offsetState);
        if (typeTagString.indexOf(",") !== 0) {
            // Despite what the OSC 1.0 spec says,
            // it just doesn't make sense to handle messages without type tags.
            // scsynth appears to read such messages as if they have a single
            // Uint8 argument. sclang throws an error if the type tag is omitted.
            throw new Error("A malformed type tag string was found while reading " +
                "the arguments of an OSC message. String was: " +
                typeTagString, " at offset: " + offsetState.idx);
        }

        var argTypes = typeTagString.substring(1).split(""),
            args = [];

        osc.readArgumentsIntoArray(args, argTypes, typeTagString, dv, options, offsetState);

        return args;
    };

    // Unsupported, non-API function.
    osc.readArgument = function (argType, typeTagString, dv, options, offsetState) {
        var typeSpec = osc.argumentTypes[argType];
        if (!typeSpec) {
            throw new Error("'" + argType + "' is not a valid OSC type tag. Type tag string was: " + typeTagString);
        }

        var argReader = typeSpec.reader,
            arg = osc[argReader](dv, offsetState);

        if (options.metadata) {
            arg = {
                type: argType,
                value: arg
            };
        }

        return arg;
    };

    // Unsupported, non-API function.
    osc.readArgumentsIntoArray = function (arr, argTypes, typeTagString, dv, options, offsetState) {
        var i = 0;

        while (i < argTypes.length) {
            var argType = argTypes[i],
                arg;

            if (argType === "[") {
                var fromArrayOpen = argTypes.slice(i + 1),
                    endArrayIdx = fromArrayOpen.indexOf("]");

                if (endArrayIdx < 0) {
                    throw new Error("Invalid argument type tag: an open array type tag ('[') was found " +
                        "without a matching close array tag ('[]'). Type tag was: " + typeTagString);
                }

                var typesInArray = fromArrayOpen.slice(0, endArrayIdx);
                arg = osc.readArgumentsIntoArray([], typesInArray, typeTagString, dv, options, offsetState);
                i += endArrayIdx + 2;
            } else {
                arg = osc.readArgument(argType, typeTagString, dv, options, offsetState);
                i++;
            }

            arr.push(arg);
        }

        return arr;
    };

    /**
     * Writes the specified arguments.
     *
     * @param {Array} args an array of arguments
     * @param {Object} options options for writing
     * @return {Uint8Array} a buffer containing the OSC-formatted argument type tag and values
     */
    osc.writeArguments = function (args, options) {
        var argCollection = osc.collectArguments(args, options);
        return osc.joinParts(argCollection);
    };

    // Unsupported, non-API function.
    osc.joinParts = function (dataCollection) {
        var buf = new Uint8Array(dataCollection.byteLength),
            parts = dataCollection.parts,
            offset = 0;

        for (var i = 0; i < parts.length; i++) {
            var part = parts[i];
            osc.copyByteArray(part, buf, offset);
            offset += part.length;
        }

        return buf;
    };

    // Unsupported, non-API function.
    osc.addDataPart = function (dataPart, dataCollection) {
        dataCollection.parts.push(dataPart);
        dataCollection.byteLength += dataPart.length;
    };

    osc.writeArrayArguments = function (args, dataCollection) {
        var typeTag = "[";

        for (var i = 0; i < args.length; i++) {
            var arg = args[i];
            typeTag += osc.writeArgument(arg, dataCollection);
        }

        typeTag += "]";

        return typeTag;
    };

    osc.writeArgument = function (arg, dataCollection) {
        if (osc.isArray(arg)) {
            return osc.writeArrayArguments(arg, dataCollection);
        }

        var type = arg.type,
            writer = osc.argumentTypes[type].writer;

        if (writer) {
            var data = osc[writer](arg.value);
            osc.addDataPart(data, dataCollection);
        }

        return arg.type;
    };

    // Unsupported, non-API function.
    osc.collectArguments = function (args, options, dataCollection) {
        if (!osc.isArray(args)) {
            args = typeof args === "undefined" ? [] : [args];
        }

        dataCollection = dataCollection || {
            byteLength: 0,
            parts: []
        };

        if (!options.metadata) {
            args = osc.annotateArguments(args);
        }

        var typeTagString = ",",
            currPartIdx = dataCollection.parts.length;

        for (var i = 0; i < args.length; i++) {
            var arg = args[i];
            typeTagString += osc.writeArgument(arg, dataCollection);
        }

        var typeData = osc.writeString(typeTagString);
        dataCollection.byteLength += typeData.byteLength;
        dataCollection.parts.splice(currPartIdx, 0, typeData);

        return dataCollection;
    };

    /**
     * Reads an OSC message.
     *
     * @param {Array-like} data an array of bytes to read from
     * @param {Object} [options] read options
     * @param {Object} [offsetState] an offsetState object that stores the current offset into dv
     * @return {Object} the OSC message, formatted as a JavaScript object containing "address" and "args" properties
     */
    osc.readMessage = function (data, options, offsetState) {
        options = options || osc.defaults;

        var dv = osc.dataView(data, data.byteOffset, data.byteLength);
        offsetState = offsetState || {
            idx: 0
        };

        var address = osc.readString(dv, offsetState);
        return osc.readMessageContents(address, dv, options, offsetState);
    };

    // Unsupported, non-API function.
    osc.readMessageContents = function (address, dv, options, offsetState) {
        if (address.indexOf("/") !== 0) {
            throw new Error("A malformed OSC address was found while reading " +
                "an OSC message. String was: " + address);
        }

        var args = osc.readArguments(dv, options, offsetState);

        return {
            address: address,
            args: args.length === 1 && options.unpackSingleArgs ? args[0] : args
        };
    };

    // Unsupported, non-API function.
    osc.collectMessageParts = function (msg, options, dataCollection) {
        dataCollection = dataCollection || {
            byteLength: 0,
            parts: []
        };

        osc.addDataPart(osc.writeString(msg.address), dataCollection);
        return osc.collectArguments(msg.args, options, dataCollection);
    };

    /**
     * Writes an OSC message.
     *
     * @param {Object} msg a message object containing "address" and "args" properties
     * @param {Object} [options] write options
     * @return {Uint8Array} an array of bytes containing the OSC message
     */
    osc.writeMessage = function (msg, options) {
        options = options || osc.defaults;

        if (!osc.isValidMessage(msg)) {
            throw new Error("An OSC message must contain a valid address. Message was: " +
                JSON.stringify(msg, null, 2));
        }

        var msgCollection = osc.collectMessageParts(msg, options);
        return osc.joinParts(msgCollection);
    };

    osc.isValidMessage = function (msg) {
        return msg.address && msg.address.indexOf("/") === 0;
    };

    /**
     * Reads an OSC bundle.
     *
     * @param {DataView} dv the DataView instance to read from
     * @param {Object} [options] read optoins
     * @param {Object} [offsetState] an offsetState object that stores the current offset into dv
     * @return {Object} the bundle or message object that was read
     */
    osc.readBundle = function (dv, options, offsetState) {
        return osc.readPacket(dv, options, offsetState);
    };

    // Unsupported, non-API function.
    osc.collectBundlePackets = function (bundle, options, dataCollection) {
        dataCollection = dataCollection || {
            byteLength: 0,
            parts: []
        };

        osc.addDataPart(osc.writeString("#bundle"), dataCollection);
        osc.addDataPart(osc.writeTimeTag(bundle.timeTag), dataCollection);

        for (var i = 0; i < bundle.packets.length; i++) {
            var packet = bundle.packets[i],
                collector = packet.address ? osc.collectMessageParts : osc.collectBundlePackets,
                packetCollection = collector(packet, options);

            dataCollection.byteLength += packetCollection.byteLength;
            osc.addDataPart(osc.writeInt32(packetCollection.byteLength), dataCollection);
            dataCollection.parts = dataCollection.parts.concat(packetCollection.parts);
        }

        return dataCollection;
    };

    /**
     * Writes an OSC bundle.
     *
     * @param {Object} a bundle object containing "timeTag" and "packets" properties
     * @param {object} [options] write options
     * @return {Uint8Array} an array of bytes containing the message
     */
    osc.writeBundle = function (bundle, options) {
        if (!osc.isValidBundle(bundle)) {
            throw new Error("An OSC bundle must contain 'timeTag' and 'packets' properties. " +
                "Bundle was: " + JSON.stringify(bundle, null, 2));
        }

        options = options || osc.defaults;
        var bundleCollection = osc.collectBundlePackets(bundle, options);

        return osc.joinParts(bundleCollection);
    };

    osc.isValidBundle = function (bundle) {
        return bundle.timeTag !== undefined && bundle.packets !== undefined;
    };

    // Unsupported, non-API function.
    osc.readBundleContents = function (dv, options, offsetState, len) {
        var timeTag = osc.readTimeTag(dv, offsetState),
            packets = [];

        while (offsetState.idx < len) {
            var packetSize = osc.readInt32(dv, offsetState),
                packetLen = offsetState.idx + packetSize,
                packet = osc.readPacket(dv, options, offsetState, packetLen);

            packets.push(packet);
        }

        return {
            timeTag: timeTag,
            packets: packets
        };
    };

    /**
     * Reads an OSC packet, which may consist of either a bundle or a message.
     *
     * @param {Array-like} data an array of bytes to read from
     * @param {Object} [options] read options
     * @return {Object} a bundle or message object
     */
    osc.readPacket = function (data, options, offsetState, len) {
        var dv = osc.dataView(data, data.byteOffset, data.byteLength);

        len = len === undefined ? dv.byteLength : len;
        offsetState = offsetState || {
            idx: 0
        };

        var header = osc.readString(dv, offsetState),
            firstChar = header[0];

        if (firstChar === "#") {
            return osc.readBundleContents(dv, options, offsetState, len);
        } else if (firstChar === "/") {
            return osc.readMessageContents(header, dv, options, offsetState);
        }

        throw new Error("The header of an OSC packet didn't contain an OSC address or a #bundle string." +
            " Header was: " + header);
    };

    /**
     * Writes an OSC packet, which may consist of either of a bundle or a message.
     *
     * @param {Object} a bundle or message object
     * @param {Object} [options] write options
     * @return {Uint8Array} an array of bytes containing the message
     */
    osc.writePacket = function (packet, options) {
        if (osc.isValidMessage(packet)) {
            return osc.writeMessage(packet, options);
        } else if (osc.isValidBundle(packet)) {
            return osc.writeBundle(packet, options);
        } else {
            throw new Error("The specified packet was not recognized as a valid OSC message or bundle." +
                " Packet was: " + JSON.stringify(packet, null, 2));
        }
    };

    // Unsupported, non-API.
    osc.argumentTypes = {
        i: {
            reader: "readInt32",
            writer: "writeInt32"
        },
        h: {
            reader: "readInt64",
            writer: "writeInt64"
        },
        f: {
            reader: "readFloat32",
            writer: "writeFloat32"
        },
        s: {
            reader: "readString",
            writer: "writeString"
        },
        S: {
            reader: "readString",
            writer: "writeString"
        },
        b: {
            reader: "readBlob",
            writer: "writeBlob"
        },
        t: {
            reader: "readTimeTag",
            writer: "writeTimeTag"
        },
        T: {
            reader: "readTrue"
        },
        F: {
            reader: "readFalse"
        },
        N: {
            reader: "readNull"
        },
        I: {
            reader: "readImpulse"
        },
        d: {
            reader: "readFloat64",
            writer: "writeFloat64"
        },
        c: {
            reader: "readChar32",
            writer: "writeChar32"
        },
        r: {
            reader: "readColor",
            writer: "writeColor"
        },
        m: {
            reader: "readMIDIBytes",
            writer: "writeMIDIBytes"
        },
        // [] are special cased within read/writeArguments()
    };

    // Unsupported, non-API function.
    osc.inferTypeForArgument = function (arg) {
        var type = typeof arg;

        // TODO: This is freaking hideous.
        switch (type) {
            case "boolean":
                return arg ? "T" : "F";
            case "string":
                return "s";
            case "number":
                return "f";
            case "undefined":
                return "N";
            case "object":
                if (arg === null) {
                    return "N";
                } else if (arg instanceof Uint8Array ||
                    arg instanceof ArrayBuffer) {
                    return "b";
                } else if (typeof arg.high === "number" && typeof arg.low === "number") {
                    return "h";
                }
                break;
        }

        throw new Error("Can't infer OSC argument type for value: " +
            JSON.stringify(arg, null, 2));
    };

    // Unsupported, non-API function.
    osc.annotateArguments = function (args) {
        var annotated = [];

        for (var i = 0; i < args.length; i++) {
            var arg = args[i],
                msgArg;

            if (typeof (arg) === "object" && arg.type && arg.value !== undefined) {
                // We've got an explicitly typed argument.
                msgArg = arg;
            } else if (osc.isArray(arg)) {
                // We've got an array of arguments,
                // so they each need to be inferred and expanded.
                msgArg = osc.annotateArguments(arg);
            } else {
                var oscType = osc.inferTypeForArgument(arg);
                msgArg = {
                    type: oscType,
                    value: arg
                };
            }

            annotated.push(msgArg);
        }

        return annotated;
    };

    if (osc.isCommonJS) {
        module.exports = osc;
    }
}());
;/*
 * osc.js: An Open Sound Control library for JavaScript that works in both the browser and Node.js
 *
 * Cross-platform base transport library for osc.js.
 *
 * Copyright 2014-2016, Colin Clark
 * Licensed under the MIT and GPL 3 licenses.
 */

/* global require, module */

var osc = osc || require("./osc.js"),
    slip = slip || require("slip"),
    EventEmitter = EventEmitter || require("events").EventEmitter;

(function () {

    "use strict";

    // Unsupported, non-API function.
    osc.firePacketEvents = function (port, packet, timeTag, packetInfo) {
        if (packet.address) {
            port.emit("message", packet, timeTag, packetInfo);
        } else {
            osc.fireBundleEvents(port, packet, timeTag, packetInfo);
        }
    };

    // Unsupported, non-API function.
    osc.fireBundleEvents = function (port, bundle, timeTag, packetInfo) {
        port.emit("bundle", bundle, timeTag, packetInfo);
        for (var i = 0; i < bundle.packets.length; i++) {
            var packet = bundle.packets[i];
            osc.firePacketEvents(port, packet, bundle.timeTag, packetInfo);
        }
    };

    osc.fireClosedPortSendError = function (port, msg) {
        msg = msg || "Can't send packets on a closed osc.Port object. Please open (or reopen) this Port by calling open().";

        port.emit("error", msg);
    };

    osc.Port = function (options) {
        this.options = options || {};
        this.on("data", this.decodeOSC.bind(this));
    };

    var p = osc.Port.prototype = Object.create(EventEmitter.prototype);
    p.constructor = osc.Port;

    p.send = function (oscPacket) {
        var args = Array.prototype.slice.call(arguments),
            encoded = this.encodeOSC(oscPacket),
            buf = osc.nativeBuffer(encoded);

        args[0] = buf;
        this.sendRaw.apply(this, args);
    };

    p.encodeOSC = function (packet) {
        // TODO gh-39: This is unsafe; we should only access the underlying
        // buffer within the range of its view.
        packet = packet.buffer ? packet.buffer : packet;
        var encoded;

        try {
            encoded = osc.writePacket(packet, this.options);
        } catch (err) {
            this.emit("error", err);
        }

        return encoded;
    };

    p.decodeOSC = function (data, packetInfo) {
        data = osc.byteArray(data);
        this.emit("raw", data, packetInfo);

        try {
            var packet = osc.readPacket(data, this.options);
            this.emit("osc", packet, packetInfo);
            osc.firePacketEvents(this, packet, undefined, packetInfo);
        } catch (err) {
            this.emit("error", err);
        }
    };


    osc.SLIPPort = function (options) {
        var that = this;
        var o = this.options = options || {};
        o.useSLIP = o.useSLIP === undefined ? true : o.useSLIP;

        this.decoder = new slip.Decoder({
            onMessage: this.decodeOSC.bind(this),
            onError: function (err) {
                that.emit("error", err);
            }
        });

        var decodeHandler = o.useSLIP ? this.decodeSLIPData : this.decodeOSC;
        this.on("data", decodeHandler.bind(this));
    };

    p = osc.SLIPPort.prototype = Object.create(osc.Port.prototype);
    p.constructor = osc.SLIPPort;

    p.encodeOSC = function (packet) {
        // TODO gh-39: This is unsafe; we should only access the underlying
        // buffer within the range of its view.
        packet = packet.buffer ? packet.buffer : packet;
        var framed;

        try {
            var encoded = osc.writePacket(packet, this.options);
            framed = slip.encode(encoded);
        } catch (err) {
            this.emit("error", err);
        }

        return framed;
    };

    p.decodeSLIPData = function (data, packetInfo) {
        // TODO: Get packetInfo through SLIP decoder.
        this.decoder.decode(data, packetInfo);
    };


    // Unsupported, non-API function.
    osc.relay = function (from, to, eventName, sendFnName, transformFn, sendArgs) {
        eventName = eventName || "message";
        sendFnName = sendFnName || "send";
        transformFn = transformFn || function () {};
        sendArgs = sendArgs ? [null].concat(sendArgs) : [];

        var listener = function (data) {
            sendArgs[0] = data;
            data = transformFn(data);
            to[sendFnName].apply(to, sendArgs);
        };

        from.on(eventName, listener);

        return {
            eventName: eventName,
            listener: listener
        };
    };

    // Unsupported, non-API function.
    osc.relayPorts = function (from, to, o) {
        var eventName = o.raw ? "raw" : "osc",
            sendFnName = o.raw ? "sendRaw" : "send";

        return osc.relay(from, to, eventName, sendFnName, o.transform);
    };

    // Unsupported, non-API function.
    osc.stopRelaying = function (from, relaySpec) {
        from.removeListener(relaySpec.eventName, relaySpec.listener);
    };


    /**
     * A Relay connects two sources of OSC data together,
     * relaying all OSC messages received by each port to the other.
     * @constructor
     *
     * @param {osc.Port} port1 the first port to relay
     * @param {osc.Port} port2 the second port to relay
     * @param {Object} options the configuration options for this relay
     */
    osc.Relay = function (port1, port2, options) {
        var o = this.options = options || {};
        o.raw = false;

        this.port1 = port1;
        this.port2 = port2;

        this.listen();
    };

    p = osc.Relay.prototype = Object.create(EventEmitter.prototype);
    p.constructor = osc.Relay;

    p.open = function () {
        this.port1.open();
        this.port2.open();
    };

    p.listen = function () {
        if (this.port1Spec && this.port2Spec) {
            this.close();
        }

        this.port1Spec = osc.relayPorts(this.port1, this.port2, this.options);
        this.port2Spec = osc.relayPorts(this.port2, this.port1, this.options);

        // Bind port close listeners to ensure that the relay
        // will stop forwarding messages if one of its ports close.
        // Users are still responsible for closing the underlying ports
        // if necessary.
        var closeListener = this.close.bind(this);
        this.port1.on("close", closeListener);
        this.port2.on("close", closeListener);
    };

    p.close = function () {
        osc.stopRelaying(this.port1, this.port1Spec);
        osc.stopRelaying(this.port2, this.port2Spec);
        this.emit("close", this.port1, this.port2);
    };


    // If we're in a require-compatible environment, export ourselves.
    if (typeof module !== "undefined" && module.exports) {
        module.exports = osc;
    }
}());
;/*
 * osc.js: An Open Sound Control library for JavaScript that works in both the browser and Node.js
 *
 * Cross-Platform Web Socket client transport for osc.js.
 *
 * Copyright 2014-2016, Colin Clark
 * Licensed under the MIT and GPL 3 licenses.
 */

/*global WebSocket, require*/

var osc = osc || require("../osc.js");

(function () {

    "use strict";

    osc.WebSocket = typeof WebSocket !== "undefined" ? WebSocket : require ("ws");

    osc.WebSocketPort = function (options) {
        osc.Port.call(this, options);
        this.on("open", this.listen.bind(this));

        this.socket = options.socket;
        if (this.socket) {
            if (this.socket.readyState === 1) {
                osc.WebSocketPort.setupSocketForBinary(this.socket);
                this.emit("open", this.socket);
            } else {
                this.open();
            }
        }
    };

    var p = osc.WebSocketPort.prototype = Object.create(osc.Port.prototype);
    p.constructor = osc.WebSocketPort;

    p.open = function () {
        if (!this.socket || this.socket.readyState > 1) {
            this.socket = new osc.WebSocket(this.options.url);
        }

        osc.WebSocketPort.setupSocketForBinary(this.socket);

        var that = this;
        this.socket.onopen = function () {
            that.emit("open", that.socket);
        };
    };

    p.listen = function () {
        var that = this;
        this.socket.onmessage = function (e) {
            that.emit("data", e.data, e);
        };

        this.socket.onerror = function (err) {
            that.emit("error", err);
        };

        this.socket.onclose = function (e) {
            that.emit("close", e);
        };

        that.emit("ready");
    };

    p.sendRaw = function (encoded) {
        if (!this.socket || this.socket.readyState !== 1) {
            osc.fireClosedPortSendError(this);
            return;
        }

        this.socket.send(encoded);
    };

    p.close = function (code, reason) {
        this.socket.close(code, reason);
    };

    osc.WebSocketPort.setupSocketForBinary = function (socket) {
        socket.binaryType = osc.isNode ? "nodebuffer" : "arraybuffer";
    };

}());
;
    return osc;
}));
