/*
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
