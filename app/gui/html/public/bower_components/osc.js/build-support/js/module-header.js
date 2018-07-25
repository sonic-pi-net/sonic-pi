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
