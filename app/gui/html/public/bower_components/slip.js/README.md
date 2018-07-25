slip.js
=======

slip.js is a JavaScript library for encoding and decoding [Serial Line Internet Protocol](http://tools.ietf.org/html/rfc1055) packets. It works in both Node.js and in a web browser.

How Do I Use It?
----------------

slip.js provides two pieces of functionality: encoding and decoding messages.

### Encoding

Encoding is stateless and synchronous. `slip.encode()` takes any array-like object containing bytes, such as a Uint8Array, Node.js Buffer, ArrayBuffer, or plain JavaScript Array. It returns a Uint8Array containing the encoded message.

#### Example

```javascript
var message = new Uint8Array([99, 97, 116, 33]);
var slipEncoded = slip.encode(message); // Result is [192, 99, 97, 33, 192]
```

#### Options

<table>
    <tr>
        <th>Option</th>
        <th>Type</th>
        <th>Description</th>
        <th>Default value</th>
    </tr>
    <tr>
        <td>bufferPadding</td>
        <td><code>Number</code></td>
        <td>_Optional_. The number of bytes to add to the message's length when initializing the encoder's internal buffer.</td>
        <td>4</td>
    </tr>
    <tr>
        <td>offset</td>
        <td><code>Number</code></td>
        <td>_Optional_. An offset index into the <code>data</code> argument to start reading the message from.</td>
        <td><code>undefined</code></td>
    </tr>
    <tr>
        <td>byteLength</td>
        <td><code>Number</code></td>
        <td>_Optional_. The number of bytes of the <code>data</code> argument to read.</td>
        <td><code>undefined</code></td>
    </tr>
</table>

### Decoding

Decoding is stateful and asynchronous. You need to instantiate a `slip.Decoder` object, providing a callback that will be invoked whenever a complete message is received. By default, messages are limited to 10 MB in size. You can increase this value by providing a `maxBufferSize` option to the `Decoder` constructor, specified in bytes.

To decode a SLIP packet, call `decode()`. Whenever the `slip.Decoder` detects the end of an incoming message, it will call its `onMessage` callback.

#### Example

```javascript
var logMessage = function (msg) {
    console.log("A SLIP message was received! Here is it: " + msg);
};

var decoder = new slip.Decoder({
    onMessage: logMessage,
    maxMessageSize: 209715200,
    bufferSize: 2048
});

decoder.decode(packet);
decoder.decode(otherPacket);
```

#### Options

<table>
    <tr>
        <th>Option</th>
        <th>Type</th>
        <th>Description</th>
        <th>Default value</th>
    </tr>
    <tr>
        <td>bufferSize</td>
        <td><code>Number</code></td>
        <td>_Optional_. The initial size of the decoder's internal buffer. It will be resized as necessary.</td>
        <td><code>1024</code></td>
    </tr>
    <tr>
        <td>maxMessageSize</td>
        <td><code>Number</code></td>
        <td>_Optional_. The maximum size of incoming messages, in bytes. Messages larger than this value will cause the <code>onError</code> callback to be invoked.</td>
        <td>10485760 (10 MB)</td>
    </tr>
    <tr>
        <td>onMessage</td>
        <td><code>Function</code></td>
        <td>A callback that will be invoked whenever a complete message is decoded.</td>
        <td><code>undefined</code></td>
    </tr>
    <tr>
        <td>onError</td>
        <td><code>Function</code></td>
        <td>A callback that will be invoked whenever an error occurs.</td>
        <td><code>undefined</code></td>
    </tr>
</table>

#### Events

The `onMessage` callback's signature is:

<table>
    <tr>
        <th>Argument</th>
        <th>Type</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>msg</td>
        <td><code>Uint8Array</code></td>
        <td>The decoded message, with SLIP characters removed.</td>
    </tr>
</table>

The `onError` callback's signature is:

<table>
    <tr>
        <th>Argument</th>
        <th>Type</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>msgBuffer</td>
        <td><code>Uint8Array</code></td>
        <td>A copy of the internal message buffer.</td>
    </tr>
    <tr>
        <td>errorMsg</td>
        <td><code>String</code></td>
        <td>The error message.</td>
    </tr>
</table>

License
-------

slip.js is written by Colin Clark and distributed under the MIT and GPL 3 licenses.
