# Getting Started


## Overview

Syphon is simple to integrate, allowing you to start using it with very few lines of code.

### Servers

Class documentation: ``SyphonMetalServer``, ``SyphonOpenGLServer``

Create a server:

```objc
SyphonMetalServer *server = [[SyphonMetalServer alloc] initWithName:@"My Output"
                                                             device:device
                                                            options:nil];
```

and then publish new frames:

```objc
[server publishFrameTexture:myTex
            onCommandBuffer:commandBuffer
                imageRegion:NSMakeRect(0, 0, width, height)
                    flipped:NO];
```

The OpenGL server has a similar method, plus methods to bind and unbind the server to the OpenGL context, so you can draw into it directly.
You can publish new frames as often as you like, but if you only publish when you have a frame different from the previous one, then clients can do less work.
You must stop the server when you are finished with it:

```objc
[server stop];
```

### Finding Servers

Class documentation: ``SyphonServerDirectory``

``SyphonServerDirectory`` handles server discovery for you. You can get an array of dictionaries describing available servers:

```objc
NSArray *available = [[SyphonServerDirectory sharedDirectory] servers];
```

The servers property can be observed for changes, or you can register to receive the notifications posted by ``SyphonServerDirectory``.

Server description dictionaries are used by Syphon when you create a client, and also contain information you can use to describe available servers in your UI:

```objc
[menuItem setTitle:[description objectForKey:SyphonServerDescriptionNameKey]];
```

### Clients

Class documentation: ``SyphonMetalClient``, ``SyphonOpenGLClient``, ``SyphonOpenGLImage``

Usually you create a client with a server description dictionary you obtained from ``SyphonServerDirectory``:


```objc
SyphonMetalClient *client = [[SyphonMetalClient alloc] initWithServerDescription:description
                                                                          device:device
                                                                         options:nil
                                                                 newFrameHandler:^(SyphonMetalClient *client) {
    // you could get the new frame here and update your view
}];
```

The new-frame handler is optional: you can pass in nil.

When you are ready to draw:


```objc
id<MTLTexture> *frame = [client newFrameImage];
if (frame)
{
    // YOUR METAL DRAWING CODE HERE

    [frame release]; // (if not using ARC)
}
```

As with servers, you must stop the client when you are finished with it:

```objc
[client stop];
```

### More examples

Example projects implementing a server and client are included with the Syphon SDK. You can also examine the source to some Syphon implementations on [GitHub](https://github.com/Syphon).

Good luck!
