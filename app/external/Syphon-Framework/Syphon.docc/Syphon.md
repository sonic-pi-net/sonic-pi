# ``Syphon``

Share video and still images with other applications in realtime, instantly.

## Overview

The Syphon framework provides the classes necessary to add Syphon support to your application. A Syphon server is used to make frames available to other applications. ``SyphonServerDirectory`` is used to discover available servers. A Syphon client is used to connect to and receive frames from a Syphon server. Servers and clients are available for OpenGL and Metal, and the two are interoperable.

If you'd like to examine the framework's source code, report a bug, or get involved in development, head on over to the [Syphon framework GitHub project](https://github.com/Syphon/Syphon-Framework).

## Topics

### Getting Started

- <doc:GettingStarted>
- <doc:ExtendingSyphon>

### Servers

- ``SyphonMetalServer``
- ``SyphonOpenGLServer``
- ``SyphonServerBase``

### Finding Servers

- ``SyphonServerDirectory``

### Clients

- ``SyphonMetalClient``
- ``SyphonOpenGLClient``
- ``SyphonClientBase``
- ``SyphonOpenGLImage``
- ``SyphonImageBase``

### Deprecated Classes

- ``SyphonServer``
- ``SyphonClient``
- ``SyphonImage``
