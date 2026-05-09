# Extending Syphon

## Overview

Syphon can be extended for any IOSurface-based technology using the Syphon Base classes without modifying the framework at all.

Import `<Syphon/SyphonSubclassing.h>` in your implementation files to gain access to essential methods for subclasses to use.

### Images

If your new format is not an Objective C class, implement a new image class by subclassing ``SyphonImageBase`` to expose the surface in your new format. This class should be a minimal wrapper to the contained type.

### Servers

Implement a server by subclassing ``SyphonServerBase``. Add methods to your subclass to publish frames. When needed, call ``SyphonServerBase/newSurfaceForWidth:height:options:`` to obtain an `IOSurfaceRef`. When you have updated the surface, call ``SyphonServerBase/publish``. Add a method named `-newFrameImage` which returns an instance of your ``SyphonImageBase`` subclass (or the new type directly).

### Clients

Implement a client by subclassing ``SyphonClientBase``. Add a method named `-newFrameImage` which returns an instance of your ``SyphonImageBase`` subclass. Implement the ``SyphonClientBase/invalidateFrame`` method, noting this may be called on a background thread.

### Other Considerations

If you override other Syphon Base class methods, be sure to pass them on to the superclass (eg if you override ``SyphonServerBase/stop``, call `[super stop]` from your implementation of ``SyphonServerBase/stop``).

If your changes are for a well-used API, please consider making a pull-request or otherwise reaching out to us so we can add them to the project.
