# adagraph-xcb

This code implements an Ada package body (mostly) compliant with the
[Adagraph](http://www.ada95.com/jvandyk/adagraph.html) graphics library using 
X11 and XCB instead of relying on the Win32 drawing system allowing 'drop-in'
use on Linux and Mac systems.

As of writing, only a subset of the package specification has been implemented,
and you may need to replace or remove the unavailable procedures from the
specification before you can successfully compile the package in your program.
