# Buclet

## CFFI bindings to Bullet


### Introduction

Buclet provides CFFI bindings to the
<a href="http://en.wikipedia.org/wiki/Bullet_(software)">Bullet</a>
library.

Bullet is written in C++ but exposes a small part of itself through a
C API. Mostly for rigid body dynamics. In the future I (or maybe you!)
will either help the Bullet project to extend this C API or, since the
previous seems all the be hand-work, I will autogenerate CFFI bindings
in a similar way as is currently being developed for
[Okra](http://common-lisp.net/project/okra/).


### License

This project is released under the simplified
[BSD](http://www.opensource.net/licenses/bsd-license.php) license.


### To Do

* (a lot)
* decide whether to work on Bullet's C API or autogenerate wrappers like in Okra


### Platforms

The code has currently been tested on Linux using SBCL and on Windows
XP using Clozure CL and MinGW.


### Version numbering

Buclet follows the Bullet version numbering with its own version
number tagged on at the end. This way you can easily see which version
of Bullet these bindings are written for and what the latest release
of Buclet itself is.
