#! /bin/sh

PATH=$PATH:../lib;
wx86cl.exe --load hello-world.lisp --eval "(in-package :buclet)";
