==================================================
Installing Alt-Ergo (Mac OS X)
==================================================

brew install Caskroom/cask/xquartz
brew install gtksourceview
brew install libxml2
opam update
LDFLAGS=-L/usr/local/lib CFLAGS=-I/usr/local/include opam install zarith --verbose
opam install alt-ergo altgr-ergo satML-plugin

Resources: 
- http://alt-ergo.ocamlpro.com/download.php
- https://github.com/mirage/mirage/issues/413