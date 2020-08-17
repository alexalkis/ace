# ACE Basic Compiler for the Amiga

This is a cross-compiler that runs on Linux.

```
git clone https://github.com/alexalkis/ace
cd ace
mkdir build
cd build
cmake ..
make
export ACE_Basic="/where/ever/you/cloned/it/ace"
```
That last export you might consider appending it to your ~/.bashrc if you find yourself using this cross-compiler.

Copy ace and acecc (in acecc dir) somewhere in your path.  

You will need vasm & vlink.

If you have a foo.b try

```
acecc foo.b
```

and you should be left with an amiga executable called foo

Have fun.
