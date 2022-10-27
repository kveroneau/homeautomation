## Generation 3 of Kevin's Home Automation System

Welcome!  If you were oddly following this project, or have oddly even been using it for some reason,
you are in for quite the surprise when it comes to the wonderful third generation of my home automation
system.  It is no longer being developed in the Python language.  As you may have noticed the Python
source code is for Python 2.7.10, and hasn't been made compatible with Python 3.x, making it more and
more difficult to run on a modern Linux distribution without putting it into some kind of container.

This is where the move to ObjectPascal comes in!  With ObjectPascal, the only place and time to worry
about dependencies, external libraries, and external modules is during the compile time.  Once a nice
ELF binary has been generated for your target platform, it's as simple to run as any other ELF binary
in Linux.  You can place it into /usr somewhere, like /usr/local/bin, or run it from where ever you
please.

### Advantages of using ObjectPascal over Python

Before diving into why ObjectPascal over **Insert popular compiler here**, let's just dive into the
basics on how this new code base already has advantages over the previous Python code.

Once it's compiled as an ELF binary for the target platform, it should run without any issues, or the
need to install anything else.  Just copy over the ELf binary, and run, simple as that!

The overall size on disk and memory of the new ObjectPascal version is incredible, and very useful in
embedded environments, such as running on a low spec Raspberry Pi.  Python can take a ton of disk
space and memory, as that is just the very nature of interpreted languages.  Check out these sizes:

```
499K -- LightController
585K -- HTTPEndpoints
```

The runtime memory usage will normally be about 2MB or slightly more.  Compare this to the whooping
size of the entire Python binary, it's standard library, and any additional libraries needed.

```
3.6M -- /usr/bin/python2.7
163M -- /usr/lib/python2.7/*
```

I'm sure Python 3.x will be even larger, but for running on a low spec embedded device, I can do
better than Python.  Although Python is technically pre-installed on every Linux install these
days, there is also the point of runtime memory requirements.  The original Pi only had 512MB of
RAM, I think most of my personal Pis only have 1GB of RAM.  So, as I keep extending and building
out my home automation in Python, it's footprint will only increase in size until it can no longer
be easily run on a Pi without some additional work.

ObjectPascal does not have a GIL!  This allows POSIX threads to work as expected and overall just
be more consistent.  Sure, Generation 2 did move to green threads using gevent, which does work
rather well.  Upgrading the existing Python 2 code to work with the new Python 3 async will be
more of a pain than it is worth, and not for much gain either.

Generation 3 will also be really modular, as now we are running native ELF binaries, we no longer
need to worry as much about memory usage, so each component can now happily live in it's own
binary program.

### Why ObjectPascal over another, more popular compiler?

Firstly, I know Pascal as a language and feel more comfortable and confident using it over other
compiled languages currently.

I feel that ObjectPascal has many more managed code-like safe guards in-place making it much
easier to develop a native application without breaking my system.  The Lazarus IDE is a
very powerfull all-in-one monolithic IDE that does a lot of the work.  For example, instead of
using an external program like Valgrind to check for memory leaks, Lazarus and actually
FreePascal for that matter has it all built-in and ready to go.  Each time I run my program
in Lazarus, after it finishes, it will provide details on my application's memory use, and if
any blocks were never freed, even giving me specific line numbers of where the memory block
was first allocated in code, so I can find the best place to free that memory I forgot about.

ObjectPascal is both a procedural language and class-based language, it works the same regardless
which mode I program in.  Whereas with C, there is a separate language called C++ or Objective-C
for using classes in C.  I'm sure there are some OOP libraries for standard C, but we aren't
looking at third-party libraries here.  With FreePascal, I can either code procedural, or using
objects/classes, and even mix both of them as needed, and I am not forced to code only procedural,
or only with classes.  I am not forced to move to an entirely different language to swap between
those programming methods.  FreePascal just supports both.

Perhaps one day I'll take a gander at Go and Rust, now that the latter has official Linux kernel
support, it might be interesting to see why Rust has been gaining popularity there.  Go is
also incredibly popular, Docker being a prime example of a successful Go application.  I wonder
if Docker would have been so popular if it were built in say Ruby or Python?  There are a few
successful Ruby system programs out there, such as `Vagrant`, but instead of controlling Linux
cgroups and automating that subsystem, `Vagrant` instead automates the building of full virtual
machines using VirtualBox, libvirt, and Hyper-V.  I still believe that Docker would have became
popular regardless what language it was developed in though, as long as it works, does what it
is intended to do, then how it was made doesn't impact it's overall quality.  However, I do
believe in that during development, and importantly for the developers themselves, that read-able
code is very important.  Some programming languages are easier to read than others.  Ruby is an
example of a language I personally struggle to read efficiently, as it has some unique symbols
most other languages do not have.  Python for a time, I thought was one of the world's most
human read-able programming languages, and well, it still is for the most part.  The language
since moving into the 3.x era began to change quite a bit, and it has begun to become a bit
more difficult for me to easily read at a glance and know what's going on.  Which brings us to
ObjectPascal!  Personally, I think it is very read-able, and very navigatable using the Lazarus
IDE.  And because it is a compiled language, I am more free to spread my code around for efficient
use, and to add comments which will never make it into the final binary.  I really like how Pascal
as a language enforces a very specific format, unlike most languages where you just start writing
your code, Pascal requires a very specific template to even work, some might call it boilerplate,
while it is, the IDE and most editors will spit out a default template for a program or unit for
you making this a non-issue.  There is an advantage to having code in a specific format, it makes
it so much easier to navigate and know what's public outside this module, what's global, and even
all the local variables for individual procedures and functions.  At a glance, you can see all
the local variables a specific method uses without needing to look at the entire method, saving a
lot of time.

A big selling point, that I believe **Go** has is easier module support.  So, in C and C++, you
usually need two components to use a module outside your main module, a header file, and the code
usually in the form of source code to be compiled with your main program, or an external library
you will need to pass to the linker during the compile process.  With Pascal, all the pain of
adding and using modules is gone.  Just like in say Python, you *import* the unit by just specifying
it's name on a special line.
