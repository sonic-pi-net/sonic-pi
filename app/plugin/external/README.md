# Patched crates

## objc2

objc2 0.6.5 from crates.io, with one change, in `src/__macro_helpers/define_class.rs` (marked "Sonic Pi's change").

objc2 makes the Objective-C classes that Rust code defines (`define_class!`), each under a name built from its
module and its crate's version. Those names are the same in every build that uses the same version of the crate.
When a name is already registered, objc2 takes the class that is there. A host with two copies of the plugin in
it (the VST3 and the Audio Unit, each its own dynamic library) then has the second copy using the first copy's
classes: the first copy's code, run on the second copy's data. Two different builds lay that data out
differently, and the host crashes (REAPER, with a VST3 and an Audio Unit of two builds).

With the change, a class whose name objc2 generated is registered under that name followed by the address of a
static of the copy that makes it, so every copy has classes of its own. The plain name is never registered, so a
copy built without the change (plugin 0.2.1 and earlier) that loads later finds nothing to take either.

To update objc2: replace this directory with the new release and make the change again. `test/host.mm` checks it
("each copy of the binary has its own web view classes").
