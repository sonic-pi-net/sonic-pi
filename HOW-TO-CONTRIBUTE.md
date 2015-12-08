# Contributing

Hey, you like Sonic Pi and want to contribute in some way?
That's great, this is an open source project and you're invited to join!

## You have an idea?

If you have any idea on how to improve Sonic Pi, don't hesitate to
[open a new issue](https://github.com/samaaron/sonic-pi/issues) on
GitHub and describe what you have in mind.

You can also visit the
[developers' chat](https://gitter.im/samaaron/sonic-pi) if you want
to discuss your idea directly.

## You need an idea?

If you don't have an itch of your own to scratch, but are still looking
for something to work on, you can first go and browse the
[open issues](https://github.com/samaaron/sonic-pi/issues) on GitHub.
There's probably one among them you can help to fix.

Next to those, here's a list of ideas, frequent feature requests or
unfinished projects that we didn't have time to work on just yet and
where help is appreciated.

### Easy Projects

- Share your teaching material
  
  You're an educator and you have made your own teaching material?
  Then share it with the world! A good place to do so is the
  [user group](https://groups.google.com/forum/#!forum/sonic-pi).

- Correct us
  
  Proofreading is always helpful. If you find a typo or bad writing,
  let us know and [open a new issue](https://github.com/samaaron/sonic-pi/issues)
  or, even better, send a pull request on GitHub.
  
- [Translate Sonic Pi to your language](https://github.com/samaaron/sonic-pi/blob/master/TRANSLATION.md)
  
  It's a wonderful way to introduce school kids in your country to
  Sonic Pi and educators will appreciate it when we make it easier for
  their class. The tutorial is fairly long, but the graphical user
  interface is quickly translated and a good place to start.

### Medium Projects

- Save/Load function for buffers
  
  Sonic Pi can save to a file, but it still doesn't have a user-friendly
  way to load existing code.

- Packaging for Linux

  Currently, there are binaries available for OS X, Windows and
  Raspberry Pi Raspbian. For other Linux distributions, users have to
  build it on their own. We'd love to find Linux enthusiasts who'd
  volunteer to maintain packaging for their Linux distros.

- Translation Server Integration

  Sonic Pi translation has been a good start, but it's now turning into
  a hassle to keep up with the changes. A translation service such as
  Transifex etc. would be beneficial. The difficult part is how to
  integrate this with GitHub.

- Optimisation: Identify & fix bottlenecks that waste CPU or RAM

  Several different parts work together in Sonic Pi, there's
  Supercollider, controlled by a server written in Ruby and
  a QT-based GUI on top of it. All this runs on a Raspberry Pi, so even
  a small optimisation under the hood may be very helpful in
  keeping things smooth. If you love profiling and optimising existing
  code we'd love to hear from you.

### Hard Projects

- Sync multiple instances of Sonic Pi on the net

  How to play Sonic Pi as an orchestra? Should there be a central
  audio server that turns all the clients' code to music? Or is it
  possible to synchronise each machine's audio on the net?

- Mobile devices? Porting to Android?

  These days, school kids have a smartphone or tablet before they have
  their own computer.

  It's not possible to build Sonic Pi for iOS, since Apple does not
  allow integrating a programming language into iOS apps.

  Technically it should be possible for Android, however nobody has
  tried that yet. Are you an Android fanboy and willing to maintain a
  port?

  Or maybe we should explore a client/server architecture instead, so
  that we gain a path for an iOS app?
