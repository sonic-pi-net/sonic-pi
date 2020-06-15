# Installing Sonic Pi from Source

If you want to use the very latest development version of Sonic Pi, then
you'll need to compile from source. Due to the complex nature of Sonic
Pi's architecture this should only be considered by those happy playing
around with the Terminal and are happy working with bleeding edge
software. 

For all other users, downloading the latest pre-built app for your
platform is highly recommended: http://sonic-pi.net
 

There are instructions for the following platforms in their own separate
files:

* Raspberry Pi - [INSTALL-RASPBERRY-PI.md](INSTALL-RASPBERRY-PI.md)
* Windows - [INSTALL-WINDOWS.md](INSTALL-WINDOWS.md)
* Mac - [INSTALL-MAC.md](INSTALL-MAC.md)
* Linux - [INSTALL-LINUX.md](INSTALL-LINUX.md)

----

## Optional: Sonic Pi reference books

Do you want to read the Sonic Pi tutorial as a whole, e.g. on your
mobile reader or printed out on paper?

During the Qt GUI build process, the directory `app/gui/qt/book` will
be generated, containing each section of the integrated help system
as a printable HTML reference book document.

As an optional step after the build process, you can convert these HTML
files to more convenient PDFs using the `./create-pdf` script.

On your Linux or OS X system, you will need to have installed

* [wkhtmltopdf](http://wkhtmltopdf.org)
  
  (Note: On Ubuntu, you will need the
  [wkhtmltopdf binary with a patched Qt](http://wkhtmltopdf.org/downloads.html)
  from their site, as Ubuntu's own binary package does not support all
  features needed for a clean PDF conversion.)

----

## Unsupported development HTML Interface

Note: This interface isn't always kept up to date with the main branch on Github.

The dependencies for this are:

* SuperCollider
* Ruby 1.9.3+

If you wish to play with the (development) HTML interface on OS X:

* Install SuperCollider manually (the Mac OS X app): http://supercollider.sourceforge.net
* Download a tar ball of the latest version of Sonic Pi: https://github.com/samaaron/sonic-pi/
* Unzip the tar ball somewhere useful
* Install JDK 1.6+ and Leiningen (to compile ClojureScript -> Javascript, not for running the app)
* Compile the `cljs` source: `cd app/gui/html`, `lein cljsbuild once`
* Start the server: `cd app/server/ruby/bin`, `ruby ws.rb`
* Open a browser and go to `http://localhost:8000`
