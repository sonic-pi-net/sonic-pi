# Release Notes

#### 0.92.0
* Renamed a require in rspec so they would work again.
* Dropped filemagic since this already has excellent file parsing and is platform agnostic.
* Dropped the rest of app.rb, since it wasn't adding anything but line count at this point.
* Added a RELEASE.md to track changes as I go.
* Dropped the unneeded yajl and sys-proctree deps since it doesn't use them.

#### 0.91.0
* Stripped out the few bits of my cli stuff that I used and dropped app.rb, cli.rb and filemagic.rb, since all the functionality was present elsewhere.
* Fixed up the gem related files, generating a proper gemspec, fixed the deps.
* Fixed up the README.md to give a few usage examples and brief explanation.
