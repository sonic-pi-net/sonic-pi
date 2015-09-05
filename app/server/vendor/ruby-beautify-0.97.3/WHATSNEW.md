## 0.97.2
* Typo fix, thanks @andyw8.
* More typos, cleaned up thanks to @veelenga.
* @tnnn fixed a namespacing issue preventing version from working, thanks.

## 0.97.2
* Thanks goes to @bachue for discovering that `ruby-beautify` was not honoring spaces and indent count on file overwrites.

## 0.97.1
* Now with more config file flavor! (check the [README.md]).
* All tests green.

## 0.97.0
* Split up the spec into usage scenarios, easier to contribut too.
* Lots of refactoring.
* Some documentation.
* Added multiple file support back so that overwrite files is more interesting.
* Added an overwrite files flag, useful for fixing up a directory of files at once.

## 0.96.0
* Rewrote the syntax checker so that it pipes a string through stdout, doesn't need a temp file.
* Moved everything into the module to clean up the bin.
* @Sir-Irk fixed a bug where else and end end up on the same line.

## 0.95.0
* Merged a quick fix from @pkuykendall to catch block assignments with ||=
* Added a required version to the gemspec.

## 0.94.2
* Support for case statements thanks to @pkuykendall.
* @pkuykendall also brings us proper syntax formatting for assignments from the return value of an if check (that's a mouth full).

## 0.94.1
* Multiline string and embedded doc support thanks to @veelenga.

## 0.94.0
* Added a very basic spec.
* Added better support for piping and redirection (thanks @veelenga)
* Renamed the bin to ruby-beautify and added a link for backwards compat.
* Fixed extra spaces on blank lines (thanks @veelenga)

## 0.93.2
* Fixed a typo in the help usage (thanks @ronald)

## 0.93.0
* Complete rewrite of the lexing engine.

## 0.92.0
* Renamed a require in rspec so they would work again.
* Dropped filemagic since this already has excellent file parsing and is platform agnostic.
* Dropped the rest of app.rb, since it wasn't adding anything but line count at this point.
* Added a RELEASE.md to track changes as I go.
* Dropped the unneeded yajl and sys-proctree deps since it doesn't use them.

## 0.91.0
* Stripped out the few bits of my cli stuff that I used and dropped app.rb, cli.rb and filemagic.rb, since all the functionality was present elsewhere.
* Fixed up the gem related files, generating a proper gemspec, fixed the deps.
* Fixed up the README.md to give a few usage examples and brief explanation.
