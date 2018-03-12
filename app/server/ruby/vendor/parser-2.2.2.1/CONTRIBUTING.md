Contributing to Parser
----------------------

Parser uses [Semantic Versioning](http://semver.org). Additionally, Parser employs a script to extract information from VCS (git) log and form a Changelog file. Thus, each commit which affects the public API in any way must be marked with one of the following sigils, or characters at the beginning of line:

 * `-` for bugfixes. For example: `- lexer.rl: fixed lexing of "alias $foo $bar".`
 * `+` for features. For example: `+ Implemented Parser::Rewriter, a module for non-intrusive rewriting of source code.`
 * `*` for miscellaneous changes. For example: `* Converted measurement units from metric to imperial.`

These categories map nicely to semantic versioning: `-` bugfixes increment patchlevel, `+` features increment minor version, `*` API changes increment major version.
