Changelog
=========

v2.2.2.1 (2015-04-17)
---------------------

Bugs fixed:
 * builders/default: don't falsely diagnose multiline regexps (fixes #190). (whitequark)

v2.2.0.4 (2015-04-15)
---------------------

Features implemented:
 * Add Parser::Source::Map#node. (whitequark)
 * Add Parser::Source::Comment.associate_locations. (kubicle)

v2.2.0.1 (2014-12-27)
---------------------

Bugs fixed:
 * lexer.rl, lexer/literal: "{'a'::": don't parse as quoted label. (Peter Zotov)
 * Update syntax deviation warning to reflect 2.2 release. (Peter Zotov)

v2.2.0 (2014-12-25)
-------------------

Bugs fixed:
 * lexer.rl: "{'x':1,'y':{}}": fix lex_state after tLABEL_END. (Peter Zotov)

v2.2.0.pre.8 (2014-11-19)
-------------------------

API modifications:
 * parser/current: update 2.1 to 2.1.5 (fixes #174). (Peter Zotov)

v2.2.0.pre.7 (2014-11-03)
-------------------------

Bugs fixed:
 * parser/meta: add missing nodes (fixes #171). (Peter Zotov)

v2.2.0.pre.6 (2014-10-28)
-------------------------

API modifications:
 * parser/current: latest stable 2.1.x is 2.1.4, update warning. (hirocaster)

v2.2.0.pre.5 (2014-10-03)
-------------------------

Features implemented:
 * parser/current: add syntax deviation warning for 2.1.2. (Peter Zotov)
 * lexer.rl, ruby22.y: "{'x':1}": add tLABEL_END. (Peter Zotov)

Bugs fixed:
 * lexer.rl, ruby{21,22}.y: "def a b:\nreturn": fix #164. (Peter Zotov)
 * Fix for `ruby-rewrite` not rewriting files if any rewriter was loaded, due to it getting confused about the filename. (Jon Frisby)

v2.2.0.pre.4 (2014-08-09)
-------------------------

Bugs fixed:
 * builders/default: "not(x)": fix source map (fixes #158). (Peter Zotov)

v2.2.0.pre.3 (2014-07-02)
-------------------------

Features implemented:
 * Raise EncodingError when source includes invalid byte sequence (Yuji Nakayama)

Bugs fixed:
 * ruby{19,20,21}.y: "x def y; z do end end": save cmdarg stack to isolate command argument state from outer scope. (Peter Zotov)
 * ruby{19,20,21}.y: "tap (proc do end)": allow parenthesed do-block in cmdarg. (Peter Zotov)

v2.2.0.pre.2 (2014-06-14)
-------------------------

Bugs fixed:
 * ruby22.rb: include in gemspec. (Peter Zotov)

v2.2.0.pre.1 (2014-06-12)
-------------------------

Features implemented:
 * Add Source::Rewriter#transaction for atomic rewrite actions (Yuji Nakayama)
 * Raise Parser::ClobberingError for clobbering rewrite error (Yuji Nakayama)

Bugs fixed:
 * parser/current: fix the fallback case (refs #146). (Peter Zotov)
 * ruby22.y: "tap (proc do end)": allow parenthesed do-block in cmdarg. (Peter Zotov)

v2.1.9 (2014-04-21)
-------------------

API modifications:
 * Extend ast dependency to >= 1.1 \< 3.0. (Peter Zotov)
 * parser/current: fallback to latest released Ruby instead of raising (fixes #140). (Peter Zotov)

Features implemented:
 * ruby-rewrite: add a --modify switch for rewriters that modify AST (fixes #143). (Peter Zotov)

Bugs fixed:
 * lexer.rl: don't fail to parse string literals in huge files (fixes #142). (Peter Zotov)

v2.1.7 (2014-03-05)
-------------------

Bugs fixed:
 * lexer.rl: make sure all invalid escapes lead to fatal errors (fixes #136). (Peter Zotov)

v2.1.6 (2014-03-04)
-------------------

Features implemented:
 * Add the list of all node types within Parser::Meta. (Markus Schirp)

v2.1.5 (2014-02-24)
-------------------

Bugs fixed:
 * Parser::Base, ruby18.y: don't try to lookup Encoding on 1.8 (fixes #133). (Peter Zotov)

v2.1.4 (2014-01-11)
-------------------

Features implemented:
 * ruby22.y: "x def y; z do end end": save cmdarg stack to isolate command argument state from outer scope. (Peter Zotov)
 * Add Ruby 2.2 syntax. (Peter Zotov)

Bugs fixed:
 * Builders::Default: "super do end": super and zsuper are like send (fixes #131). (Peter Zotov)

v2.1.3 (2014-01-10)
-------------------

Bugs fixed:
 * lexer.rl: "/\//": don't include escaped delimiter in AST in its escaped form (fixes #125). (Peter Zotov)
 * Builders::Default: "return x y do end": correctly build AST for keywords followed by command (closes #129). (Peter Zotov)
 * Fix a bug where "ambiguous first argument" diagnostic was not emitted (Yuji Nakayama)
 * Source::Comment::Associator: don't die while associating with "__ENCODING__". (Peter Zotov)
 * ruby-parse: don't die when invoked with -L -e "__ENCODING__". (Peter Zotov)
 * Add missing source map for match-current-line (Yuji Nakayama)

v2.1.2 (2014-01-05)
-------------------

Bugs fixed:
 * lexer.rl: in "foo!= x", foo is tIDENTIFIER, not tFID (closes #126). (Peter Zotov)

v2.1.1 (2013-12-25)
-------------------

API modifications:
 * ruby21.y: Ruby 2.1 is released already. (Peter Zotov)

v2.1.0 (2013-12-25)
-------------------

API modifications:
 * Parser::Diagnostic: expose reason symbolically (closes #115, #116). (Ian MacLeod)
 * lexer.rl: coerce literals to UTF-8 in ASCII-encoded files if they contain \uXXXX (Peter Zotov)

Bugs fixed:
 * builders/default: represent heredocs with dedicated map (fixes #100). (Peter Zotov)

v2.1.0.pre1 (2013-11-12)
------------------------

API modifications:
 * lexer.rl: correctly handle __END__ with non-whitespace after it (Peter Zotov)
 * lexer.rl: handle \r in middle of a line as mere whitespace (Peter Zotov)
 * ruby{18,19,20,21}.y, builders/default: precisely point to tUMINUS_NUM. (Peter Zotov)

Features implemented:
 * lexer.rl, ruby21.y, builders/default: rational/complex literals. (Peter Zotov)

v2.0.0 (2013-10-06)
-------------------

API modifications:
 * Source::Rewriter: raise an exception if updates clobber each other. (Peter Zotov)
 * Source::Range#inspect: use full class name. (Peter Zotov)
 * lexer.rl: make EOF tokens actually pointing at EOF and zero-length. (Peter Zotov)
 * Source::Range#column_range: raise RangeError if range spans >1 line. (Peter Zotov)
 * Source::Comment::Associator: fix argument order. (Peter Zotov)

Features implemented:
 * Source::Comment: implement #inspect. (Peter Zotov)
 * Backport Array#bsearch from Ruby 2.0. (Peter Zotov)

v2.0.0.pre8 (2013-09-15)
------------------------

API modifications:
 * lexer.rl: make lexing faster and improve parsing speed by ~60%. (Peter Zotov)

v2.0.0.pre7 (2013-09-10)
------------------------

Features implemented:
 * Parser::Base: add #parse_with_comments, #parse_file_with_comments. (Trent Ogren)
 * lexer.rl (Ruby 2.1): "1end": lex non-exponent `e' separate from number. (Peter Zotov)

Bugs fixed:
 * lexer.rl: "->*{}": tLAMBEG at expr_beg (fixes #103). (Peter Zotov)
 * Source::Rewriter: apply actions in the insertion order. (Josh Cheek)

v2.0.0.pre5 (2013-07-31)
------------------------

Bugs fixed:
 * Remove a forgotten require. (Peter Zotov)

v2.0.0.pre4 (2013-07-31)
------------------------

API modifications:
 * source/comment: make #loc/#location return Source::Map for consistency (fixes #96). (Peter Zotov)

Features implemented:
 * source/comment/associator: skip shebang and encoding line by default (fixes #95). (Peter Zotov)

Bugs fixed:
 * ruby{19,20,21}.y, lexer.rl, builders/default: correct begin for ?a (fixes #92). (Peter Zotov)
 * ruby{18,19,20,21}.y, builders/default: don't add spurious begin/end for string parts (refs #92). (Peter Zotov)
 * Activate `diagnostics.all_errors_are_fatal` on non-MRI Rubies as a workaround (closes #93). (Peter Zotov)

v2.0.0.pre3 (2013-07-26)
------------------------

API modifications:
 * lexer.rl: add simple explicit output encoding for strings. (Peter Zotov)

Features implemented:
 * Source::Buffer: support for -(dos|unix|mac) and utf8-mac encodings. (Peter Zotov)
 * Source::Range#resize. (Peter Zotov)
 * Significantly improve speed for large (>100k) and very large (>1M) files. (Peter Zotov)

Bugs fixed:
 * ruby21.y: fix typos. (Peter Zotov)
 * builders/default: respect regexp encoding. (Peter Zotov)
 * lexer.rl: literal EOF (\0, \x04, \x1a) inside literals and comments. (Peter Zotov)
 * lexer.rl: "meth (lambda do end)" (1.8), "f x: -> do meth do end end": expr_cmdarg. (Peter Zotov)
 * lexer.rl: "\<\<E\nE\r\r\n": extraneous CRs are ignored after heredoc delimiter. (Peter Zotov)
 * lexer.rl: "%\nfoo\n": \n can be used as %-literal delimiter. (Peter Zotov)
 * source/buffer, lexer.rl: convert CRLF to LF prior to lexing. (Peter Zotov)
 * lexer.rl: "\<\<w; "\nfoo\nw\n"": interleaved heredoc and non-heredoc literals. (Peter Zotov)
 * builders/default: 1.8 did not descend into &&/|| in conditional context. (Peter Zotov)
 * lexer.rl: "1+a:a": respect context sensitivity in 1.8 label fallback. (Peter Zotov)
 * lexer.rl: ruby 1.8 is context-sensitive wrt/ locals as well. (Peter Zotov)
 * lexer.rl: "eof??a": expr_arg doesn't need space before character literal. (Peter Zotov)
 * lexer.rl: interleaved heredoc and interpolated double-quoted string. (Peter Zotov)
 * lexer.rl: "#{f:a}": interpolation starts expr_value, not expr_beg. (Peter Zotov)
 * lexer.rl: "\cM" is "\r", not an error. (Peter Zotov)
 * ruby{20,21}.y: constant op-assignment inside a def is not an error. (Peter Zotov)
 * lexer.rl: "when Date:" fix label fallback for 1.8 mode. (Peter Zotov)
 * ruby{19,20,21}.y: "->(scope){}; scope :foo": lambda identifier leakage. (Peter Zotov)
 * lexer.rl: "eh ?\r\n": don't eat tEH if followed by CRLF. (Peter Zotov)
 * lexer.rl: "f \<\<-TABLE\ndo |a,b|\nTABLE\nend": leave FSM after lexing heredoc. (Peter Zotov)
 * lexer.rl: "foo %\n bar": don't % at expr_arg as tSTRING_BEG. (Peter Zotov)
 * lexer.rl, lexer/literal: use lexer encoding for literal buffer. (Peter Zotov)
 * lexer.rl: "\u{9}": one-digit braced unicode escapes. (Peter Zotov)
 * Source::Buffer: don't chew \r from source lines. (Peter Zotov)
 * builders/default: don't die in eh_keyword_map if else branch is empty. (Peter Zotov)
 * lexer.rl: "0777_333": octal literals with internal underscores. (Peter Zotov)
 * lexer.rl: "let [] {}": goto tLBRACE_ARG after any closing braces. (Peter Zotov)
 * lexer.rl: "while not (1) do end": emit proper kDO* when in cond/cmdarg state. (Peter Zotov)
 * lexer.rl: "rescue=>": correctly handle rescue+assoc at expr_beg. (Peter Zotov)
 * lexer.rl: "puts 123do end": only trailing `_' and `e' in number are errors. (Peter Zotov)
 * lexer.rl: "begin; rescue rescue1; end": accept barewords at expr_mid. (Peter Zotov)
 * lexer.rl: "f.x!if 1": correct modifier handling in expr_arg. (Peter Zotov)
 * lexer.rl: "=begin\n#=end\n=end": only recognize =end at bol. (Peter Zotov)
 * builders/default: don't check for duplicate arguments in 1.8 mode. (Peter Zotov)
 * Don't attempt to parse magic encoding comment in 1.8 mode. (Peter Zotov)
 * lexer.rl: "\777": octal literals overflow. (Peter Zotov)
 * lexer.rl: "foo;\n__END__", "\na:b": whitespace in expr_value. (Peter Zotov)
 * lexer.rl: "\xE2\x80\x99": concatenation of byte escape sequences. (Peter Zotov)
 * lexer.rl: "E10", "E4U": don't conflate floats and identifiers. (Peter Zotov)
 * lexer.rl: "foo.bar= {1=>2}": return fid, = as separate tokens in expr_dot. (Peter Zotov)
 * lexer.rl: "def defined?": properly return defined? in expr_fname. (Peter Zotov)
 * lexer.rl: "Rainbows! do end", "foo.S?": allow bareword fid in expr_beg/dot. (Peter Zotov)

v2.0.0.pre2 (2013-07-11)
------------------------

Features implemented:
 * Allow to differentiate between __FILE__/__LINE__ and literals (closes #89). (Peter Zotov)
 * Add attribute `diagnostic' to Parser::SyntaxError (closes #88). (Peter Zotov)

Bugs fixed:
 * Don't treat byte order mark as an identifier (closes #91). (Peter Zotov)

v2.0.0.beta10 (2013-07-02)
--------------------------

Bugs fixed:
 * ruby-parse, ruby-rewrite: fix require of removed compatibility shim. (Peter Zotov)
 * lexer.rl: "def !@; end" unary bang. (Peter Zotov)

v2.0.0.beta9 (2013-06-28)
-------------------------

API modifications:
 * ruby{18,19,20,21}.y: removed obsolete warnings and linting. (Peter Zotov)

Features implemented:
 * builders/default: add keyword source range for BEGIN/END (fixes #85). (Peter Zotov)

Bugs fixed:
 * lexer.rl: "t=1;(a)?t:T" context sensitivity in expr_value (fixes #87). (Peter Zotov)
 * lexer.rl: keywords as labels, e.g. "unless:" (fixes #83, #84). (Peter Zotov)
 * lexer.rl: rectify usage of c_space/c_space_nl (fixes #81). (Peter Zotov)
 * ruby{18,19,20,21}.y: fix warnings for class/module in method body. (Peter Zotov)
 * lexer.rl: fix warning for ?\s. (Peter Zotov)
 * lexer.rl: expr_fname emits expr_beg-like keywords (fixes #82). (Peter Zotov)
 * lexer.rl: get rid of harmful nondeterminism in w_space (fixes #80). (Peter Zotov)
 * lexer/explanation: 1.8, 1.9 compatibility (fixes #76). (Peter Zotov)

v2.0.0.beta8 (2013-06-24)
-------------------------

Bugs fixed:
 * ast/processor: add missing node handlers (Yuji Nakayama)
 * ast/processor: rename some obsolete node handlers (Yuji Nakayama)

v2.0.0.beta7 (2013-06-22)
-------------------------

API modifications:
 * Implement a much more sane encoding model (closes #60). (Peter Zotov)

Features implemented:
 * builders/default: (while-post) and (until-post); (kwbegin) (fixes #70). (Peter Zotov)

Bugs fixed:
 * builders/default: don't swallow (begin) in "if (foo); end" (fixes #75). (Peter Zotov)

v2.0.0.beta6 (2013-06-17)
-------------------------

API modifications:
 * Get rid of "synthesized (nil)". If it's not in source, it's not in AST (fixes #71). (Peter Zotov)
 * lexer.rl, ruby{18,19,20,21}.y: source maps for interpolation (fixes #27). (Peter Zotov)

Features implemented:
 * ruby{18,19,20,21}.y, builders/default: lvar-injecting match (closes #69). (Peter Zotov)
 * builders/default: implicit matches (refs #69). (Peter Zotov)
 * builders/default: flip-flops (refs #69). (Peter Zotov)

Bugs fixed:
 * lexer.rl: fix an off-by-1 error in heredoc parsing. (Peter Zotov)
 * lexer.rl: don't fail on "alias $a $b\n# comment\nalias $c $d". (Peter Zotov)
 * builders/default: fix treatment of masgn in &&/|| (refs #69). (Peter Zotov)
 * ruby-parse: make -L command line option work again. (Peter Zotov)
 * ruby{18,19,20,21}.y: begin source map for "if foo\nthen bar end" (fixes #68). (Peter Zotov)
 * Source::Comment::Associator: gracefully terminate when out of comments (fixes #67). (Peter Zotov)

v2.0.0.beta5 (2013-06-08)
-------------------------

Bugs fixed:
 * Source::Buffer: better magic encoding comment recognition (fixes #65). (Peter Zotov)
 * lexer.rl: "{success?: true}" (fixes #66). (Peter Zotov)
 * Source::Buffer: if in doubt, treat data as UTF-8 (closes #60). (Peter Zotov)

v2.0.0.beta4 (2013-06-05)
-------------------------

Bugs fixed:
 * lexer.rl: fix heredoc parsing with CRLF line endings (closes #61). (Peter Zotov)
 * lexer.rl: fix premature ending of heredoc "\<\<D\nABCDEF\nD" (fixes #59). (Peter Zotov)

v2.0.0.beta3 (2013-05-29)
-------------------------

Bugs fixed:
 * AST::Processor: traverse "A, B = foo" (fixes #55). (Peter Zotov)
 * lexer.rl: correctly handle CRLF line endings (fixes #56). (Peter Zotov)
 * Fix traversing of "case; when a?; when b?; end" by AST::Processor. (Peter Zotov)
 * Correctly lex "foo a, b # comment\nbar" (fixes #54). (Peter Zotov)

v2.0.0.beta2 (2013-05-27)
-------------------------

Bugs fixed:
 * Actually return comments from Parser::Base#tokenize instead of nil. (Peter Zotov)

v2.0.0.beta1 (2013-05-25)
-------------------------

API modifications:
 * Completely rewrite whitespace handling in lexer (fixes #36). (Peter Zotov)
 * Rename Parser::AST::Node#source_map to #location, #src to #loc (closes #40). (Peter Zotov)
 * Rename Parser::Source::Range#to_source to #source (refs #40). (Peter Zotov)
 * Rename (cdecl) node to (casgn), remove (cvdecl) nodes (fixes #26). (Peter Zotov)

Features implemented:
 * Add Source::Comment.associate for mapping comments back to nodes (fixes #31). (Peter Zotov)
 * Return AST and comments from Parser::Base#parse_with_comments. (Peter Zotov)
 * Return comments from Parser::Base#tokenize (fixes #46). (Peter Zotov)
 * Add tokenizer, Parser::Base#tokenize (refs #46). (Peter Zotov)
 * lexer.rl: better location reporting for invalid unicode codepoints (fixes #38). (Peter Zotov)
 * lexer.rl: better location reporting for unterminated =begin (fixes #37). (Peter Zotov)
 * Better location reporting for hashes with labels. (Peter Zotov)
 * Add `dot' source map to (send) nodes (fixes #34). (Peter Zotov)
 * Significantly improve performance of Source::Buffer (fixes #28). (Peter Zotov)

Bugs fixed:
 * lexer.rl: fix lexing label at line_begin "foo:bar" (fixes #48). (Peter Zotov)
 * lexer.rl: "Option /^I/" is a method call (fixes #32). (Peter Zotov)
 * Don't allow destructive mutation of line cache in Source::Buffer. (Peter Zotov)
 * Fix quantifier in magic encoding parser (refs #33). (Peter Zotov)
 * Better handling of magic encoding comment edge cases (fixes #33). (Peter Zotov)

v1.3.2 (2013-05-13)
-------------------

Features implemented:
 * lexer.rl: disallow "$-" (dollar, dash, no character) special. (Peter Zotov)

Bugs fixed:
 * Source::Range: fix #to_source for multiline ranges. (Peter Zotov)
 * builders/default: source map for class/module name (fixes #24). (Peter Zotov)

v1.3.1 (2013-05-09)
-------------------

Bugs fixed:
 * ruby{19,20,21}.y: "def foo\n=begin\n=end\nend" (fixes #22). (Peter Zotov)
 * lexer.rl: "rescue::Exception" (fixes #23). (Peter Zotov)

v1.3.0 (2013-04-26)
-------------------

Bugs fixed:
 * lexer.rl: "alias foo bar \n alias bar baz". (Peter Zotov)

v1.2.0 (2013-04-25)
-------------------

Bugs fixed:
 * lexer.rl: lex "def String.foo; end" correctly (fixes #16). (Peter Zotov)
 * lexer.rl: reject "1end", "1.1end". (Peter Zotov)

v1.1.0 (2013-04-18)
-------------------

API modifications:
 * ruby19.y, ruby20.y, ruby21.y: check for Encoding support (fixes #9). (Peter Zotov)

Features implemented:
 * builders/default: ignore duplicate _ args (>=1.9), _.* args (>1.9) (fixes #5). (Peter Zotov)
 * builders/default: detect duplicate argument names (refs #5). (Peter Zotov)
 * lexer.rl: "def foo bar: 1; end" (for ruby 2.1) (fixes #15). (Peter Zotov)
 * ruby21.y: required keyword arguments. (Peter Zotov)

Bugs fixed:
 * ruby20.y, ruby21.y: "foo::A += 1" and friends (scoped constant op-asgn). (Peter Zotov)

v1.0.1 (2013-04-18)
-------------------

Bugs fixed:
 * builders/default: %Q{#{1}} and friends (fixes #14). (Peter Zotov)

v1.0.0 (2013-04-17)
-------------------

Features implemented:
 * ruby20.y: "meth 1 do end.fun(bar) {}" and friends. (Peter Zotov)
 * ruby20.y: keyword arguments. (Peter Zotov)
 * ruby20.y: { **kwsplat }. (Peter Zotov)

v0.9.2 (2013-04-16)
-------------------

Features implemented:
 * lexer.rl: "-> (a) {}". (Peter Zotov)
 * builders/default: treat &&/|| lhs/rhs as conditional context. (Peter Zotov)
 * ruby20.y: "class Foo \< a:b; end". (Peter Zotov)
 * lexer.rl: "class \<\< a:b". (Peter Zotov)
 * ruby19.y, ruby20.y: "f { || a:b }". (Peter Zotov)
 * ruby19.y, ruby20.y: "def foo() a:b end", "def foo\n a:b end". (Peter Zotov)
 * lexer.rl: %i/%I. (Peter Zotov)
 * lexer.rl: warn at "foo **bar". (Peter Zotov)
 * lexer.rl: ** at expr_beg is tDSTAR. (Peter Zotov)
 * ruby20.y: "f {|;\nvar\n|}". (Peter Zotov)
 * ruby20.y: "p () {}". (Peter Zotov)
 * ruby20.y: "p begin 1.times do 1 end end". (Peter Zotov)
 * ruby20.y: better error message for BEGIN{} in a method body. (Peter Zotov)

Bugs fixed:
 * lexer.rl, ruby18.y, ruby19.y, ruby20.y: "%W[#{a}#@b foo #{c}]". (Peter Zotov)
 * lexer.rl: parse "foo=1; foo / bar #/" as method call on 1.8, division on 1.9. (Peter Zotov)
 * ruby18.y, ruby19.y: BEGIN{} does not introduce a scope. (Peter Zotov)
 * lexer.rl: improve whitespace handling. (Peter Zotov)

v0.9.0 (2013-04-15)
-------------------

API modifications:
 * runtime compatibility with 1.8.7. (Peter Zotov)

Features implemented:
 * builders/default: check for multiple assignment in conditions (fixes #4). (Peter Zotov)
 * builders/default: check if actual block and blockarg are passed (fixes #6). (Peter Zotov)
 * ruby19.y: "foo::A += m foo". (Peter Zotov)
 * ruby18.y, ruby19.y: "rescue without else is useless" warning. (Peter Zotov)
 * ruby19.y: 99.16% coverage, 100% sans error recovery. (Peter Zotov)
 * ruby19.y: mlhs arguments "def foo((a, *, p)) end". (Peter Zotov)
 * ruby19.y: "fun (1) {}" and friends. (Peter Zotov)
 * ruby19.y: mlhs post variables "a, *b, c = ...". (Peter Zotov)
 * builders/default: @@a |= 1; def f; @@a |= 1; end. (Peter Zotov)
 * ruby18.y: fun (&foo). (Peter Zotov)
 * ruby18.y: block formal arguments. 99.33% coverage. (Peter Zotov)
 * ruby18.y: fun(meth 1 do end); fun(1, meth 1 do end). (Peter Zotov)
 * ruby18.y: "meth 1 do end.fun(bar)" and friends. (Peter Zotov)
 * ruby18.y: foo () {}; a.foo () {}; a::foo () {}. (Peter Zotov)
 * ruby18.y: various call argument combinations. (Peter Zotov)
 * ruby18.y: foo (1, 2); foo (). (Peter Zotov)
 * ruby18.y: foo (1).to_i. (Peter Zotov)
 * ruby18.y: fun{}; fun(){}; fun(1){}; fun do end. (Peter Zotov)
 * ruby18.y: foo.fun bar. (Peter Zotov)
 * lexer.rl, ruby18.y: add support for cond/cmdarg stack states. (Peter Zotov)
 * ruby18.y: rescue. (Peter Zotov)
 * ruby18.y: begin end while|until (tests only). (Peter Zotov)
 * ruby18.y: case. (Peter Zotov)
 * ruby18.y: foo[m bar]. (Peter Zotov)
 * ruby18.y: for..in. (Peter Zotov)

Bugs fixed:
 * lexer.rl: handle : at expr_beg as a symbol, at expr_end as tCOLON. (Peter Zotov)
 * lexer.rl: handle "rescue #foo\nbar". (Peter Zotov)
 * lexer.rl: handle "foo.#bar\nbaz". (Peter Zotov)
 * lexer.rl: fix location info for symbols. (Peter Zotov)
 * lexer.rl: handle \<backslash>\<nl> at expr_beg. (Peter Zotov)
 * lexer.rl: emit tCONSTANT/tIDENTIFIER/tFID in expr_dot. (Peter Zotov)
 * lexer.rl: correctly disambiguate "x ::Foo" as tIDENT, tCOLON3, ... (Peter Zotov)
 * lexer.rl: correctly disambiguate ident!= as tIDENTIFIER, tNEQ. (Peter Zotov)
 * lexer.rl: correctly report the %r%% tREGEXP_BEG value as %r%. (Peter Zotov)
 * ruby19.y: emit correct error on "nil = 1" and friends. (Peter Zotov)
 * ruby19.y: 1.9 permits empty symbol literals. (Peter Zotov)
 * ruby18.y: foo(&bar). (Peter Zotov)
 * lexer.rl: don't lookahead two tokens on "func %{str} do". (Peter Zotov)
 * lexer.rl: fix lexing of non-interp heredoc with trailing backslash. (Peter Zotov)
 * lexer.rl: fix erroneous number and =begin lookahead in expr_beg. (Peter Zotov)
 * lexer.rl: fix stack corruption. (Peter Zotov)
 * lexer.rl: /= at expr_beg. (Peter Zotov)
 * lexer.rl: class\<\<self. (Peter Zotov)
 * fix lexing comments at expr_beg "{#1\n}". (Peter Zotov)

