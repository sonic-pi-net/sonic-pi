diff --git a/README.md b/README.md
index 87f10f4..09ba8c0 100644
--- a/README.md
+++ b/README.md
@@ -1,10 +1,23 @@
 # Rouge
 
-This project is not yet finished, but this works:
+This project needs your help!  There are lots of lexers to be implemented / ported / fixed, and features that need to be added or implemented.  If you'd like to help out, send me a pull request (even if it's not done yet!).  Bonus points for feature branches.
+
+## Usage
 
 ``` ruby
-formatter = Rouge::Formatters::HTML.new
+# make some nice lexed html, compatible with pygments stylesheets
+formatter = Rouge::Formatters::HTML.new(:css_class => '.highlight')
 Rouge.highlight(File.read('/etc/bash.bashrc'), 'shell', formatter)
+
+# apply a theme
+Rouge::Themes::ThankfulEyes.new(:scope => '.highlight').render
 ```
 
-More features, documentation, lexers, and formatters to come.  Help is appreciated, too, if you think this is awesome :)
+Rouge aims to be simple to extend, and to be a drop-in replacement pygments, with the same quality of output.
+
+### Advantages to pygments.rb
+* No python bridge is necessary - you can deploy it on Heroku effortlessly, without the need for [epic hacks][].
+
+### Advantages to CodeRay
+
+[epic hacks]: https://github.com/rumblelabs/pygments-heroku
diff --git a/lib/rouge.rb b/lib/rouge.rb
index e86da00..c947a8b 100644
--- a/lib/rouge.rb
+++ b/lib/rouge.rb
@@ -16,6 +16,7 @@ load_dir = Pathname.new(__FILE__).dirname
 load load_dir.join('rouge/token.rb')
 load load_dir.join('rouge/lexer.rb')
 load load_dir.join('rouge/lexers/text.rb')
+load load_dir.join('rouge/lexers/diff.rb')
 load load_dir.join('rouge/lexers/shell.rb')
 load load_dir.join('rouge/lexers/javascript.rb')
 
diff --git a/lib/rouge/token.rb b/lib/rouge/token.rb
index ab1701b..155fa52 100644
--- a/lib/rouge/token.rb
+++ b/lib/rouge/token.rb
@@ -112,6 +112,7 @@ module Rouge
     token 'Keyword.Type',                'kt'
 
     token 'Name',                        'n'
+
     token 'Name.Attribute',              'na'
     token 'Name.Builtin',                'nb'
     token 'Name.Builtin.Pseudo',         'bp'
