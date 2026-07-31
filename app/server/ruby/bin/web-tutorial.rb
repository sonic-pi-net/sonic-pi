#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# Builds the sonic-pi.net web tutorial as a fully self-contained bundle from
# the repo (no external assets). Turns the tutorial HTML produced by
# qt-doc.rb (app/gui/book/Sonic Pi - Tutorial.html) into the two-column
# listingg/contentss web page, then copies the vendored css/js and the
# tutorial images alongside it. Run qt-doc.rb first, then this.
#
# Assets live in etc/www/tutorial/{css,js} (tracked). Output is a complete
# web root: <out>/tutorial.html + css/ + js/ + media/.
#
# Usage: web-tutorial.rb [input book HTML] [output dir] [version]

require 'fileutils'

repo_root = File.expand_path("../../../../..", __FILE__)
book_dir  = File.join(repo_root, "app/gui/book")
asset_dir = File.join(repo_root, "etc/www/tutorial")
img_src      = File.join(repo_root, "etc/doc/images/tutorial")
logo_src      = File.join(repo_root, "app/gui/images/logo-smaller.png")
logo_dark_src = File.join(repo_root, "app/gui/images/logo-smaller-dark.png")

input   = ARGV[0] || File.join(book_dir, "Sonic Pi - Tutorial.html")
out_dir = ARGV[1] || File.join(repo_root, "app/build/web-tutorial")
version = ARGV[2] || ENV["SONIC_PI_VERSION"] || "5.0"

abort "Input not found: #{input}\nRun qt-doc.rb first." unless File.exist?(input)

raw = File.read(input, encoding: "UTF-8")

# --- Split the book fragment into TOC and content -------------------------
body_idx = raw.index("<body>")
abort "No <body> in #{input}" unless body_idx
inner = raw[(body_idx + "<body>".length)..]
inner = inner.sub(%r{</body>.*\z}m, "")

h1_idx = inner.index(/<h1[\s>]/)
abort "No <h1> content in #{input}" unless h1_idx
toc     = inner[0...h1_idx]
content = inner[h1_idx..]

# --- Map numeric tutorial_item_N ids to section-N-M slugs -----------------
# The TOC numbers every entry (content H1s drop the letter on appendices),
# e.g.  <a href="#tutorial_item_2634">   1.1 Live Coding</a>   -> section-1-1
#       <a href="#tutorial_item_2698">   A.6 Musical Minecraft</a> -> section-A-6
#       <a href="#tutorial_item_2692">A Appendix A - MagPi ...</a>  -> section-A
id_map = {}
toc.scan(%r{href="#(tutorial_item_\d+)">\s*([0-9A-Za-z.]+)\s}) do |id, num|
  id_map[id] = "section-" + num.gsub(".", "-")
end
id_map.each do |id, slug|
  toc.gsub!(id, slug)
  content.gsub!(id, slug)
end

# --- Web-ify -------------------------------------------------------------
# Outermost TOC list becomes the top-level list (styled as the sidebar).
toc = toc.sub('<ul class="toc">', '<ul class="toc top-level">')

# GUI-relative image paths -> bundle-relative media paths.
content.gsub!("../../../etc/doc/images/", "media/images/")

# Tag code blocks as Ruby so highlight.js colours them consistently.
content.gsub!("<pre><code>", '<pre><code class="ruby">')

toc     = toc.strip
content = content.strip

# --- Site head (self-contained: all assets served from the bundle) -------
head = <<~'HTML'
<!DOCTYPE HTML>
<html>
  <head>
    <title>Sonic Pi - Tutorial</title>

    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <meta name="description" content="Sonic Pi Tutorial - Learn how to code music." />
    <meta name="keywords" content="live coding music code programming learn teach tutorial" />

    <link rel="stylesheet" href="css/doc-styles.css" type="text/css"/>
    <link rel="stylesheet" href="css/layout.css" type="text/css"/>
    <link rel="stylesheet" href="css/themes.css" type="text/css"/>

    <script src="js/theme.js"></script>
    <script src="js/highlight.min.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>

    <!-- copy-to-clipboard buttons on code blocks (vanilla, no jQuery) -->
    <script src="js/clipboard.min.js"></script>
    <script>
document.addEventListener('DOMContentLoaded', function () {
  if (typeof Clipboard === 'undefined' || !Clipboard.isSupported()) return;
  var blocks = document.querySelectorAll('#contentss pre code');
  for (var i = 0; i < blocks.length; i++) {
    var code = blocks[i];
    code.id = 'code-snippet-' + i;
    var btn = document.createElement('button');
    btn.className = 'copy-button';
    btn.textContent = 'Copy';
    btn.setAttribute('data-clipboard-target', '#' + code.id);
    code.parentElement.appendChild(btn);
  }
  new Clipboard('.copy-button');
});
    </script>
  </head>
HTML

page = +""
page << head
page << "\n  <body>\n\n\n"
page << %{    <div id="listingg">\n}
page << %{<details id="toc" open>\n}
page << %{<summary>Contents</summary>\n\n}
page << toc << "\n\n"
page << %{</details>\n}
page << %{    </div>\n\n}
page << %{<div id="contentss">\n\n}
page << %{  <span class="image fit"><img class="logo-light" src="media/logos/sonic-pi-logo.png" alt="Sonic Pi" /><img class="logo-dark" src="media/logos/sonic-pi-logo-dark.png" alt="Sonic Pi" /></span>\n}
page << content << "\n\n"
page << "<hr/>\n\n"
page << %{</div>\n\n}
# The footer is a sibling of the content, not inside it, so on small screens it
# can be a pinned bottom bar (the theme switcher has to stay reachable without
# scrolling the whole tutorial). On wide screens it's position:fixed anyway, so
# where it sits in the DOM makes no visual difference.
page << %{<div id="doc-footer">\n}
# Long/short label pairs so the footer collapses to a single thin row on a
# phone. aria-label carries the full wording either way, so shortening the
# visible text costs nothing for a screen reader.
page << %{  <a href="/" id="doc-nav" aria-label="Back to sonic-pi.net">&larr;<span class="footer-long"> Back to sonic-pi.net</span></a>\n}
page << %{  <span id="theme-switch" role="group" aria-label="Colour theme">}
page << %{<button type="button" data-theme="light">Light</button>}
page << %{<button type="button" data-theme="dark">Dark</button>}
page << %{<button type="button" data-theme="hc" aria-label="High Contrast">}
page << %{<span class="footer-short">HC</span><span class="footer-long">High Contrast</span></button></span>\n}
page << %{  <p id="doc-version"><span class="footer-long">Sonic Pi Tutorial </span>v#{version}</p>\n\n}
page << "</div>\n"
page << "\n\n</body>\n</html>\n"

# --- Assemble the self-contained bundle ----------------------------------
FileUtils.mkdir_p(out_dir)
File.write(File.join(out_dir, "tutorial.html"), page)

# Vendored css/js from the repo.
FileUtils.cp_r(File.join(asset_dir, "css"), out_dir)
FileUtils.cp_r(File.join(asset_dir, "js"),  out_dir)

# Media: tutorial images + logo, from the repo.
FileUtils.mkdir_p(File.join(out_dir, "media/images/tutorial"))
FileUtils.cp_r(Dir.glob(File.join(img_src, "*")), File.join(out_dir, "media/images/tutorial"))
FileUtils.mkdir_p(File.join(out_dir, "media/logos"))
FileUtils.cp(logo_src,      File.join(out_dir, "media/logos/sonic-pi-logo.png"))      if File.exist?(logo_src)
FileUtils.cp(logo_dark_src, File.join(out_dir, "media/logos/sonic-pi-logo-dark.png")) if File.exist?(logo_dark_src)

puts "Wrote self-contained bundle to #{out_dir}"
puts "  tutorial.html (#{page.bytesize} bytes, #{id_map.size} sections, version #{version})"
puts "  + css/ js/ media/ (all from the repo, no external assets)"
