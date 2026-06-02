#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'cgi'
require 'fileutils'

require_relative "../core.rb"
require_relative "../paths"
require_relative "../lib/sonicpi/synths/synthinfo"
require_relative "../lib/sonicpi/util"
require_relative "../lib/sonicpi/runtime"
require_relative "../lib/sonicpi/lang/core"
require_relative "../lib/sonicpi/lang/sound"
require_relative "../lib/sonicpi/lang/midi"
require_relative "../lib/sonicpi/note"
require_relative "../lib/sonicpi/chord"
require_relative "../lib/sonicpi/scale"


include SonicPi::Util

# List of all languages with GUI translation files
@lang_names = Hash[
  "ar" => "اَلْعَرَبِيَّةُ", # Arabic
  "bg" => "български", # Bulgarian
  "bn" => "বাংলা", # Bengali/Bangla
  "bs" => "Bosanski", # Bosnian
  "ca" => "Català", # Catalan
  "ca@valencia" => "Valencià", # Valencian
  "cs" => "Čeština", # Czech
  "da" => "Dansk", # Danish
  "de" => "Deutsch", # German
  "el" => "ελληνικά", # Greek
  "en" => "English", # English
  "en_AU" => "English (Australian)", # English (Australian)
  "en_GB" => "English (UK)", # English (UK) - default language
  "en_US" => "English (US)", # English (US)
  "eo" => "Esperanto", # Esperanto
  "es" => "Español", # Spanish
  "et" => "Eesti keel", # Estonian
  "eu" => "Euskara", # Basque
  "fa" => "فارسی", # Persian
  "fi" => "Suomi", # Finnish
  "fr" => "Français", # French
  "ga" => "Gaeilge", # Irish
  "gl" => "Galego", # Galician
  "he" => "עברית", # Hebrew
  "hi" => "हिन्दी", # Hindi
  "hu" => "Magyar", # Hungarian
  "hy" => "Հայերեն", # Armenian
  "id" => "Bahasa Indonesia", # Indonesian
  "is" => "Íslenska", # Icelandic
  "it" => "Italiano", # Italian
  "ja" => "日本語", # Japanese
  "ka" => "ქართული", # Georgian
  "ko" => "한국어", # Korean
  "nb" => "Norsk Bokmål", # Norwegian Bokmål
  "nl" => "Nederlands", # Dutch (Netherlands)
  "pl" => "Polski", # Polish
  "pt" => "Português", # Portuguese
  "pt_BR" => "Português do Brasil", # Brazilian Portuguese
  "ro" => "Română", # Romanian
  "ru" => "Pусский", # Russian
  "si" => "සිංහල", # Sinhala/Sinhalese
  "sk" => "Slovenčina",#/Slovenský Jazyk", # Slovak/Slovakian
  "sl" => "Slovenščina",#/Slovenski Jezik", # Slovenian
  "sv" => "Svenska", # Swedish
  "sw" => "Kiswahili", # Swahili
  "th" => "ไทย", # Thai
  "tr" => "Türkçe", # Turkish
  "ug" => "ئۇيغۇر تىلى", # Uyghur
  "uk" => "Українська", # Ukranian
  "vi" => "Tiếng Việt", # Vietnamese
  "zh" => "中文", # Chinese
  "zh-Hans" => "简体中文", # Chinese (Simplified)
  "zh_HK" => "廣東話", # Chinese (Traditional, Hong Kong)
  "zh_TW" => "臺灣華語" # Chinese (Traditional, Taiwan)
]

FileUtils::rm_rf "#{SonicPi::Paths.qt_gui_path}/help/"
FileUtils::mkdir "#{SonicPi::Paths.qt_gui_path}/help/"

FileUtils::rm_rf "#{SonicPi::Paths.qt_gui_path}/info/"
FileUtils::mkdir "#{SonicPi::Paths.qt_gui_path}/info/"

FileUtils::rm_rf "#{SonicPi::Paths.qt_gui_path}/book/"
FileUtils::mkdir "#{SonicPi::Paths.qt_gui_path}/book/"

docs = []
filenames = []
count = 0

# valid names: lang, synths, fx, samples, examples
make_tab = lambda do |name, doc_items, titleize=false, should_sort=true, with_keyword=false, page_break=false, chapters=false, lang="en"|
  return if doc_items.empty?
  list_widget = "#{name}NameList"
  layout = "#{name}Layout"
  tab_widget = "#{name}TabWidget"
  help_pages = "#{name}HelpPages"

  docs << "\n"
  docs << "  // #{name} info\n"

  docs << "  struct help_page #{help_pages}[] = {\n"
  doc_items = doc_items.sort if should_sort

  book = ""
  toc = "<ul class=\"toc\">\n"
  toc_level = 0

  doc_items.each do |n, doc|
    title = n
    if titleize == :titleize then
      title = title.titleize
      # HPF et al get capitalized
      if name == 'fx' and title =~ /pf$/ then
        title = title.upcase
      end
    end

    item_var = "#{name}_item_#{count+=1}"
    filename = "help/#{item_var}.html"

    if title.start_with?("   ") then
      if toc_level == 0 then
        toc << "<ul class=\"toc\">\n"
        toc_level += 1
      end
    else
      if toc_level == 1 then
        toc << "</ul>\n"
        toc_level -= 1
      end
    end
    toc << "<li><a href=\"\##{item_var}\">#{title.gsub(/"/, '&quot;')}</a></li>\n"

    docs << "    { "

    docs << "QString::fromUtf8(" unless title.ascii_only?
    docs << "\"#{title.gsub(/"/, '\\"')}\""
    docs << ")" unless title.ascii_only?

    docs << ", "

    if with_keyword then
      docs << "\"#{n.downcase}\""
    else
      docs << "NULL"
    end

    docs << ", "
    docs << "\"qrc:///#{filename}\""
    docs << "},\n"

    filenames << filename

    File.open("#{SonicPi::Paths.qt_gui_path}/#{filename}", 'w') do |f|
      f << "#{doc}"
    end

    if chapters then
      c = title[/\A\s*[0-9]+(\.[0-9]+)?/]
      doc.gsub!(/(<h1.*?>)/, "\\1#{c} - ")
    end
    if page_break then
      doc.gsub!(/<h1.*?>/, "<h1 id=\"#{item_var}\" style=\"page-break-before: always;\">")
    else
      doc.gsub!(/<h1.*?>/, "<h1 id=\"#{item_var}\">")
    end
    book << doc
    book << "<hr/>\n"
  end

  while toc_level >= 0 do
    toc << "</ul>\n"
    toc_level -= 1
  end

  book_body = book[/<body.*?>/]
  book.gsub!(/<\/?body.*?>/, '')
  book.gsub!(/<meta http-equiv.*?>/, '')
  File.open("#{SonicPi::Paths.qt_gui_path}/book/Sonic Pi - #{name.capitalize}" + (lang != "en" ? " (#{lang})" : "") + ".html", 'w') do |f|
    f << "<link rel=\"stylesheet\" href=\"../theme/light/doc-styles.css\" type=\"text/css\"/>\n"
    f << "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n"
    f << book_body << "\n"
    f << toc << "\n"
    f << book << "\n"
    f << "</body>\n"
  end

  docs << "  };\n\n"
  docs << "  addHelpPage(createHelpTab(tr(\"#{name.capitalize}\")), #{help_pages}, #{doc_items.length});\n\n"

  docs
end


make_tutorial = lambda do |lang|

  docs << "\n  // language #{lang}\n"
  tutorial_html_map = {}
  if lang == "en" then
    markdown_path = SonicPi::Paths.tutorial_path
  else
    markdown_path = File.expand_path("../generated/#{lang}/tutorial", SonicPi::Paths.tutorial_path)
  end
  Dir["#{markdown_path}/*.md"].sort.each do |path|
    f = File.open(path, 'r:UTF-8')
    # read first line (title) of the markdown, use as title
    name = f.readline.strip
    # indent subchapters
    name = "   #{name}" if name.match(/\A[A-Z0-9]+\.[0-9]+ /)
    # read remaining content of markdown
    markdown = f.read
    html = SonicPi::MarkdownConverter.convert markdown
    tutorial_html_map[name] = html
  end

  make_tab.call("tutorial", tutorial_html_map, false, false, false, true, true, lang)
end


example_html_map = {}
example_dirs = ["Apprentice", "Illusionist", "Magician", "Sorcerer", "Wizard", "Algomancer"]
example_dirs.each do |ex_dir|
  Dir["#{SonicPi::Paths.examples_path}/#{ex_dir.downcase}/*.rb"].sort.each do |path|
    bname = File.basename(path, ".rb")
    bname = bname.titleize
    name = "[#{ex_dir}] #{bname}"
    lines = IO.readlines(path).map(&:chop).map{|s| CGI.escapeHTML(s)}
    html = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n"
    html << "<body class=\"example\">\n"
    html << '<h1>'
    html << "# #{bname}"
    html << '</h1>'
    html << "<p><pre><code>\n"

    html << "#{lines.join("\n")}\n\n</code></pre></p>\n"
    html << "</body>\n"
    example_html_map[name] = html
  end
end

ruby_html_map = {
#  "n.times" => "Loop n times",
#  "loop" => "Loop forever",
}

# this will sort locale code names by reverse length
# to make sure that a more specific locale is handled
# before the generic language code,
# e.g., "de_CH" should be handled before "de"
languages =
  Dir[File.expand_path("../lang/sonic-pi-tutorial-*.po", SonicPi::Paths.tutorial_path)].
  map { |p| File.basename(p).gsub(/sonic-pi-tutorial-(.*?).po/, '\1') }.
  sort_by {|n| [-n.length, n]}

docs << "\n"

# first, try to match all non-default languages (those that aren't "en")
languages.each do |lang|
  docs << "if (this->ui_language.startsWith(\"#{lang}\")) {\n"
  make_tutorial.call(lang)
  docs << "} else "
end

# finally, add the default language ("en")
docs << "{\n" unless (languages.empty?)
make_tutorial.call("en")
docs << "}\n" unless (languages.empty?)

make_tab.call("examples", example_html_map, false, false, false, true)
make_tab.call("synths", SonicPi::Synths::SynthInfo.synth_doc_html_map, :titleize, true, true, true)
make_tab.call("fx", SonicPi::Synths::SynthInfo.fx_doc_html_map, :titleize, true, true, true)
make_tab.call("samples", SonicPi::Synths::SynthInfo.samples_doc_html_map, false, true, false, true)
make_tab.call("lang", SonicPi::Lang::Core.docs_html_map.merge(SonicPi::Lang::Sound.docs_html_map).merge(ruby_html_map), false, true, true, false)

# One-line, C++-string-safe summary text for the completion popup.
summary_clean = lambda do |s|
  t = s.to_s.gsub(/\s+/, ' ').strip
  t = t[0, 90]
  t.gsub(/[\\"]/) { |m| "\\" + m }
end
# Full docstring escaped for a C++ string literal, preserving newlines (so
# markdown renders) and capped to a sane length.
doc_escape = lambda do |s|
  s.to_s.gsub(/[\\"]/) { |m| "\\" + m }.gsub("\r", "").gsub("\n", "\\n")[0, 30000]
end
# Emit a QString::fromUtf8(...) expression for a doc body, splitting the escaped
# text into adjacent string literals so no single literal exceeds MSVC's
# ~16 KB-per-literal limit (error C2026). The compiler concatenates adjacent
# literals, and doc_escape's cap keeps the combined size well under MSVC's 64 KB
# concatenation limit. We budget by *byte* length and only cut between
# characters (never mid multi-byte UTF-8) and never in the middle of a
# \\-escape, so the literals stay valid.
LITERAL_MAX_BYTES = 8000
qutf8_doc = lambda do |raw|
  esc = doc_escape.call(raw)
  literals = []
  cur = +""
  cur_bytes = 0
  pending_escape = false
  esc.each_char do |ch|
    if cur_bytes >= LITERAL_MAX_BYTES && !pending_escape
      literals << cur
      cur = +""
      cur_bytes = 0
    end
    cur << ch
    cur_bytes += ch.bytesize
    pending_escape = (ch == "\\") ? !pending_escape : false
  end
  literals << cur unless cur.empty? && !literals.empty?
  literals << "" if literals.empty?
  "QString::fromUtf8(" + literals.map { |c| "\"#{c}\"" }.join("\n    ") + ")"
end
# opt name -> short doc, collected across all synths/fx (first one wins).
opt_summaries = {}

# An opt's default value as a bare display string, or nil when there's no
# meaningful one (consumers wrap it in backticks as needed).
fmt_default = lambda do |d|
  if d.is_a?(Numeric) || d == true || d == false
    d.to_s
  elsif d.is_a?(Symbol)
    d.inspect
  elsif d.is_a?(String) && !d.empty?
    d
  end
end

# Minimal inline HTML for an opt docstring: escape <,>,& and turn `code` spans
# into <code> (opt docs are otherwise plain prose).
opt_doc_html = lambda do |s|
  s.to_s.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')
   .gsub(/`([^`]+)`/, '<code>\1</code>')
end

# The "Opts" section appended to a synth/fx docstring, as HTML (so opt names can
# be coloured): a compact names+defaults summary grid to scan, then a description
# per opt — each its own block (name heading + doc), separated by a rule.
opts_html = lambda do |arg_info|
  return "" if arg_info.empty?
  # Summary: a small-font grid, 2 opts per row (a name cell + a value cell each).
  cells = arg_info.map do |ak, info|
    ds = fmt_default.call(info[:default])
    # opt name links to its description block below (anchor named after the opt).
    "<td><a href=\"##{ak}\"><code>#{ak}:</code></a></td><td>#{ds ? opt_doc_html.call(ds) : ''}</td>"
  end
  rows = cells.each_slice(2).map { |s| "<tr>#{s.join}</tr>" }.join
  table = "<table cellspacing=\"0\" cellpadding=\"4\" style=\"font-size:small\">#{rows}</table>"
  blocks = arg_info.map do |ak, info|
    head = "<a name=\"#{ak}\"></a><b><code>#{ak}:</code></b>"
    head += " <i>(slidable)</i>" if info[:slidable]
    body = info[:doc].to_s.strip
    body.empty? ? "<p>#{head}</p>" : "<p>#{head}</p><p>#{opt_doc_html.call(body)}</p>"
  end.join("<hr/>")
  "<p><b>Opts</b></p>#{table}<p>&nbsp;</p>#{blocks}"
end

docs << "  // FX arguments for autocompletion\n"
docs << "  QStringList fxtmp;\n"
SonicPi::Synths::SynthInfo.get_all.each do |k, v|
  next unless v.is_a? SonicPi::Synths::FXInfo
  next if (k.to_s.include? 'replace_')
  safe_k = k.to_s[3..-1]
  docs << "  // fx :#{safe_k}\n"
  docs << "  fxtmp.clear(); fxtmp "
  v.arg_info.each do |ak, av|
    docs << "<< \"#{ak}:\" ";
    opt_summaries[ak] ||= av if av[:doc]
  end
  docs << ";\n"
  docs << "  autocomplete->addFXArgs(\":#{safe_k}\", fxtmp);\n"
  docs << "  autocomplete->setSummary(\":#{safe_k}\", QString::fromUtf8(\"#{summary_clean.call(v.name)}\"));\n"
  fx_doc = Kramdown::Document.new(v.doc.to_s.strip).to_html + opts_html.call(v.arg_info)
  docs << "  autocomplete->setDoc(\":#{safe_k}\", #{qutf8_doc.call(fx_doc)});\n\n"
end


SonicPi::Synths::SynthInfo.get_all.each do |k, v|
  next unless v.is_a? SonicPi::Synths::SynthInfo
  docs << "  // synth :#{k}\n"
  docs << "  fxtmp.clear(); fxtmp "
  v.arg_info.each do |ak, av|
    docs << "<< \"#{ak}:\" ";
    opt_summaries[ak] ||= av if av[:doc]
  end
  docs << ";\n"
  docs << "  autocomplete->addSynthArgs(\":#{k}\", fxtmp);\n"
  docs << "  autocomplete->setSummary(\":#{k}\", QString::fromUtf8(\"#{summary_clean.call(v.name)}\"));\n"
  synth_doc = Kramdown::Document.new(v.doc.to_s.strip).to_html + opts_html.call(v.arg_info)
  docs << "  autocomplete->setDoc(\":#{k}\", #{qutf8_doc.call(synth_doc)});\n\n"
end

docs << "  // opt headings (the opt name) + default/slidable + full docstrings\n"
opt_summaries.each do |ak, info|
  # HTML, like the synth/fx docs, for consistent block spacing.
  meta = []
  ds = fmt_default.call(info[:default])
  meta << "Default: <code>#{ds}</code>" if ds
  meta << "<i>slidable</i>" if info[:slidable]
  body = ""
  body += "<p>#{meta.join(' · ')}</p>" unless meta.empty?
  d = info[:doc].to_s.strip
  body += "<p>#{opt_doc_html.call(d)}</p>" unless d.empty?
  docs << "  autocomplete->setSummary(\"#{ak}:\", QString::fromUtf8(\"#{ak}:\"));\n"
  docs << "  autocomplete->setDoc(\"#{ak}:\", #{qutf8_doc.call(body)});\n"
  # Bounded opt (inferred from its :type) → a value-picker slider in the GUI.
  if (r = info[:range])
    dv = info[:default].is_a?(Numeric) ? info[:default] : ((r[0] + r[1]) / 2.0)
    docs << "  autocomplete->setOptRange(\"#{ak}:\", #{r[0].to_f}, #{r[1].to_f}, #{dv.to_f});\n"
  end
end
docs << "\n"

# Chord/scale semitone offsets from the tonic, keyed by bare name, so the
# completion popup can light up a chord/scale's notes on the keyboard.
docs << "  // chord + scale intervals for the keyboard preview\n"
chord_seen = {}
SonicPi::Chord::CHORD_LOOKUP.keys.each do |k|
  name = k.to_s
  next if chord_seen[name]
  chord_seen[name] = true
  offs = (SonicPi::Chord.new(0, k).to_a rescue nil)
  next unless offs && !offs.empty?
  # Round to nearest semitone — a few scales/chords are microtonal; the keyboard preview is 12-TET.
  docs << "  autocomplete->setChordIntervals(\"#{name}\", QList<int>{#{offs.map { |o| o.round }.join(',')}});\n"
end
scale_seen = {}
SonicPi::Scale::SCALE.keys.each do |k|
  name = k.to_s
  next if scale_seen[name]
  scale_seen[name] = true
  offs = (SonicPi::Scale.new(0, k).to_a rescue nil)
  next unless offs && !offs.empty?
  docs << "  autocomplete->setScaleIntervals(\"#{name}\", QList<int>{#{offs.map { |o| o.round }.join(',')}});\n"
end
docs << "\n"

# Function summaries + full docstrings for the completion detail pane.
docs << "  // function summaries + docstrings\n"
fn_info = {}
[SonicPi::Lang::Core, SonicPi::Lang::Sound].each do |mod|
  next unless mod.respond_to?(:docs)
  mod.docs.each do |name, info|
    next if info[:hide]
    fn_info[name.to_s] ||= info
  end
end
fn_info.each do |name, info|
  esc = name.gsub(/[\\"]/) { |m| "\\" + m }
  s = (info[:summary] || info[:name]).to_s
  docs << "  autocomplete->setSummary(\"#{esc}\", QString::fromUtf8(\"#{summary_clean.call(s)}\"));\n" unless s.empty?
  d = info[:doc].to_s.strip
  # HTML, like the synth/opt docs, for consistent block spacing.
  docs << "  autocomplete->setDoc(\"#{esc}\", #{qutf8_doc.call(Kramdown::Document.new(d).to_html)});\n" unless d.empty?
end
docs << "\n"

# play completes the opts of whichever synth use_synth selected (resolved live
# in the GUI). This list is only the fallback when that synth is unknown, so we
# use the default synth (:beep). sample opts come from the stereo sample player.
docs << "  // play opts fallback (default synth :beep)\n"
docs << "  fxtmp.clear(); fxtmp "
SonicPi::Synths::SynthInfo.get_all[:beep].arg_info.each do |ak, av|
  docs << "<< \"#{ak}:\" ";
end
docs << ";\n"
docs << "  autocomplete->setPlayArgs(fxtmp);\n\n"

# sample opts = the :stereo_player synth args PLUS the documented `sample` opts.
# The latter (beat_stretch:, pitch_stretch:, …) aren't synth args — they're munged
# into rate:/pitch: at runtime — so arg_info alone misses them.
docs << "  // sample opts (:stereo_player synth args + documented `sample` opts)\n"
docs << "  fxtmp.clear(); fxtmp "
sample_doc_opts = (SonicPi::Lang::Sound.docs[:sample][:opts] rescue {}) || {}
sample_opts = SonicPi::Synths::SynthInfo.get_all[:stereo_player].arg_info.keys
(sample_opts + sample_doc_opts.keys).uniq.each do |ak|
  docs << "<< \"#{ak}:\" ";
end
docs << ";\n"
docs << "  autocomplete->setSampleArgs(fxtmp);\n\n"

# Summaries/docs for documented sample opts not already registered from a synth/fx
# arg_info (e.g. beat_stretch, pitch_stretch), reusing the opt_summaries helpers.
sample_doc_opts.each do |ak, d|
  next if opt_summaries.key?(ak)
  d = d.to_s.strip
  docs << "  autocomplete->setSummary(\"#{ak}:\", QString::fromUtf8(\"#{ak}:\"));\n"
  docs << "  autocomplete->setDoc(\"#{ak}:\", #{qutf8_doc.call("<p>#{opt_doc_html.call(d)}</p>")});\n" unless d.empty?
end

def generate_ui_lang_names
  # Define the language list map -----
  ui_languages = @lang_names.keys
  ui_languages = ui_languages.sort_by {|l| l.downcase}
  locale_arrays = []
  locale_arrays << "std::map<QString, QString> SonicPii18n::native_language_names = {\n"

  # # Add each language
  for i in 0..(ui_languages.length()-1) do
    lang = ui_languages[i]
    locale_arrays << ",\n" if i != 0
    locale_arrays << "{\"#{lang}\", \"#{@lang_names[lang]}\"}"
  end

  # End the map
  locale_arrays << "\n};\n"

  # Write the map to lang_list.h
  content = File.readlines("#{SonicPi::Paths.qt_gui_path}/utils/lang_list.tmpl")
  lang_names_generated = content.take_while { |line| !line.start_with?("// AUTO-GENERATED")}
  lang_names_generated << "// AUTO-GENERATED HEADER FILE\n"
  lang_names_generated << "// Do not add any code to this file\n"
  lang_names_generated << "// as it will be removed/overwritten\n"
  lang_names_generated << "\n"
  lang_names_generated << "#ifndef LANG_LIST_H\n"
  lang_names_generated << "#define LANG_LIST_H\n"
  lang_names_generated << "#include <map>\n"
  lang_names_generated << locale_arrays.join()
  lang_names_generated << "#endif\n"

  File.open("#{SonicPi::Paths.qt_gui_path}/utils/lang_list.h", 'w') do |f|
    f << lang_names_generated.join()
  end
end


cpp = "#{SonicPi::Paths.qt_gui_utils_path}/ruby_help.h"
content = File.readlines(cpp)
new_content = content.take_while { |line| !line.start_with?("// AUTO-GENERATED-DOCS")}
new_content << "// AUTO-GENERATED-DOCS\n"
new_content << "// Do not manually add any code below this comment\n"
new_content << "// otherwise it may be removed\n"
new_content << "\n"
new_content << "void MainWindow::initDocsWindow() {\n"
new_content += docs
new_content << "}\n"

File.open(cpp, 'w') do |f|
  f << new_content.join
end

File.open("#{SonicPi::Paths.qt_gui_path}/help_files.qrc", 'w') do |f|
  f << "<RCC>\n  <qresource prefix=\"/\">\n"
  f << filenames.map{|n| "    <file>#{n}</file>\n"}.join
  f << "  </qresource>\n</RCC>\n"
end


###
# Generate info pages
###

info_sources = ["CHANGELOG.md", "CONTRIBUTORS.md", "COMMUNITY.md", "CORETEAM.html", "LICENSE.md"]
outputdir = File.absolute_path("#{SonicPi::Paths.qt_gui_path}/info")

info_sources.each do |src|

  input_path = File.absolute_path("#{SonicPi::Paths.root_path}/#{src}")
  base = File.basename(input_path)
  m = base.match /(.*)\.(.*)/
  bn = m[1]
  ext = m[2]

  input = IO.read(input_path, :encoding => 'utf-8')
  if ext == "md"
    html = SonicPi::MarkdownConverter.convert(input)
  else
    html = SonicPi::MarkdownConverter.massage!(input)
  end

  output_path = "#{outputdir}/#{bn}.html"

  File.open(output_path, 'w') do |f|
    f << html
  end

end

generate_ui_lang_names()
