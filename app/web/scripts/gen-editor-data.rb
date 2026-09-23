#!/usr/bin/env ruby
# SPDX-License-Identifier: AGPL-3.0-or-later
# Writes web/data/: what the editor knows about Sonic Pi, generated from the
# oracle's own doc system (Sonic Pi v5.0.0), in the shapes native Sonic Pi's
# GUI uses. Modelled on native's app/server/ruby/bin/qt-doc.rb, which emits
# the same facts as C++ tables and JSON; here they are JSON only.
#
#   ruby scripts/gen-editor-data.rb              # English tutorial
#   ruby scripts/gen-editor-data.rb --all-langs  # every translated tutorial
#
# Writes:
#   reference/lang.json      functions: summary, usage, doc, examples
#   reference/synths.json    synths: doc, opts with default, doc, range, slidable
#   reference/fx.json        the same for FX
#   reference/samples.json   built-in sample groups
#   reference/examples.json  the bundled example programs
#   completion.json          what autocomplete offers, and its detail pane
#   tutorial/<lang>/         chapters as typed blocks, and an index
require "json"
require "fileutils"
require "ripper"

ROOT = File.expand_path("..", __dir__)
SP_ROOT = File.join(ROOT, "../..")
SP = File.join(SP_ROOT, "app/server/ruby")
OUT = File.join(ROOT, "web/data")

require File.join(SP, "core.rb")
require File.join(SP, "paths")
%w[synths/synthinfo util runtime lang/core lang/sound lang/midi lang/western_theory note chord scale markdown_converter].each do |f|
  require File.join(SP, "lib/sonicpi", f)
end
require "kramdown"

# the web's own docstrings where it differs from native (scripts/web-docs.rb), over native's before anything is written
require File.join(__dir__, "web-docs")
WEB_DOCS.each do |k, v|
  v = v.dup
  note = v.delete(:doc_note)   # a paragraph for the web ahead of native's own doc, which otherwise stands
  d = SonicPi::Lang::Core.docs[k] = (SonicPi::Lang::Core.docs[k] || {}).merge(v)
  d[:doc] = "#{note}\n\n#{d[:doc]}" if note
end

all_langs = ARGV.include?("--all-langs")
FileUtils.mkdir_p OUT

# What this generator put here last time, from the manifest it leaves in SOURCE.json. web/data also holds files
# that are nobody's output — the app's icons, the piano's wavetable, native's toolbar images — so the directory
# cannot simply be emptied and rebuilt: only what was generated before and is not generated now is taken away.
previous = begin
  JSON.parse(File.read(File.join(OUT, "SOURCE.json")))["files"] || []
rescue StandardError
  []
end
written = []

write = lambda do |rel, obj|
  path = File.join(OUT, rel)
  FileUtils.mkdir_p File.dirname(path)
  File.write(path, JSON.generate(obj) + "\n")
  written << rel
end

md_html = ->(s) { Kramdown::Document.new(s.to_s.strip).to_html.strip }

# One-line summary for the completion popup (qt-doc's summary_clean, minus
# the C++ escaping).
summary_clean = ->(s) { s.to_s.gsub(/\s+/, " ").strip[0, 90] }

# ── From qt-doc.rb: runnable snippets ─────────────────────────────────────

# A snippet is runnable if Ripper parses it and it isn't a comment-only,
# absolute-path, printed-result (#=>) or bare-expression demo.
code_runnable = lambda do |src|
  s = src.to_s
  lines = s.lines.map(&:strip)
  return false unless lines.any? { |l| !l.empty? && !l.start_with?("#") }
  return false if s.include?("#=>")
  return false if s.include?("\"/")
  return false if (("A".."Z").to_a + ("a".."z").to_a).any? { |d| s.include?("\"#{d}:/") }
  first = lines.find { |l| !l.empty? }
  return false if first.start_with?("(") || first.start_with?("[")
  begin
    !Ripper.sexp(s).nil?
  rescue StandardError
    false
  end
end

# ── From qt-doc.rb: opts, defaults, ranges ────────────────────────────────

fmt_default = lambda do |d|
  if d.is_a?(Numeric) || d == true || d == false
    d.to_s
  elsif d.is_a?(Symbol)
    d.inspect
  elsif d.is_a?(String) && !d.empty?
    d
  end
end

opt_doc_html = lambda do |s|
  s.to_s.gsub("&", "&amp;").gsub("<", "&lt;").gsub(">", "&gt;").gsub(/`([^`]+)`/, '<code>\1</code>')
end

opts_html = lambda do |arg_info|
  return "" if arg_info.empty?
  cells = arg_info.map do |ak, info|
    ds = fmt_default.call(info[:default])
    "<td><a href=\"##{ak}\"><code>#{ak}:</code></a></td><td>#{ds ? opt_doc_html.call(ds) : ''}</td>"
  end
  rows = cells.each_slice(2).map { |s| "<tr>#{s.join}</tr>" }.join
  table = "<table cellspacing=\"0\" cellpadding=\"4\">#{rows}</table>"
  blocks = arg_info.map do |ak, info|
    head = "<a name=\"#{ak}\"></a><b><code>#{ak}:</code></b>"
    head += " <i>(slidable)</i>" if info[:slidable]
    body = info[:doc].to_s.strip
    body.empty? ? "<p>#{head}</p>" : "<p>#{head}</p><p>#{opt_doc_html.call(body)}</p>"
  end.join("<hr/>")
  "<p><b>Opts</b></p>#{table}<p>&nbsp;</p>#{blocks}"
end

derive_opt_range = lambda do |ak, info, prefer_bounds|
  midi_ranges = { "cutoff" => [30.0, 130.0, 100.0] }
  bounds = info[:bounds] || {}
  from_bounds = lambda do
    if bounds.key?(:min) && bounds.key?(:max)
      lo, hi = bounds[:min].to_f, bounds[:max].to_f
      dv = info[:default].is_a?(Numeric) ? [[info[:default].to_f, lo].max, hi].min : (lo + hi) / 2.0
      [lo, hi, dv, bounds[:min_incl] == false, bounds[:max_incl] == false]
    elsif bounds.key?(:min) && info[:default].is_a?(Numeric)
      lo, dv = bounds[:min].to_f, info[:default].to_f
      hi = dv > lo ? lo + (dv - lo) * 4.0 : lo + 1.0
      [lo, hi, dv, bounds[:min_incl] == false, false]
    elsif bounds.key?(:max) && info[:default].is_a?(Numeric)
      hi, dv = bounds[:max].to_f, info[:default].to_f
      lo = dv < hi ? hi - (hi - dv) * 4.0 : hi - 1.0
      [lo, hi, dv, false, bounds[:max_incl] == false]
    end
  end
  if prefer_bounds && (early = from_bounds.call)
    early
  elsif (mr = midi_ranges[ak.to_s]) && !info[:range]
    [mr[0], mr[1], mr[2], false, false]
  elsif info[:midi] && !info[:range] && !(bounds.key?(:min) && bounds.key?(:max))
    dv = info[:default].is_a?(Numeric) ? [[info[:default].to_f, 30.0].max, 130.0].min : 100.0
    [30.0, 130.0, dv, false, false]
  elsif (r = info[:range])
    dv = info[:default].is_a?(Numeric) ? info[:default].to_f : ((r[0] + r[1]) / 2.0)
    [r[0].to_f, r[1].to_f, dv, false, false]
  else
    from_bounds.call
  end
end

range_violation = lambda do |ak, range, validations|
  lo, hi, dv, lo_x, hi_x = range
  grid = 10.0**(Math.log10(hi - lo).floor - 2)
  lo_eff = lo_x ? lo + grid : lo
  hi_eff = hi_x ? hi - grid : hi
  validations.each do |fn, msg, _meta|
    [lo_eff, hi_eff, dv].each do |edge|
      ok = begin
        fn.call({ ak.to_sym => edge })
      rescue StandardError
        true
      end
      return [edge, msg] unless ok
    end
  end
  nil
end

opt_doc_body = lambda do |info|
  meta = []
  ds = fmt_default.call(info[:default])
  meta << "Default: <code>#{ds}</code>" if ds
  meta << "<i>slidable</i>" if info[:slidable]
  body = ""
  body += "<p>#{meta.join(' · ')}</p>" unless meta.empty?
  d = info[:doc].to_s.strip
  body += "<p>#{opt_doc_html.call(d)}</p>" unless d.empty?
  cons = info[:constraints] || []
  body += "<p><i>#{opt_doc_html.call(cons.join('; '))}</i></p>" unless cons.empty?
  body
end

opt_min_max = lambda do |ak, info, default|
  bounds = info[:bounds] || {}
  return nil if bounds[:options]
  midi_ranges = { "cutoff" => [30.0, 130.0] }
  if (mr = midi_ranges[ak.to_s]) && !info[:range]
    lo, hi = mr
  elsif info[:midi] && !info[:range] && !(bounds.key?(:min) && bounds.key?(:max))
    lo, hi = 30.0, 130.0
  elsif (r = info[:range])
    lo, hi = r[0].to_f, r[1].to_f
  elsif bounds.key?(:min) && bounds.key?(:max)
    lo, hi = bounds[:min].to_f, bounds[:max].to_f
  elsif bounds.key?(:min) && default.is_a?(Numeric)
    lo = bounds[:min].to_f
    dv = default.to_f
    hi = dv > lo ? lo + (dv - lo) * 4.0 : lo + 1.0
  elsif bounds.key?(:max) && default.is_a?(Numeric)
    hi = bounds[:max].to_f
    dv = default.to_f
    lo = dv < hi ? hi - (hi - dv) * 4.0 : hi - 1.0
  else
    return nil
  end
  min_excl = max_excl = false
  if bounds.key?(:min)
    bmin = bounds[:min].to_f
    lo = bmin if lo < bmin
    min_excl = (lo <= bmin) && (bounds[:min_incl] == false)
  end
  if bounds.key?(:max)
    bmax = bounds[:max].to_f
    hi = bmax if hi > bmax
    max_excl = (hi >= bmax) && (bounds[:max_incl] == false)
  end
  [lo, hi, min_excl, max_excl]
end

num = ->(n) { (n.is_a?(Float) && n == n.to_i) ? n.to_i : n }

# ── Reference ─────────────────────────────────────────────────────────────

# a synth's summary: its description's first sentence, in plain words (native keeps one text, its doc)
summary_of = ->(doc) { t = doc.to_s.gsub("`", "").gsub(/\s+/, " ").strip; t[/\A.+?[.!?](?=\s|\z)/] || t }
# what kind of value an opt takes, for a GUI to give it the right control and a reader the right words: a note (a MIDI
# number or a name, :c4), a time (in beats, scaled by the BPM), a switch (0 off, 1 on), a choice (one of its options),
# else a number
type_of = lambda do |ak, info|
  options = (info[:bounds] || {})[:options]
  next "switch" if options && options.map(&:to_f).sort == [0.0, 1.0]
  next "choice" if options
  next "note" if ak.to_s == "note" || ak.to_s.end_with?("_note")
  next "time" if info[:bpm_scale]
  "number"
end

instrument_pages = lambda do |klass|
  pages = []
  SonicPi::Synths::SynthInfo.get_all.each do |k, v|
    next unless v.is_a? klass
    next if (klass == SonicPi::Synths::FXInfo) && k.to_s.include?("replace_")
    next if v.is_a? SonicPi::Synths::StudioInfo
    key = (klass == SonicPi::Synths::FXInfo) ? k.to_s[3..-1] : k.to_s
    defaults = v.arg_defaults
    opts = v.arg_info.map do |ak, info|
      d = defaults[ak]
      o = { "name" => ak.to_s }
      o["default"] = d.is_a?(Numeric) ? num.call(d) : (fmt_default.call(d) || d.to_s)
      o["doc"] = info[:doc].to_s
      if d.is_a?(Numeric) && (mm = opt_min_max.call(ak, info, d))
        o["min"] = num.call(mm[0])
        o["max"] = num.call(mm[1])
        o["min_excl"] = true if mm[2]
        o["max_excl"] = true if mm[3]
      end
      if (options = (info[:bounds] || {})[:options])
        o["options"] = options.map { |x| num.call(x) }
      end
      o["slidable"] = !!info[:slidable]
      o["type"] = type_of.call(ak, info)
      o
    end
    page = { "key" => key, "title" => v.name, "summary" => summary_of.call(v.doc), "doc_html" => md_html.call(v.doc), "opts" => opts }
    page["gui"] = { "basic" => v.gui_basic.map(&:to_s) } if v.respond_to?(:gui_basic)   # native's SynthInfo GUI_BASIC
    pages << page
  end
  pages.sort_by { |p| p["key"] }
end

# Every built-in synth in the format a user's synth describes itself in (the .json beside its .scsyndef;
# runtime/lib/sonic_pi/synth_meta.rb): a worked example for each, and the one shape the GUI reads for both
synth_meta = lambda do
  SonicPi::Synths::SynthInfo.get_all.filter_map do |k, v|
    next unless v.is_a?(SonicPi::Synths::SynthInfo) && !v.is_a?(SonicPi::Synths::FXInfo) && !v.is_a?(SonicPi::Synths::StudioInfo)
    defaults = v.arg_defaults
    opts = {}
    v.arg_info.each do |ak, info|
      next if ak.to_s.end_with?("_slide", "_slide_shape", "_slide_curve")
      d = defaults[ak]
      o = { "default" => d.is_a?(Numeric) ? num.call(d) : d.to_s.sub(/\A:/, "") }
      if d.is_a?(Numeric) && (mm = opt_min_max.call(ak, info, d))
        o["range"] = [num.call(mm[0]), num.call(mm[1])]
      end
      if (options = (info[:bounds] || {})[:options])
        o["options"] = options.map { |x| num.call(x) }
      end
      o["slidable"] = true if info[:slidable]
      o["bpm_scale"] = true if info[:bpm_scale]
      o["type"] = type_of.call(ak, info)
      o["doc"] = info[:doc].to_s
      opts[ak.to_s] = o
    end
    { "name" => "sonic-pi-#{k}", "title" => v.name, "summary" => summary_of.call(v.doc), "description" => v.doc.to_s, "opts" => opts, "gui" => { "basic" => v.gui_basic.map(&:to_s) } }
  end
end

synth_pages = instrument_pages.call(SonicPi::Synths::SynthInfo)
fx_pages = instrument_pages.call(SonicPi::Synths::FXInfo)
write.call("reference/synths.json", { "pages" => synth_pages })
write.call("reference/fx.json", { "pages" => fx_pages })
write.call("synth-meta.json", { "format" => "sonic-pi-synth-metadata/1", "synths" => synth_meta.call })

sample_groups = SonicPi::Synths::SynthInfo.grouped_samples.map do |_, v|
  { "title" => v[:desc].to_s, "samples" => v[:samples].map(&:to_s) }
end
write.call("reference/samples.json", { "groups" => sample_groups })

lang_pages = []
SonicPi::Lang::Core.docs.each do |k, v|
  next if v[:hide]
  summary = (v[:summary] || v[:name]).to_s.dup
  summary[0] = summary[0].capitalize unless summary.empty?
  usage_args = (v[:args] || []).map { |arg| n, t = *arg; "#{n} (#{t})" }
  usage = v[:name].to_s
  usage = "#{usage} #{usage_args.join(', ')}" unless usage_args.empty?
  lang_pages << {
    "key" => k.to_s,
    "summary" => summary,
    "usage" => usage,
    "doc_html" => md_html.call(v[:doc]),
    "introduced" => v[:introduced].to_s,
    "examples" => (v[:examples] || []).map { |e| code = e.to_s.strip; { "code" => code, "runnable" => code_runnable.call(code) } },
  }
end
lang_pages.sort_by! { |p| p["key"] }
write.call("reference/lang.json", { "pages" => lang_pages })
# the runtime's share: the fns that make a ring, whose names native's preparser keeps from being variables
# (runtime/lib/sonic_pi/preparser.rb)
File.write(File.join(ROOT, "runtime/data/lang.rb"), "# Generated by scripts/gen-editor-data.rb from Sonic Pi's docs. Do not edit.\nmodule SonicPi; module Data\nVEC_FNS = #{SonicPi::Lang::Core.vec_fns.map { |f| f[:name].to_s }.sort.inspect}\nend; end\n")

example_groups = %w[Apprentice Illusionist Magician Sorcerer Wizard Algomancer].map do |dir|
  paths = Dir["#{SonicPi::Paths.examples_path}/#{dir.downcase}/*.rb"].sort
  examples = paths.map do |path|
    code = File.read(path, encoding: "utf-8")
    # use_debug false quiets the log's line for every sound; an example that prints nothing has nothing it hides,
    # so the line only asks a reader to wonder what it is for. Kept where the example puts or prints.
    code = code.sub(/^use_debug false\n(\n(?=\n))?/, "") unless code =~ /\b(puts|print)\b/
    key = File.basename(path, ".rb")
    { "key" => key, "title" => key.split("_").map(&:capitalize).join(" "), "code" => code, "runnable" => code_runnable.call(code) }
  end
  { "title" => dir, "examples" => examples }
end.reject { |g| g["examples"].empty? }
write.call("reference/examples.json", { "groups" => example_groups })

# ── Tutorial ──────────────────────────────────────────────────────────────

render_elements = lambda do |els|
  root = Kramdown::Element.new(:root, nil, nil, encoding: "UTF-8", location: 1, options: {}, abbrev_defs: {}, abbrev_attr: {})
  root.children = els
  Kramdown::Converter::Html.convert(root)[0].to_s
end

image_path = lambda do |src|
  if src.include?("etc/doc/images/") then src.split("etc/doc/images/").last
  elsif src.include?("images/") then src.split("images/").last
  else src
  end
end

only_img = lambda do |el|
  imgs = el.children.select { |c| c.type == :img }
  return nil unless imgs.length == 1
  rest = el.children - imgs
  return nil unless rest.all? { |c| c.type == :text && c.value.to_s.strip.empty? }
  imgs.first
end

li_html = lambda do |li|
  html = render_elements.call(li.children).strip
  if li.children.length == 1 && li.children[0].type == :p && html.start_with?("<p>") && html.end_with?("</p>")
    html = html[3...-4].strip
  end
  html
end

tutorial_blocks = lambda do |markdown|
  md = markdown.to_s.gsub(/\`\`\`\`*/, "~~~~")
  blocks = []
  Kramdown::Document.new(md).root.children.each do |el|
    case el.type
    when :blank
      next
    when :header
      blocks << { "type" => "heading", "level" => el.options[:level], "text" => el.options[:raw_text].to_s.strip }
    when :codeblock
      source = el.value.to_s
      source = source[0...-1] while source.end_with?("\n")
      blocks << { "type" => "code", "source" => source, "runnable" => code_runnable.call(source) }
    when :ul, :ol
      items = el.children.select { |c| c.type == :li }.map { |li| li_html.call(li) }
      blocks << { "type" => "list", "ordered" => el.type == :ol, "items" => items }
    when :p
      if (img = only_img.call(el))
        blocks << { "type" => "image", "path" => image_path.call(img.attr["src"].to_s), "alt" => img.attr["alt"].to_s }
      else
        html = render_elements.call([el]).strip
        blocks << { "type" => "prose", "html" => html } unless html.empty?
      end
    else
      html = render_elements.call([el]).strip
      blocks << { "type" => "prose", "html" => html } unless html.empty?
    end
  end
  blocks
end

tutorial_langs = ["en"]
if all_langs
  tutorial_langs += Dir["#{SonicPi::Paths.docs_generated_path}/*/tutorial"].map { |p| File.basename(File.dirname(p)) }.reject { |l| l == "native" }.sort
end
tutorial_langs.each do |lang|
  src_dir = lang == "en" ? SonicPi::Paths.tutorial_path : File.expand_path("../generated/#{lang}/tutorial", SonicPi::Paths.tutorial_path)
  chapters = []
  Dir["#{src_dir}/*.md"].sort.each do |path|
    lines = File.read(path, encoding: "utf-8").lines
    title = lines.first.to_s.strip
    body = lines.length > 1 ? lines[1..].join : ""
    key = File.basename(path, ".md")
    write.call("tutorial/#{lang}/#{key}.json", { "title" => title, "blocks" => tutorial_blocks.call(body) })
    chapters << { "key" => key, "title" => title }
  end
  write.call("tutorial/#{lang}/index.json", { "chapters" => chapters })
end

# ── Quickstart cards ──────────────────────────────────────────────────────
#
# etc/quickstart/cards.txt as native's quickstart pane reads it: decks, each
# a description and cards; a card is a title, a line or two, and a snippet
# between fences.

decks = []
cards_path = File.join(SP_ROOT, "etc/quickstart/cards.txt")
if File.exist?(cards_path)
  deck = nil
  card = nil
  in_code = false
  File.read(cards_path, encoding: "utf-8").each_line do |raw|
    line = raw.chomp
    if in_code
      if line.strip.start_with?("```")
        in_code = false
      else
        card["code"] << line << "\n"
      end
      next
    end
    if line.start_with?("# Deck:")
      deck = { "title" => line.sub("# Deck:", "").strip, "description" => "", "cards" => [] }
      decks << deck
      card = nil
    elsif line.start_with?("## ") && deck
      card = { "title" => line[3..].strip, "blurb" => "", "code" => +"" }
      deck["cards"] << card
    elsif line.strip.start_with?("```") && card
      in_code = true
    elsif line.start_with?("#")
      next
    elsif !line.strip.empty?
      if card
        card["blurb"] = [card["blurb"], line.strip].reject(&:empty?).join(" ")
      elsif deck
        deck["description"] = [deck["description"], line.strip].reject(&:empty?).join(" ")
      end
    end
  end
  decks.each { |d| d["cards"].each { |c| c["code"] = c["code"].chomp; c["runnable"] = code_runnable.call(c["code"]) } }
end
write.call("reference/quickstart.json", { "decks" => decks })

images_src = File.join(SP_ROOT, "etc/doc/images")
if Dir.exist?(images_src)
  FileUtils.rm_rf File.join(OUT, "images")   # all of it is native's, so it goes and comes back whole
  # the pictures alone: what they were drawn from (a .graffle, a .ly and its .pdf, a credits note) is native's to keep
  Dir[File.join(images_src, "**/*.{png,jpg,jpeg,gif,svg,webp}")].each do |f|
    to = File.join(OUT, "images", f.sub(images_src + "/", ""))
    FileUtils.mkdir_p File.dirname(to)
    FileUtils.cp f, to
    written << to.sub(OUT + "/", "")
  end
end

# ── Completion ────────────────────────────────────────────────────────────
#
# qt-doc.rb registers these with the native editor one call at a time
# (addSynthArgs, setSummary, setDoc, setUsage, setOptRange, setOptOptions,
# setChordIntervals ...) and writes the arg-kind, fn-opt and opt-owner tables
# as C++. Here they are one document.

entries = Hash.new { |h, k| h[k] = {} }   # name → { summary, usage, doc }
synths = {}                               # ":prophet" → ["note:", ...]
fx = {}                                   # ":reverb"  → ["room:", ...]
opt_summaries = {}
opt_validations = {}
opt_owners = {}

SonicPi::Synths::SynthInfo.get_all.each do |k, v|
  next unless v.is_a? SonicPi::Synths::FXInfo
  next if k.to_s.include?("replace_")
  name = ":#{k.to_s[3..-1]}"
  fx[name] = v.arg_info.keys.map { |ak| "#{ak}:" }
  v.arg_info.each do |ak, av|
    opt_summaries[ak] ||= av if av[:doc]
    (opt_owners[ak] ||= []) << [name, av]
    vals = ((v.info[ak] || {})[:validations] rescue nil)
    (opt_validations[ak] ||= []) << [name, av, vals] if vals
  end
  entries[name][:summary] = summary_clean.call(v.name)
  entries[name][:doc] = md_html.call(v.doc) + opts_html.call(v.arg_info)
end

SonicPi::Synths::SynthInfo.get_all.each do |k, v|
  next unless v.is_a? SonicPi::Synths::SynthInfo
  next if v.is_a? SonicPi::Synths::FXInfo
  name = ":#{k}"
  synths[name] = v.arg_info.keys.map { |ak| "#{ak}:" }
  v.arg_info.each do |ak, av|
    opt_summaries[ak] ||= av if av[:doc]
    (opt_owners[ak] ||= []) << [name, av]
    vals = ((v.info[ak] || {})[:validations] rescue nil)
    (opt_validations[ak] ||= []) << [name, av, vals] if vals
  end
  entries[name][:summary] = summary_clean.call(v.name)
  entries[name][:doc] = md_html.call(v.doc) + opts_html.call(v.arg_info)
end

opt_ranges = {}
opt_options = {}
owner_docs = []
owner_options = []
owner_ranges = []
range_problems = []

opt_summaries.each do |ak, info|
  key = "#{ak}:"
  body = opt_doc_body.call(info)
  entries[key][:summary] = key
  entries[key][:doc] = body
  (opt_owners[ak] || []).each do |owner, oinfo|
    own = opt_doc_body.call(oinfo)
    owner_docs << { "owner" => owner, "opt" => key, "doc" => own } unless own == body
  end
  bounds = info[:bounds] || {}
  if (options = bounds[:options])
    opt_options[key] = options.map(&:to_s)
    (opt_owners[ak] || []).each do |owner, oinfo|
      own_opts = (oinfo[:bounds] || {})[:options]
      owner_options << { "owner" => owner, "opt" => key, "options" => own_opts.map(&:to_s) } if own_opts && own_opts != options
    end
  elsif (range = derive_opt_range.call(ak, info, false))
    opt_ranges[key] = range
    (opt_validations[ak] || []).each do |owner, oinfo, vals|
      next unless range_violation.call(ak, range, vals)
      own = derive_opt_range.call(ak, oinfo, true)
      own_bad = own.nil? ? [nil, "no derivable range"] : range_violation.call(ak, own, vals)
      if own_bad
        # native aborts its docs build here; report it and offer no range
        range_problems << "#{key} on #{owner}: #{own_bad[1]}"
        next
      end
      owner_ranges << { "owner" => owner, "opt" => key, "range" => own }
    end
  end
end

chord_intervals = {}
SonicPi::Chord::CHORD_LOOKUP.keys.each do |k|
  next if chord_intervals.key?(k.to_s)
  offs = (SonicPi::Chord.new(0, k).to_a rescue nil)
  chord_intervals[k.to_s] = offs.map(&:round) if offs && !offs.empty?
end
scale_intervals = {}
SonicPi::Scale::SCALE.keys.each do |k|
  next if scale_intervals.key?(k.to_s)
  offs = (SonicPi::Scale.new(0, k).to_a rescue nil)
  scale_intervals[k.to_s] = offs.map(&:round) if offs && !offs.empty?
end

fn_info = {}
[SonicPi::Lang::Core, SonicPi::Lang::Sound].each do |mod|
  next unless mod.respond_to?(:docs)
  mod.docs.each do |name, info|
    next if info[:hide]
    fn_info[name.to_s] ||= info
  end
end
fn_info.each do |name, info|
  s = (info[:summary] || info[:name]).to_s
  entries[name][:summary] = summary_clean.call(s) unless s.empty?
  usage = info[:usage_example].to_s.strip
  usage = "#{name} #{info[:args].map { |a| a[0] }.join(', ')}" if usage.empty? && info[:args] && !info[:args].empty?
  entries[name][:usage] = usage unless usage.empty?
  d = info[:doc].to_s.strip
  entries[name][:doc] = md_html.call(d) unless d.empty?
end

play_args = SonicPi::Synths::SynthInfo.get_all[:beep].arg_info.keys.map { |ak| "#{ak}:" }
sample_doc_opts = (SonicPi::Lang::Sound.docs[:sample][:opts] rescue {}) || {}
sample_args = (SonicPi::Synths::SynthInfo.get_all[:stereo_player].arg_info.keys + sample_doc_opts.keys).uniq.map { |ak| "#{ak}:" }

sample_doc_opts.each do |ak, d|
  next if opt_summaries.key?(ak)
  key = "#{ak}:"
  entries[key][:summary] = key
  d = d.to_s.strip
  entries[key][:doc] = "<p>#{opt_doc_html.call(d)}</p>" unless d.empty?
end

lang_opt_docs = {}
fn_info.each_value do |info|
  opts = info[:opts]
  next unless opts.is_a?(Hash)
  next if opts.key?(:your_key)
  opts.each { |ak, d| lang_opt_docs[ak] ||= d }
end
lang_opt_docs.each do |ak, d|
  next if opt_summaries.key?(ak) || sample_doc_opts.key?(ak)
  key = "#{ak}:"
  entries[key][:summary] = key
  d = d.to_s.strip
  entries[key][:doc] = "<p>#{opt_doc_html.call(d)}</p>" unless d.empty?
end

SonicPi::Synths::SynthInfo.grouped_samples.each do |_, v|
  desc = v[:desc].to_s
  v[:samples].each do |s|
    name = ":#{s}"
    entries[name][:summary] = desc
    entries[name][:usage] = "sample #{name}"
    entries[name][:doc] = "<p>One of the built-in <b>#{desc}</b> samples.</p>"
  end
end

argkind_names = { nil => "None", sample: "Sample", cue: "CuePath", fx: "Fx", synth: "Synth", note: "Note",
                  scale: "Scale", chord: "Chord", link_peer: "LinkAudioPeer", link_channel: "LinkAudioChannel", track: "Track" }
arg_kinds = {}
fn_opts = {}
mods = [SonicPi::Lang::Core, SonicPi::Lang::Sound]
mods << SonicPi::Lang::WesternTheory if defined?(SonicPi::Lang::WesternTheory)
mods.each do |mod|
  next unless mod.respond_to?(:docs)
  mod.docs.each do |name, info|
    ak = info[:arg_kinds]
    arg_kinds[name.to_s] ||= ak.map { |k| argkind_names[k] || "None" } if ak && !ak.empty?
    next if info[:hide]
    opts = info[:opts]
    next unless opts.is_a?(Hash) && !opts.empty?
    next if opts.key?(:your_key)
    fn_opts[name.to_s] ||= opts.keys.map { |k| "#{k}:" }
  end
end

write.call("completion.json", {
  "synths" => synths, "fx" => fx,
  "playArgs" => play_args, "sampleArgs" => sample_args,
  "entries" => entries.transform_values { |h| h.transform_keys(&:to_s) },
  "optRanges" => opt_ranges, "optOptions" => opt_options,
  "optOwners" => { "docs" => owner_docs, "options" => owner_options, "ranges" => owner_ranges },
  "chordIntervals" => chord_intervals, "scaleIntervals" => scale_intervals,
  "argKinds" => arg_kinds.sort.to_h, "fnOpts" => fn_opts.sort.to_h,
})

# ── What was made ─────────────────────────────────────────────────────────

version = File.read(File.join(ROOT, "../../VERSION")).strip   # the release's (VERSION), not git's: the same on every commit and in a checkout with no tags
# last, and with the manifest the next run reads: everything written above, and what is no longer written is gone
(previous - written).each do |rel|
  path = File.join(OUT, rel)
  File.delete(path) if File.file?(path)
end
Dir[File.join(OUT, "**/*")].select { |d| File.directory?(d) }.sort.reverse_each { |d| Dir.rmdir(d) if Dir.empty?(d) }
File.write(File.join(OUT, "SOURCE.json"),
           JSON.generate({ "sonic_pi" => version, "generator" => "scripts/gen-editor-data.rb",
                           "files" => written.sort }) + "\n")
puts "Sonic Pi #{version}: #{lang_pages.size} functions, #{synth_pages.size} synths, #{fx_pages.size} fx, " \
     "#{sample_groups.sum { |g| g['samples'].size }} samples, #{example_groups.sum { |g| g['examples'].size }} examples"
puts "completion: #{entries.size} entries, #{opt_ranges.size} opt ranges, #{opt_options.size} enum opts, " \
     "#{owner_ranges.size} owner ranges, #{arg_kinds.size} fns with arg kinds, #{fn_opts.size} fns with opts"
puts "tutorial: #{tutorial_langs.join(', ')}; quickstart: #{decks.size} decks, #{decks.sum { |d| d['cards'].size }} cards"
unless range_problems.empty?
  puts "ranges native would refuse to ship (#{range_problems.size}):"
  range_problems.each { |p| puts "  #{p}" }
end
