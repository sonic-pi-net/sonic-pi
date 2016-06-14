## Rakefile for Sonic Pi

# rake is tool similar to make, but uses ruby.
#
# The Rakefile is firstly a description of all file and task
# dependencies of the project. The prerequisites of each file
# or task are collected with the "=>" operator.
#
# Secondly, the Rakefile describes the steps needed to fulfill a task
# or create a file, written in ruby code. This code is only executed
# when needed, e.g. it will only convert a file if the target file does
# not exist yet or if the source file is more recent than the target.
#
# So when reading this Rakefile, read it in these two steps: First,
# rake will extract all dependencies, second, it executes all the
# code needed to fulfill a requested task.
#
# Sonic Pi's dependencies in this Rakefile look fairly complicated,
# but with this guide you should easily navigate this file:
#
# - The Qt GUI app needs a number of resource files, which will be
#   generated at build time in put in RESOURCES_PATH.
#
# - The images in IMAGES_PATH will be copied to RESOURCES_PATH.
#
# - Some documentation text files and the English tutorial will be
#   converted from Markdown to HTML and put in RESOURCES_PATH.
#
# - The English tutorial Markdown will be parsed and each element of
#   the document translated with the MarkdownPOHelper class, using the
#   existing .po gettext files found in TUTORIAL_LANG_PATH. The
#   resulting translated Markdown is then converted to HTML and put
#   in RESOURCES_PATH
#
# - The HTML documentation for the Sonic Pi reference manual
#   sections Synths, FX, Samples, Lang will be retrieved from the
#   server's ruby libraries and put in  RESOURCES_PATH.
#
# - The HELP_HEADER_FILE will be created. It is a generated C++
#   file that is used to setup the Sonic Pi GUI's help system.
#
# - The .qrc Qt resource lists will be created, which are needed
#   by the Qt build tools to link the resources with the Sonic Pi
#   GUI binary.
#
# - The QT_MAKEFILE will be generated from the QT_PRO_FILE.
#
# - And finally, the QT_GUI_BINARY will be compiled and linked.
#
# But that's not all. There are also
#
# - The "clobber" and several cleanup tasks to remove temporary files
#   and compiled binaries after the build
#
# - The "test_server" task which runs the Sonic Pi server test suite.
#
# - The "update_language_files" task, which is used during development
#   and described in TRANSLATION-WORKFLOW.md.


## ----------- TASK DESCRIPTIONS -----------


desc "Compile native libraries for gems in vendor/ directory"
task :compile_gems

desc "Remove temporary compiled object files after compile_gems"
task :clean_gems

desc "Update language files with current English strings from Qt GUI sources and tutorial Markdown (read TRANSLATION-WORKFLOW.md for info)" 
task :update_language_files

desc "Compile Qt GUI application (*default task*)" 
task :compile_qt_gui
task :default => [:compile_qt_gui]

desc "Remove temporary compiled object files after compile_qt_gui" 
task :clean_qt_gui

desc "Run the ruby test suite of the Sonic Pi server component"
task :test_server


## ----------- SETUP -----------


# This is our minimal set of gems required for this Rakefile...

require 'rake/clean'
require 'rbconfig'
require 'pathname'

# ...all other gems will be installed through the vendor directory.

RUBY_API = RbConfig::CONFIG['ruby_version']

OS = case RUBY_PLATFORM
when /.*arm.*-linux.*/
  :raspberry
when /.*linux.*/
  :linux
when /.*darwin.*/
  :osx
when /.*mingw.*/
  :windows
else
  RUBY_PLATFORM
end

LIB_EXT = (OS == :osx) ? 'bundle' : 'so'


## ----------- CONFIG -----------


chdir(File.absolute_path(File.dirname(__FILE__)))

APP_PATH           = "app"

SERVER_PATH        = "#{APP_PATH}/server"
VENDOR_GEM_PATH    = "#{SERVER_PATH}/vendor"
NATIVE_GEM_PATH    = "#{SERVER_PATH}/rb-native/#{OS}/#{RUBY_API}"

SERVER_TEST_PATH   = "#{SERVER_PATH}/sonicpi/test"

QT_GUI_PATH        = "#{APP_PATH}/gui/qt"
QT_LANG_PATH       = "#{QT_GUI_PATH}/lang"
HELP_HEADER_FILE   = "#{QT_GUI_PATH}/ruby_help.h"
QT_PRO_FILE        = "#{QT_GUI_PATH}/SonicPi.pro"
QT_MAKEFILE        = "#{QT_GUI_PATH}/Makefile"
QT_GUI_BINARY      = "#{QT_GUI_PATH}/sonic-pi"

ETC_PATH           = "etc"
DOC_PATH           = "#{ETC_PATH}/doc"
IMAGES_PATH        = "#{DOC_PATH}/images"
TUTORIAL_PATH      = "#{DOC_PATH}/tutorial"
TUTORIAL_LANG_PATH = "#{DOC_PATH}/lang"
EXAMPLES_PATH      = "#{ETC_PATH}/examples"

RESOURCES_PATH     = "#{QT_GUI_PATH}/resources"
INFO_QRC           = "#{RESOURCES_PATH}/info_files.qrc"
IMAGES_QRC         = "#{RESOURCES_PATH}/image_files.qrc"
I18N_QRC           = "#{RESOURCES_PATH}/i18n_files.qrc"
REFERENCE_QRC      = "#{RESOURCES_PATH}/reference_files.qrc"
TUTORIAL_QRC       = "#{RESOURCES_PATH}/tutorial_files.qrc"

EXAMPLE_SKILLS     = ["Apprentice", "Illusionist", "Magician", "Sorcerer", "Wizard", "Algomancer"]


## ----------- GEM SEARCH PATH -----------


$:.unshift NATIVE_GEM_PATH

Dir["#{VENDOR_GEM_PATH}/*"].select { |fn| File.directory?(fn) }.each do |fn|
  # did_you_mean comes pre-installed with ruby 2.3.0+, so don't use our copy of it and its dep from the vendor/ dir
  $:.unshift "#{fn}/lib" unless (Gem::Version.new(RUBY_API) >= Gem::Version.new("2.3.0") && (File.basename(fn) =~ /^did_you_mean|^interception/))
end


## ----------- TASK :compile_gems -----------


def make_clean(dir)
  Dir.chdir(dir) do
    puts "### Running make clean in #{dir}"
    if File.exist?("Makefile") then
      sh "make clean"
      rm_rf 'Makefile'
    end
  end
end


def gem_native_dependency(source_dir, target_dir, *lib_files)  
  directory target_dir
  CLOBBER << target_dir
  
  lib_files.map{ |l| "#{l}.#{LIB_EXT}" }.each do |l|
  
    task :compile_gems => ["#{target_dir}/#{l}"]
    CLOBBER << "#{target_dir}/#{l}"
    
    # Run extconf.rb to prepare the gem's native lib build.
    file "#{source_dir}/Makefile" => ["#{source_dir}/extconf.rb"] do |t|
      puts "### Preparing #{t.name} from #{t.prerequisites.first}"
      Dir.chdir(File.dirname(t.prerequisites.first)) do
        sh "#{RbConfig.ruby} extconf.rb"
      end
    end

    # Then build it.
    file "#{source_dir}/#{l}" => ["#{source_dir}/Makefile"] do |t|
      puts "### Building #{t.name}"
      Dir.chdir(File.dirname(t.prerequisites.first)) do
        sh "make"
      end
    end

    # Finally, copy the result to the native extensions directory
    file "#{target_dir}/#{l}" => ["#{source_dir}/#{l}", target_dir] do |t|
      puts "### Copying #{t.name}"
      cp t.prerequisites.first, t.name
    end

    # Also, we want to clean up afterwards.
    task :clean_gems do
      make_clean(source_dir)
    end

  end
end


gem_native_dependency("#{VENDOR_GEM_PATH}/rugged-0.24.0/ext/rugged", NATIVE_GEM_PATH, 'rugged')
gem_native_dependency("#{VENDOR_GEM_PATH}/atomic/ext", NATIVE_GEM_PATH, 'atomic_reference')
gem_native_dependency("#{VENDOR_GEM_PATH}/ffi-1.9.10/ext/ffi_c", NATIVE_GEM_PATH, 'ffi_c')
gem_native_dependency("#{VENDOR_GEM_PATH}/ruby-prof-0.15.8/ext/ruby_prof", NATIVE_GEM_PATH, 'ruby_prof')
gem_native_dependency("#{VENDOR_GEM_PATH}/fast_osc/ext/fast_osc", NATIVE_GEM_PATH, 'fast_osc')

if Gem::Version.new(RUBY_API) < Gem::Version.new("2.3.0") then
  # The did_you_mean gem is bundled with ruby 2.3.0 and above,
  # so we use the one provided in our vendor/ dir only for older versions of ruby.
  gem_native_dependency("#{VENDOR_GEM_PATH}/did_you_mean-0.10.0/ext/did_you_mean", "#{NATIVE_GEM_PATH}/did_you_mean" , 'method_receiver')
end

if Gem::Version.new(RUBY_API) < Gem::Version.new("2.0.0") then
  # The did_you_mean gem depends on the interception gem,
  # which needs a native lib when used with ruby < 2.0.0. 
  gem_native_dependency("#{VENDOR_GEM_PATH}/interception/ext", NATIVE_GEM_PATH, 'interception')
end

if OS == :osx then
  # These are needed on OS X, only.
  gem_native_dependency("#{VENDOR_GEM_PATH}/narray-0.6.1.1", NATIVE_GEM_PATH, 'narray')
  gem_native_dependency("#{VENDOR_GEM_PATH}/ruby-coreaudio-0.0.11/ext", NATIVE_GEM_PATH, 'coreaudio_ext')
end


## ----------- CHECK IF GEMS ARE INSTALLED -----------


gems_installed = false

unless (Rake.application.top_level_tasks.include?("compile_gems") || Rake.application.top_level_tasks.include?("clean_gems") ) then

  begin
  
    # Let's see if the gems we depend on are installed on this systems.
    require 'kramdown'
    require 'gettext'
    require 'gettext/po'
    require 'gettext/po_parser'
    require_relative "#{SERVER_PATH}/core.rb"
    require_relative "#{SERVER_PATH}/sonicpi/lib/sonicpi/synths/synthinfo"
    require_relative "#{SERVER_PATH}/sonicpi/lib/sonicpi/util"
    require_relative "#{SERVER_PATH}/sonicpi/lib/sonicpi/runtime"
    require_relative "#{SERVER_PATH}/sonicpi/lib/sonicpi/lang/sound"
    require_relative "#{SERVER_PATH}/sonicpi/lib/sonicpi/lang/minecraftpi"
    
    gems_installed = true
  
  rescue LoadError
  
    # If the vendor gems didn't load properly,
    # we'll tell the user to run "rake :compile_gems" first.
    
    task :needs_gems do
      puts "rake aborted, you need to compile the vendor gems first."
      puts "Please run 'rake compile_gems' now."
      abort
    end
  
    task :compile_qt_gui => [:needs_gems]
    task :test_server => [:needs_gems]
    task :update_language_files => [:needs_gems]
  
  end

end


## ----------- NOW DEFINE EVERYTHING ELSE -----------


if gems_installed then
    
  ## ----------- HELPER FUNCTIONS -----------


  def relative_path(filename, from_path)
    pf = Pathname.new(filename)
    pp = Pathname.new(from_path)
    return pf.relative_path_from(pp)
  end


  def read_file(filename)
    return File.open(filename, 'r:utf-8') { |f| f.read }
  end
  
  
  def write_file(filename, content)
    File.open(filename, 'w:utf-8') { |f| f.write(content) }
  end
  
  
  def convert_file_task(t)
    write_file(t.name, yield(read_file(t.prerequisites.first)))
  end


  # GitHub Markdown syntax uses ```` to mark code blocks,
  # but Kramdown uses ~~~~.
  # Therefore, let's fix-point on GitHub syntax, and fudge it
  # into Kramdown syntax where necessary.
  
  def massage_markdown(md)
    return md.gsub(/\`\`\`\`*/, '~~~~')
  end
  
  
  def massage_html(html)
    # Add Unicode and body tags & remove unneded newlines before </pre>
    html.gsub!(/\n+(<\/code>)?<\/pre>/, '\1</pre>')
    return "" +
      "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n" +
      "\n" +
      "<body>\n" +
      "#{html}\n" +
      "</body>\n"
  end
  
  
  def markdown_to_html(md)
    require 'kramdown'
    html = Kramdown::Document.new(massage_markdown(md)).to_html
    return massage_html(html)
  end
  
  
  def qrc_url(fn)
    return "qrc:///#{relative_path(fn, RESOURCES_PATH)}"
  end


  ## ----------- HELPER CLASSES -----------


  # Slightly alters the behaviour of ruby kramdown gem's converter.
  # TODO: send these as config options to upstream devs.
  
  class KramdownToOurMarkdown < Kramdown::Converter::Kramdown
  
    def convert_a(el, opts)
      # ruby kramdown wants to use document-wide link list footnotes,
      # but we prefer inline links instead
      if el.attr['href'].empty? then
        "[#{inner(el, opts)}]()"
      elsif el.attr['href'] =~ /^(?:http|ftp)/ || el.attr['href'].count("()") > 0
        "[#{inner(el, opts)}](#{el.attr['href']})"
      else
        title = parse_title(el.attr['title'])
        "[#{inner(el, opts)}](#{el.attr['href']}#{title})"
      end
    end
  
  end


  # This helper class parses a Markdown document using the Kramdown
  # parser, then iterates through the document's structure to extract
  # each translateable element.

  class MarkdownPOHelper
  
    def initialize(po_filename = nil)
      @po = GetText::PO.new
      if (!po_filename.nil?) then
        parser = GetText::POParser.new
        parser.ignore_fuzzy = false
        parser.report_warning = false
        parser.parse_file(po_filename, @po)
      end
    end
    
    def is_number? string
      true if Float(string) rescue false
    end
  
    def handle_entry(msgid, filename, line, flags = [])
      reference = "#{filename}" + (line ? ":#{line}" : "")
      msgid.gsub!(/\\([.:_])/, '\1')
    
      if is_number?msgid then
        return msgid
      end
    
      if @po.has_key?msgid then
        entry = @po[msgid]
      else
        entry = GetText::POEntry.new(:normal)
        entry.msgid = msgid
      end
    
      entry.flags |= flags
      entry.references << reference
    
      @po[msgid] = entry
    
      return @po[msgid].msgstr || msgid
    end
    
    def convert_element(filename, el, bullet = nil)
      case el.type
    
      when :root, :li, :ul, :ol
        t = ""
        i = 0
        while i < el.children.count do
          case el.type
          when :ul
            b = '*'
          when :ol
            b = "#{i+1}."
          else
            b = nil
          end
          t += convert_element(filename, el.children[i], b)
          i += 1
        end
        return t
    
      when :blank
        return el.value.gsub(/' '/, '')
    
      when :p
        root = Kramdown::Element.new(
          :root, nil, nil,
          :encoding => "UTF-8",
          :location => 1,
          :options => {},
          :abbrev_defs => {}, :abbrev_attr => {}
        )
        root.children = [el]
        output, warnings = KramdownToOurMarkdown.convert(root)
        output.gsub!(/\n/, ' ').strip!
    
        t = handle_entry(output, filename, el.options[:location])
        return (bullet ? bullet + " " : "") + t + "\n"
    
      when :codeblock
        t = handle_entry(el.value.gsub(/\n+$/, ""), filename, el.options[:location], ["no-wrap"])
        return "```\n" + t + "\n" + "```\n"
    
      when :header
        t = handle_entry(el.options[:raw_text].strip, filename, el.options[:location])
        return ("#" * el.options[:level]) + " " + t + "\n"
      
      when :xml_comment
        # TODO: add :location for kramdown parser and send patch upstream  
        t = handle_entry(el.value.gsub(/<!--(.*)-->/m, '\1').strip, filename, el.options[:location])
        return "<!-- " + t + " -->\n"
  
      else
        raise "Error #{filename}: Please implement conversion for unknown Kramdown element type :#{el.type} in line #{el.options[:location]}"
      end
    end
  
    def translate(markdown_filename)
      content = read_file(markdown_filename)
      k = Kramdown::Document.new(massage_markdown(content))
      return convert_element(File.basename(markdown_filename), k.root)
    end
    
    def write_po(po_filename)
      s = "" +
        "# This file is distributed under the same license as the Sonic Pi package.\n" +
        "# Do not edit this file, use Weblate instead.\n" +
        "# Read TRANSLATION.md for more information.\n" +
        "\n" +
        "msgid \"\"\n" +
        "msgstr \"\"\n" +
        "\"Project-Id-Version: Sonic Pi\\n\"\n" +
        "\"MIME-Version: 1.0\\n\"\n" +
        "\"Content-Type: text/plain; charset=UTF-8\\n\"\n" +
        "\"Content-Transfer-Encoding: 8bit\\n\n" +
        "\n" +
        @po.to_s

      write_file(po_filename, s)
    end
  end


  ## ----------- RESOURCE FILE DEPENDENCIES -----------
  
  
  # The resources/ directory contains generated files that are
  # copied or converted from other files, then a .qrc list of
  # all files is created for Qt.
  
  directory RESOURCES_PATH  
  CLEAN << RESOURCES_PATH
  
  
  # The tutorial's PNG images are copied to the resource dir for the GUI build.
  
  generated_dir = "#{RESOURCES_PATH}/images" 
  directory generated_dir
  
  FileList["#{IMAGES_PATH}/**/*.png"].sort.each do |fn|
    generated_file = "#{generated_dir}/#{relative_path(fn, IMAGES_PATH)}"
    directory File.dirname(generated_file)
    file generated_file => [fn, File.dirname(generated_file)] do |t|
      puts "### Copying #{t.name}"
      cp t.prerequisites.first, t.name
    end
    file IMAGES_QRC => generated_file
  end
  
  
  # The project root contains some Markdown and HTML files which
  # are converted to HTML and later used in the GUI's info window.
  
  generated_dir = "#{RESOURCES_PATH}/info"
  directory generated_dir
  
  ["CHANGELOG.md", "CONTRIBUTORS.md", "COMMUNITY.md", "LICENSE.md"].each do |fn|
    generated_file = "#{generated_dir}/#{File.basename(fn, '.md')}.html"
    file generated_file => [fn, generated_dir] do |t|
      puts "### Converting #{t.name}"
      convert_file_task(t) { |s| markdown_to_html(s) }
    end
    file INFO_QRC => generated_file
  end
  
  ["CORETEAM.html"].each do |fn|
    generated_file = "#{generated_dir}/#{File.basename(fn)}"
    file generated_file => [fn, generated_dir] do |t|
      puts "### Converting #{t.name}"
      convert_file_task(t) { |s| massage_html(s) }
    end
    file INFO_QRC => generated_file
  end
  
  
  # Now, we convert the English tutorial files to HTML.
  
  TUTORIAL_FILES = FileList["#{TUTORIAL_PATH}/*.md"]

  generated_dir = "#{RESOURCES_PATH}/tutorial-en"
  directory generated_dir
  
  TUTORIAL_FILES.each do |fn|
    generated_file = "#{generated_dir}/#{File.basename(fn, '.md')}.html"
    file generated_file => [fn, generated_dir] do |t|
      puts "### Converting #{t.name}"
      convert_file_task(t) { |s| markdown_to_html(s) }
    end
    file TUTORIAL_QRC => generated_file
  end
  
  
  # Then, we translate the English tutorial to all languages
  # that we have gettext .po files for.
  
  TUTORIAL_LANGUAGES =
    FileList["#{TUTORIAL_LANG_PATH}/sonic-pi-tutorial-*.po"].
    map { |n| File.basename(n).gsub(/sonic-pi-tutorial-(.*?).po/, '\1') }
  
  # To avoid reloading the PO file for each Markdown page,
  # this keeps a global cache in memory.
  mdpo = {}
  
  TUTORIAL_LANGUAGES.sort.each do |l|
    generated_dir = "#{RESOURCES_PATH}/tutorial-#{l}"
    directory generated_dir
    po_file = "#{TUTORIAL_LANG_PATH}/sonic-pi-tutorial-#{l}.po"
    TUTORIAL_FILES.each do |fn|
      generated_file = "#{generated_dir}/#{File.basename(fn, '.md')}.html"
      file generated_file => [fn, po_file, generated_dir] do |t|
        mdpo[po_file] ||= MarkdownPOHelper.new(po_file)
        puts "### Translating #{t.name} with #{po_file}"
        html = markdown_to_html(mdpo[po_file].translate(t.prerequisites.first))
        write_file(t.name, html)
      end
      file TUTORIAL_QRC => generated_file
    end
  end
  
  
  TAB_LIST = {}

  # The examples are small ruby scripts showcasing the
  # Sonic Pi features. For the GUI, we need HTML versions of them.
  
  def example_to_html(name, ruby_code)
    html = CGI.escapeHTML(ruby_code)
    return "" + 
      "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n" +
      "\n" +
      "<body class=\"example\">\n" +
      "<h1># #{name}</h1>\n" +
      "<p><pre><code>\n" +
      "#{html}\n" +
      "</code></pre></p>\n" +
      "</body>"
  end
  
  TAB_LIST["examples"] = []
  generated_dir = "#{RESOURCES_PATH}/examples"
  directory generated_dir
  EXAMPLE_SKILLS.each do |s|
    require 'active_support/inflector'
    FileList["#{EXAMPLES_PATH}/#{s.downcase}/*.rb"].sort.each do |x|
      n = File.basename(x, '.rb')
      generated_file = "#{generated_dir}/#{s.downcase}-#{n}.html"
      file generated_file => [x, generated_dir] do |t|
        puts "### Converting #{t.name}"
        name = ActiveSupport::Inflector.titleize(File.basename(t.name, '.html').split('-', 2)[1])
        convert_file_task(t) { |s| example_to_html(name, s) }
      end
      TAB_LIST["examples"].push(["[#{s}] #{ActiveSupport::Inflector.titleize(n)}", nil, qrc_url(generated_file)])
      file REFERENCE_QRC => generated_file
    end
  end
  
  
  # The Sonic Pi help systems contains reference sections describing
  # the Synths, FX, Samples and the Language items. These are documented
  # in the Sonic Pi server ruby code and will be extracted here.
  
  TAB_LIST["synths"] = []
  generated_dir = "#{RESOURCES_PATH}/synths"
  directory generated_dir
  SonicPi::Synths::SynthInfo.synth_doc_html_map.sort.each do |n, html|
    generated_file = "#{generated_dir}/#{n}.html"
    file generated_file => ["#{SERVER_PATH}/sonicpi/lib/sonicpi/synths/synthinfo.rb", generated_dir] do |t|
      puts "### Generating #{t.name}"
      write_file(t.name, html)
    end
    TAB_LIST["synths"].push([ActiveSupport::Inflector.titleize(n), n, qrc_url(generated_file)])
    file REFERENCE_QRC => generated_file
  end
  
  TAB_LIST["fx"] = []
  generated_dir = "#{RESOURCES_PATH}/fx"
  directory generated_dir
  SonicPi::Synths::SynthInfo.fx_doc_html_map.sort.each do |n, html|
    generated_file = "#{generated_dir}/#{n}.html"
    file generated_file => ["#{SERVER_PATH}/sonicpi/lib/sonicpi/synths/synthinfo.rb", generated_dir] do |t|
      puts "### Generating #{t.name}"
      write_file(t.name, html)
    end
    TAB_LIST["fx"].push([n.end_with?("pf") ? n.upcase : ActiveSupport::Inflector.titleize(n), n, qrc_url(generated_file)])
    file REFERENCE_QRC => generated_file
  end

  TAB_LIST["samples"] = []
  generated_dir = "#{RESOURCES_PATH}/samples"
  directory generated_dir
  SonicPi::Synths::SynthInfo.samples_doc_html_map.sort.each do |n, html|
    generated_file = "#{generated_dir}/#{n.gsub(/ /, '_').downcase}.html"
    file generated_file => ["#{SERVER_PATH}/sonicpi/lib/sonicpi/synths/synthinfo.rb", generated_dir] do |t|
      puts "### Generating #{t.name}"
      write_file(t.name, html)
    end
    TAB_LIST["samples"].push([ActiveSupport::Inflector.titleize(n), nil, qrc_url(generated_file)])
    file REFERENCE_QRC => generated_file
  end

  TAB_LIST["lang"] = []
  generated_dir = "#{RESOURCES_PATH}/lang"
  directory generated_dir
  SonicPi::Lang::Core.docs_html_map.merge(SonicPi::Lang::Sound.docs_html_map).sort.each do |n, html|
    generated_file = "#{generated_dir}/#{n.gsub(/[\?\!]/){ |s| "_x" + s.unpack('H*').first }}.html"
    file generated_file => ["#{SERVER_PATH}/sonicpi/lib/sonicpi/lang/core.rb", "#{SERVER_PATH}/sonicpi/lib/sonicpi/lang/sound.rb", generated_dir] do |t|
      puts "### Generating #{t.name}"
      write_file(t.name, html)
    end
    TAB_LIST["lang"].push([n, n, qrc_url(generated_file)])
    file REFERENCE_QRC => generated_file
  end


  ## ----------- QT RESOURCE FILES -----------


  # The resource qrc file is a list of resources that is needed for
  # the Qt build. All these files will be linked with the Sonic Pi binary
  # and later used within the GUI.
  
  [INFO_QRC, IMAGES_QRC, I18N_QRC, REFERENCE_QRC, TUTORIAL_QRC].each do |qrc|
    file qrc => [RESOURCES_PATH] do |t|
      puts "### Generating #{t.name}"
            
      File.open(t.name, 'w:utf-8') do |f|
        f << "<RCC>\n"
        f << "<qresource prefix=\"/\">\n"
        t.prerequisites.select { |n| File.file?(n) }.sort.each do |n|
          f << "<file>#{relative_path(n, File.dirname(t.name))}</file>\n"
        end
        f << "</qresource>\n"
        f << "</RCC>\n"
      end
    end
  end
  
  
  ## ----------- GENERATED C++ FILE FOR BUILD -----------


  # For the generated .h file, we need to make the ruby strings safe
  # for use in Qt C++ source.
  #   nil         -> NULL
  #   Yay, "fun". -> "Yay, \"fun\""
  #   Och nö      -> QString::fromUtf8("Och nö")
  
  def ruby_to_cpp_string(s)
    return (s.nil?) ? "NULL" : (
      # Escape quotes in strings, add UTF8 handling if necessairy.
      (s.ascii_only? ? "" : "QString::fromUtf8(") +
      "\"#{s.gsub(/"/, '\\"')}\"" +
      (s.ascii_only? ? "" : ")")
    )
  end
  
  
  # The generated .h file lists the resources needed for the
  # Sonic Pi help system. This creates a C++ snippet with an array
  # that contains the resource items.
  
  def make_help_tab(tab_name, list)
    tab_title = ActiveSupport::Inflector.titleize(tab_name)

    cpp_struct = list.map { |n|
      n.map { |s| ruby_to_cpp_string(s) }.join(", ")
    }.
    map{ |s| "{ #{s} }" }.
    join(",\n    ")
    
    return "" + 
      "  // #{tab_name} help info\n" +
      "  struct help_page #{tab_name}HelpPages[] = {\n" +
      "    #{cpp_struct}\n" +
      "  };\n" +
      "  addHelpPage(createHelpTab(tr(\"#{tab_title}\")), #{tab_name}HelpPages, #{list.length});\n"
  end
  
  
  # The title of a tutorial page is the <h1>first headline</h1>
  # found in the HTML. It can be overridden by the
  # <!-- first comment -->.
  
  def read_tutorial_title(filename)
    # Extract chapter.subchapter.
    (chapter, subchapter) = File.basename(filename).split('-')[0].split('.').map{ |s| s.gsub(/^0*/, '') }
    # Indent index with three spaces for subchapters.
    index = (subchapter.nil?) ? "#{chapter} " : "   #{chapter}.#{subchapter} "
    
    content = read_file(filename)
    comment_start = (content =~ /<!-- (.*?) -->/m)
    comment_title = $1
    headline_start = (content =~ /<h1( .*)?>(.*?)<\/h1>/m)
    headline_title = $2
    
    if (!comment_start.nil?) && (comment_start < headline_start) then
      return index + comment_title
    else
      return index + headline_title
    end
  end
  
  def prepare_tutorial_tab_list(lang)
    list = []
    FileList["#{RESOURCES_PATH}/tutorial-#{lang}/*.html"].sort.each do |fn|
      list.push([read_tutorial_title(fn), nil, qrc_url(fn)])
    end
    return list
  end
  
  
  # The .h header file contains generated C++ code which sets up
  # the Sonic Pi GUI's help system and the keyboard shortcuts.
  
  file HELP_HEADER_FILE => [TUTORIAL_QRC, REFERENCE_QRC] do |t|
  
    puts "### Generating #{t.name}"
  
    File.open(t.name, 'w:utf-8') do |f|
  
      f << "// AUTO-GENERATED FILE.\n"
      f << "// It was generated from the Rakefile.\n"
      f << "// Do not edit this file.\n"
      f << "\n"
      f << "void MainWindow::initDocsWindow() {\n"
      f << "\n"
      f << "  QString systemLocale = QLocale::system().name();\n"
      f << "\n"
      
      # This will sort locale code names by reverse length
      # to make sure that a more specific locale is handled
      # before the generic language code.
      # E.g., "de_CH" should be handled before "de".
      TUTORIAL_LANGUAGES.sort_by { |l| -l.length }.each do |l|
        f << "  if (systemLocale.startsWith(\"#{l}\")) {\n"
        f << "\n"
        f << make_help_tab("tutorial", prepare_tutorial_tab_list(l))
        f << "\n"
        f << "  } else "
      end
      
      f << "  {\n\n" unless TUTORIAL_LANGUAGES.empty?
      f << make_help_tab("tutorial", prepare_tutorial_tab_list('en'))
      f << "\n  }\n\n" unless TUTORIAL_LANGUAGES.empty?
    
      TAB_LIST.each do |section, tab_list|
        f << "\n"
        f << make_help_tab(section, tab_list)
      end
      
      f << "  // arguments for autocompletion\n"
      f << "\n"
  
      SonicPi::Synths::SynthInfo.get_all.
      select { |k, v| v.is_a?(SonicPi::Synths::FXInfo) }.
      select { |k, v| !k.to_s.include? 'replace_' }.    
      each do |k, v|
        safe_k = k.to_s[3..-1]
        f << "  // fx :#{safe_k}\n"
        l = v.arg_info.keys.map { |k| "\"#{k}:\"" }.join(", ")
        f << "  autocomplete->addFXArgs(\":#{safe_k}\", {#{l}});\n\n"
      end
  
      SonicPi::Synths::SynthInfo.get_all.
      select { |k, v| v.is_a?(SonicPi::Synths::SynthInfo) }.
      each do |k, v|
        f << "  // synth :#{k}\n"
        l = v.arg_info.keys.map { |k| "\"#{k}:\"" }.join(", ")
        f << "  autocomplete->addSynthArgs(\":#{k}\", {#{l}});\n\n"
      end
  
      f << "}\n"
    end
  end
  CLEAN << HELP_HEADER_FILE


  ## ----------- TRANSLATION RESOURCES -----------
  

  # QT .ts files are converted to a compressed .qm format, which are
  # loaded by the Sonic Pi GUI at runtime.
  
  generated_dir = "#{RESOURCES_PATH}/i18n"
  directory generated_dir
  FileList["#{QT_LANG_PATH}/*.ts"].each do |fn|
    generated_file = "#{generated_dir}/#{File.basename(fn, '.ts')}.qm"
    file generated_file => [fn, generated_dir] do |t|
      puts "### Generating #{t.name}"
      sh "lrelease #{fn} -qm #{generated_file}"
    end
    file I18N_QRC => generated_file
  end
  
  
  # We will need a gettext translation template
  # .pot file of the tutorial for a language file update.
  
  file "#{TUTORIAL_LANG_PATH}/sonic-pi-tutorial.pot" => TUTORIAL_FILES do |t|
    mdpot = MarkdownPOHelper.new()
    t.prerequisites.sort.each do |fn|
      puts "### Extracting pot gettext template from #{fn}"
      mdpot.translate(fn)
    end
    mdpot.write_po(t.name)
  end
  CLEAN << "#{TUTORIAL_LANG_PATH}/sonic-pi-tutorial.pot"
  
  
  # A translation file update merges the updated message strings
  # from the Qt GUI sources and from the tutorial Markdown with the
  # existing translations.
  
  task :update_language_files => ["#{TUTORIAL_LANG_PATH}/sonic-pi-tutorial.pot", HELP_HEADER_FILE] do
    require 'gettext/tools/msgmerge'
    FileList["#{TUTORIAL_LANG_PATH}/*.po"].sort.each do |n|
      puts "### Merging #{n}"
      cmdline = ['--update', '--no-obsolete-entries', n, "#{TUTORIAL_LANG_PATH}/sonic-pi-tutorial.pot"]
      GetText::Tools::MsgMerge.run(*cmdline)
    end
    sh "lupdate -no-obsolete -pro #{QT_PRO_FILE} -ts #{QT_LANG_PATH}/*.ts"
  end
  

  ## ----------- QT GUI BINARY BUILD RULES -----------


  # The Qt build system creates a Makefile from its .pro file
  
  file QT_MAKEFILE => [QT_PRO_FILE, HELP_HEADER_FILE, I18N_QRC, IMAGES_QRC, INFO_QRC] do |t|
    puts "### Generating #{t.name}"
    Dir.chdir(File.dirname(t.name)) do
      sh "qmake -o #{File.basename(t.name)} #{File.basename(QT_PRO_FILE)}"
    end
  end
  CLEAN << QT_MAKEFILE
  
  
  # When all things come together, we can finally build it all into one Qt binary.
  
  file QT_GUI_BINARY => FileList["#{QT_GUI_PATH}/**/*.cpp"]
  file QT_GUI_BINARY => FileList["#{QT_GUI_PATH}/**/*.h"]
  file QT_GUI_BINARY => [QT_MAKEFILE]
  file QT_GUI_BINARY do |t|
    puts "### Making #{t.name}"
    Dir.chdir(File.dirname(t.name)) do
      sh "make"
    end
  end
  CLOBBER << QT_GUI_BINARY
  

  task :compile_qt_gui => [QT_GUI_BINARY]


  ## ----------- RUBY SERVER TEST SUITE -----------
 
 
  # The Sonic Pi ruby server code comes with a Rakefile for
  # its test suite. This calls another rake with it.
  
  task :test_server do
    Dir.chdir(SERVER_TEST_PATH) do
      sh "rake test"
    end
  end
  

end


## ----------- CLEANUP -----------


task :clean_qt_gui do
  make_clean(QT_GUI_PATH)
end


task :clean => [:clean_qt_gui, :clean_gems]
