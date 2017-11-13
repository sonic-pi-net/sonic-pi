#!/usr/bin/env ruby

# Ruby beautifier, version 2.1, 09/11/2006
# Copyright (c) 2006, P. Lutus
# Released under the GPL

# Changes
# Craig Williams
# 12/26/2011
# Modified (very minor) to work with a Sublime Text 2 plugin

# indent regexp tests

@indentExp = [
  /^module\b/,
  /^if\b/,
  /^\@{0,2}[\w\.]*[\s\t]*\=[\s\t]*if\b/,
  /(=\s*|^)until\b/,
  /(=\s*|^)for\b/,
  /^unless\b/,
  /^\@{0,2}[\w\.]*[\s\t]*\=[\s\t]*unless\b/,
  /(=\s*|^)while\b/,
  /(=\s*|^)begin\b/,
  /(^| )case\b/,
  /\bthen\b/,
  /^class\b/,
  /^rescue\b/,
  /^def\b/,
  /\bdo\b/,
  /^else\b/,
  /^elsif\b/,
  /^ensure\b/,
  /\bwhen\b/,
  /\{[^\}]*$/,
  /\[[^\]]*$/,
  /\([^\)]*$/
]

# outdent regexp tests

@outdentExp = [
  /^rescue\b/,
  /^ensure\b/,
  /^elsif\b/,
  /^end\b/,
  /^else\b/,
  /\bwhen\b/,
  /^[^\{]*\}/,
  /^[^\[]*\]/,
  /^[^\(]*\)/
]

def makeTab(tab)
  (tab < 0) ? "" : @tabStr * @tabSize * tab
end

def addLine(line,tab)
  line.strip!
  line = makeTab(tab)+line if line.length > 0
  line + "\n"
end

def beautifyRuby(contents)
  commentBlock = false
  programEnd = false
  multiLineArray = Array.new
  multiLineStr = ""
  tab = 0
  source = contents
  dest = ""
  source.split("\n").each do |line|
    if(!programEnd)
      # detect program end mark
      if(line =~ /^__END__$/)
        programEnd = true
      else
        # combine continuing lines
        if(!(line =~ /^\s*#/) && line =~ /[^\\]\\\s*$/)
          multiLineArray.push line
          multiLineStr += line.sub(/^(.*)\\\s*$/,"\\1")
          next
        end

        # add final line
        if(multiLineStr.length > 0)
          multiLineArray.push line
          multiLineStr += line.sub(/^(.*)\\\s*$/,"\\1")
        end

        tline = ((multiLineStr.length > 0)?multiLineStr:line).strip
        if(tline =~ /^=begin/)
          commentBlock = true
        end
      end
    end
    if(commentBlock || programEnd)
      # add the line unchanged
      dest += line + "\n"
    else
      commentLine = (tline =~ /^#/)
      if(!commentLine)
        # throw out sequences that will
        # only sow confusion
        while tline.gsub!(/'.*?'/,"")
        end
        while tline.gsub!(/".*?"/,"")
        end
        while tline.gsub!(/\`.*?\`/,"")
        end
        while tline.gsub!(/\{[^\{]*?\}/,"")
        end
        while tline.gsub!(/\([^\(]*?\)/,"")
        end
        while tline.gsub!(/\/.*?\//,"")
        end
        while tline.gsub!(/%r(.).*?\1/,"")
        end
        tline.gsub!(/\\\"/,"'")
        @outdentExp.each do |re|
          if(tline =~ re)
            tab -= 1
            break
          end
        end
      end
      if (multiLineArray.length > 0)
        multiLineArray.each do |ml|
          dest += addLine(ml,tab)
        end
        multiLineArray.clear
        multiLineStr = ""
      else
        dest += addLine(line,tab)
      end
      if(!commentLine)
        @indentExp.each do |re|
          if(tline =~ re && !(tline =~ /\s+end\s*$/))
            tab += 1
            break
          end
        end
      end
    end
    if(tline =~ /^=end/)
      commentBlock = false
    end
  end
  STDOUT.write(dest)
  # uncomment this to complain about mismatched blocks
  # if(tab != 0)
  #   STDERR.puts "#{path}: Indentation error: #{tab}"
  # end
end

tab_or_space = ARGV.first
path         = ARGV.last
path.gsub!("\\", "/") if RUBY_PLATFORM =~ /mswin/
contents     = IO.read(path)

if tab_or_space == 'space'
  @tabSize = 2
  @tabStr = " "
else
  @tabSize = 1
  @tabStr = "\t"
end

beautifyRuby(contents)
