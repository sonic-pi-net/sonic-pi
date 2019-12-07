# -*- coding: utf-8 -*-

=begin
    mo.rb - A simple class for operating GNU MO file.

    Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
    Copyright (C) 2003-2009  Masao Mutoh
    Copyright (C) 2002  Masahiro Sakai, Masao Mutoh
    Copyright (C) 2001  Masahiro Sakai

        Masahiro Sakai                  <s01397ms at sfc.keio.ac.jp>
        Masao Mutoh                     <mutomasa at gmail.com>

    You can redistribute this file and/or modify it under the same term
    of Ruby.  License of Ruby is included with Ruby distribution in
    the file "README".

=end

require 'stringio'

module GetText
  class MO < Hash
    class InvalidFormat < RuntimeError; end;

    attr_reader :filename

    Header = Struct.new(:magic,
                        :revision,
                        :nstrings,
                        :orig_table_offset,
                        :translated_table_offset,
                        :hash_table_size,
                        :hash_table_offset)

    # The following are only used in .mo files
    # with minor revision >= 1.
    class HeaderRev1 < Header
      attr_accessor :n_sysdep_segments,
      :sysdep_segments_offset,
      :n_sysdep_strings,
      :orig_sysdep_tab_offset,
      :trans_sysdep_tab_offset
    end

    MAGIC_BIG_ENDIAN    = "\x95\x04\x12\xde".force_encoding("ASCII-8BIT")
    MAGIC_LITTLE_ENDIAN = "\xde\x12\x04\x95".force_encoding("ASCII-8BIT")

    def self.open(arg = nil, output_charset = nil)
      result = self.new(output_charset)
      result.load(arg)
    end

    def initialize(output_charset = nil)
      @filename = nil
      @last_modified = nil
      @little_endian = true
      @charset = nil
      @output_charset = output_charset
      @plural_proc = nil
      super()
    end

    def store(msgid, msgstr, options)
      string = generate_original_string(msgid, options)
      self[string] = msgstr
    end

    def update!
      if FileTest.exist?(@filename)
        st = File.stat(@filename)
        load(@filename) unless (@last_modified == [st.ctime, st.mtime])
      else
        warn "#{@filename} was lost." if $DEBUG
        clear
      end
      self
    end

    def load(arg)
      if arg.kind_of? String
        begin
          st = File.stat(arg)
          @last_modified = [st.ctime, st.mtime]
        rescue Exception
        end
        load_from_file(arg)
      else
        load_from_stream(arg)
      end
      @filename = arg
      self
    end

    def load_from_stream(io)
      magic = io.read(4)
      case magic
      when MAGIC_BIG_ENDIAN
        @little_endian = false
      when MAGIC_LITTLE_ENDIAN
        @little_endian = true
      else
        raise InvalidFormat.new(sprintf("Unknown signature %s", magic.dump))
      end

      endian_type6 = @little_endian ? 'V6' : 'N6'
      endian_type_astr = @little_endian ? 'V*' : 'N*'

      header = HeaderRev1.new(magic, *(io.read(4 * 6).unpack(endian_type6)))

      if header.revision == 1
        # FIXME: It doesn't support sysdep correctly.
        header.n_sysdep_segments = io.read(4).unpack(endian_type6)
        header.sysdep_segments_offset = io.read(4).unpack(endian_type6)
        header.n_sysdep_strings = io.read(4).unpack(endian_type6)
        header.orig_sysdep_tab_offset = io.read(4).unpack(endian_type6)
        header.trans_sysdep_tab_offset = io.read(4).unpack(endian_type6)
      elsif header.revision > 1
        raise InvalidFormat.new(sprintf("file format revision %d isn't supported", header.revision))
      end
      io.pos = header.orig_table_offset
      orig_table_data = io.read((4 * 2) * header.nstrings).unpack(endian_type_astr)

      io.pos = header.translated_table_offset
      trans_table_data = io.read((4 * 2) * header.nstrings).unpack(endian_type_astr)

      msgids = Array.new(header.nstrings)
      for i in 0...header.nstrings
        io.pos = orig_table_data[i * 2 + 1]
        msgids[i] = io.read(orig_table_data[i * 2 + 0])
      end

      clear
      for i in 0...header.nstrings
        io.pos = trans_table_data[i * 2 + 1]
        msgstr = io.read(trans_table_data[i * 2 + 0])

        msgid = msgids[i]
        if msgid.nil? || msgid.empty?
          if msgstr
            @charset = nil
            @nplurals = nil
            @plural = nil
            msgstr.each_line{|line|
              if /^Content-Type:/i =~ line and /charset=((?:\w|-)+)/i =~ line
                @charset = $1
              elsif /^Plural-Forms:\s*nplurals\s*\=\s*(\d*);\s*plural\s*\=\s*([^;]*)\n?/ =~ line
                @nplurals = $1
                @plural = $2
              end
              break if @charset and @nplurals
            }
            @nplurals = "1" unless @nplurals
            @plural = "0" unless @plural
          end
        else
          unless msgstr.nil?
            msgstr = convert_encoding(msgstr, msgid)
          end
        end
        msgid.force_encoding(@charset) if @charset
        self[msgid] = msgstr.freeze
      end
      self
    end

    def prime?(number)
      ('1' * number) !~ /^1?$|^(11+?)\1+$/
    end

    begin
      require 'prime'
      def next_prime(seed)
        Prime.instance.find{|x| x > seed }
      end
    rescue LoadError
      def next_prime(seed)
        require 'mathn'
        prime = Prime.new
        while current = prime.succ
          return current if current > seed
        end
      end
    end

    HASHWORDBITS = 32
    # From gettext-0.12.1/gettext-runtime/intl/hash-string.h
    # Defines the so called `hashpjw' function by P.J. Weinberger
    # [see Aho/Sethi/Ullman, COMPILERS: Principles, Techniques and Tools,
    # 1986, 1987 Bell Telephone Laboratories, Inc.]
    def hash_string(str)
      hval = 0
      str.each_byte do |b|
        break if b == '\0'
        hval <<= 4
        hval += b.to_i
        g = hval & (0xf << (HASHWORDBITS - 4))
        if (g != 0)
          hval ^= g >> (HASHWORDBITS - 8)
          hval ^= g
        end
      end
      hval
    end

    #Save data as little endian format.
    def save_to_stream(io)
      # remove untranslated message
      translated_messages = reject do |msgid, msgstr|
        msgstr.nil?
      end

      size = translated_messages.size
      header_size = 4 * 7
      table_size  = 4 * 2 * size

      hash_table_size = next_prime((size * 4) / 3)
      hash_table_size = 3 if hash_table_size <= 2
      header = Header.new(
                          MAGIC_LITTLE_ENDIAN,          # magic
                          0,                            # revision
                          size,                         # nstrings
                          header_size,                  # orig_table_offset
                          header_size + table_size,     # translated_table_offset
                          hash_table_size,              # hash_table_size
                          header_size + table_size * 2  # hash_table_offset
                          )
      io.write(header.to_a.pack('a4V*'))

      ary = translated_messages.to_a
      ary.sort!{|a, b| a[0] <=> b[0]} # sort by original string

      pos = header.hash_table_size * 4 + header.hash_table_offset

      orig_table_data = Array.new()
      ary.each{|item, _|
        orig_table_data.push(item.bytesize)
        orig_table_data.push(pos)
        pos += item.bytesize + 1 # +1 is <NUL>
      }
      io.write(orig_table_data.pack('V*'))

      trans_table_data = Array.new()
      ary.each{|_, item|
        trans_table_data.push(item.bytesize)
        trans_table_data.push(pos)
        pos += item.bytesize + 1 # +1 is <NUL>
      }
      io.write(trans_table_data.pack('V*'))

      hash_tab = Array.new(hash_table_size)
      j = 0
      ary[0...size].each {|key, _|
        hash_val = hash_string(key)
        idx = hash_val % hash_table_size
        if hash_tab[idx] != nil
          incr = 1 + (hash_val % (hash_table_size - 2))
          begin
            if (idx >= hash_table_size - incr)
              idx -= hash_table_size - incr
            else
              idx += incr
            end
          end until (hash_tab[idx] == nil)
        end
        hash_tab[idx] = j + 1
        j += 1
      }
      hash_tab.collect!{|i| i ? i : 0}

      io.write(hash_tab.pack('V*'))

      ary.each{|item, _| io.write(item); io.write("\0") }
      ary.each{|_, item| io.write(item); io.write("\0") }

      self
    end

    def load_from_file(filename)
      @filename = filename
      begin
        File.open(filename, 'rb'){|f| load_from_stream(f)}
      rescue => e
        e.set_backtrace("File: #{@filename}")
        raise e
      end
    end

    def save_to_file(filename)
      File.open(filename, 'wb'){|f| save_to_stream(f)}
    end

    def set_comment(msgid_or_sym, comment)
      #Do nothing
    end

    def plural_as_proc
      unless @plural_proc
        @plural_proc = Proc.new{|n| eval(@plural)}
        begin
          @plural_proc.call(1)
        rescue
          @plural_proc = Proc.new{|n| 0}
        end
      end
      @plural_proc
    end

    attr_accessor :little_endian, :path, :last_modified
    attr_reader :charset, :nplurals, :plural

    private
    def convert_encoding(string, original_string)
      return string if @output_charset.nil? or @charset.nil?

      if Encoding.find(@output_charset) == Encoding.find(@charset)
        string.force_encoding(@output_charset)
        return string
      end

      string.encode(@output_charset,
                    @charset,
                    :invalid => :replace,
                    :undef => :replace)
    end

    def generate_original_string(msgid, options)
      string = ""

      msgctxt = options.delete(:msgctxt)
      msgid_plural = options.delete(:msgid_plural)

      string << msgctxt << "\004" unless msgctxt.nil?
      string << msgid
      string << "\000" << msgid_plural unless msgid_plural.nil?
      string
    end
  end
end

# Test

if $0 == __FILE__
  if (ARGV.include? "-h") or (ARGV.include? "--help")
    STDERR.puts("mo.rb [filename.mo ...]")
    exit
  end

  ARGV.each{ |item|
    mo = GetText::MO.open(item)
    puts "------------------------------------------------------------------"
    puts "charset  = \"#{mo.charset}\""
    puts "nplurals = \"#{mo.nplurals}\""
    puts "plural   = \"#{mo.plural}\""
    puts "------------------------------------------------------------------"
    mo.each do |key, value|
      puts "original message = #{key.inspect}"
      puts "translated message = #{value.inspect}"
      puts "--------------------------------------------------------------------"
    end
  }
end
