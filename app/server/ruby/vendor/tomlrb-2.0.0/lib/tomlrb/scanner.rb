require 'strscan'

module Tomlrb
  class Scanner
    COMMENT = /#[^\u0000-\u0008\u000A-\u001F\u007F]*/
    IDENTIFIER = /[A-Za-z0-9_-]+/
    SPACE = /[ \t]/
    NEWLINE = /(\r?\n)/
    STRING_BASIC = /(["])(?:\\?[^\u0000-\u0008\u000A-\u001F\u007F])*?\1/
    STRING_MULTI = /"{3}([^\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F]*?(?<!\\)"{3,5})/m
    STRING_LITERAL = /(['])(?:\\?[^\u0000-\u0008\u000A-\u001F\u007F])*?\1/
    STRING_LITERAL_MULTI = /'{3}([^\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F]*?'{3,5})/m
    DATETIME = /(-?\d{4})-(\d{2})-(\d{2})(?:(?:t|\s)(\d{2}):(\d{2}):(\d{2}(?:\.\d+)?))?(z|[-+]\d{2}:\d{2})/i
    LOCAL_DATETIME = /(-?\d{4})-(\d{2})-(\d{2})(?:t|\s)(\d{2}):(\d{2}):(\d{2}(?:\.\d+)?)/i
    LOCAL_DATE = /(-?\d{4})-(\d{2})-(\d{2})/
    LOCAL_TIME = /(\d{2}):(\d{2}):(\d{2}(?:\.\d+)?)/
    FLOAT = /[+-]?(?:(?:\d|[1-9](?:_?\d)*)\.\d(?:_?\d)*|\d+(?=[eE]))(?:[eE][+-]?[0-9_]+)?(?!\w)/
    FLOAT_INF = /[+-]?inf\b/
    FLOAT_NAN = /[+-]?nan\b/
    INTEGER = /[+-]?([1-9](_?\d)*|0)(?![A-Za-z0-9_-]+)/
    HEX_INTEGER = /0x[0-9A-Fa-f][0-9A-Fa-f_]*/
    OCT_INTEGER = /0o[0-7][0-7_]*/
    BIN_INTEGER = /0b[01][01_]*/
    TRUE   = /true/
    FALSE  = /false/

    def initialize(io)
      @ss = StringScanner.new(io.read)
      @eos = false
    end

    def next_token
      case
      when @ss.eos? then process_eos
      when @ss.scan(SPACE) then next_token
      when @ss.scan(COMMENT) then next_token
      when @ss.scan(DATETIME) then process_datetime
      when @ss.scan(LOCAL_DATETIME) then process_local_datetime
      when @ss.scan(LOCAL_DATE) then process_local_date
      when @ss.scan(LOCAL_TIME) then process_local_time
      when text = @ss.scan(STRING_MULTI) then [:STRING_MULTI, text[3..-4]]
      when text = @ss.scan(STRING_BASIC) then [:STRING_BASIC, text[1..-2]]
      when text = @ss.scan(STRING_LITERAL_MULTI) then [:STRING_LITERAL_MULTI, text[3..-4]]
      when text = @ss.scan(STRING_LITERAL) then [:STRING_LITERAL, text[1..-2]]
      when text = @ss.scan(FLOAT) then [:FLOAT, text]
      when text = @ss.scan(FLOAT_INF) then [:FLOAT_INF, text]
      when text = @ss.scan(FLOAT_NAN) then [:FLOAT_NAN, text]
      when text = @ss.scan(INTEGER) then [:INTEGER, text]
      when text = @ss.scan(HEX_INTEGER) then [:HEX_INTEGER, text]
      when text = @ss.scan(OCT_INTEGER) then [:OCT_INTEGER, text]
      when text = @ss.scan(BIN_INTEGER) then [:BIN_INTEGER, text]
      when text = @ss.scan(TRUE)   then [:TRUE, text]
      when text = @ss.scan(FALSE)  then [:FALSE, text]
      when text = @ss.scan(NEWLINE) then [:NEWLINE, text]
      when text = @ss.scan(IDENTIFIER) then [:IDENTIFIER, text]
      else x = @ss.getch; [x, x]
      end
    end

    def process_datetime
      if @ss[7].nil?
        offset = '+00:00'
      else
        offset = @ss[7].gsub('Z', '+00:00')
      end
      args = [@ss[1], @ss[2], @ss[3], @ss[4] || 0, @ss[5] || 0, @ss[6].to_f, offset]
      [:DATETIME, args]
    end

    def process_local_datetime
      args = [@ss[1], @ss[2], @ss[3], @ss[4] || 0, @ss[5] || 0, @ss[6].to_f]
      [:LOCAL_DATETIME, args]
    end

    def process_local_date
      args = [@ss[1], @ss[2], @ss[3]]
      [:LOCAL_DATE, args]
    end

    def process_local_time
      args = [@ss[1], @ss[2], @ss[3].to_f]
      [:LOCAL_TIME, args]
    end

    def process_eos
      return if @eos

      @eos = true
      [:EOS, nil]
    end
  end
end
