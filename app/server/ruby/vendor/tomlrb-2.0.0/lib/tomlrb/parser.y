class Tomlrb::GeneratedParser
token IDENTIFIER STRING_MULTI STRING_BASIC STRING_LITERAL_MULTI STRING_LITERAL DATETIME LOCAL_DATETIME LOCAL_DATE LOCAL_TIME INTEGER HEX_INTEGER OCT_INTEGER BIN_INTEGER FLOAT FLOAT_INF FLOAT_NAN TRUE FALSE NEWLINE EOS
rule
  expressions
    | expressions expression
    | expressions EOS
    ;
  expression
    : table
    | assignment
    | inline_table
    | NEWLINE
    ;
  table
    : table_start table_continued
    ;
  table_start
    : '[' '[' { @handler.start_(:array_of_tables) }
    | '[' { @handler.start_(:table) }
    ;
  table_continued
    : ']' ']' { array = @handler.end_(:array_of_tables); @handler.set_context(array, is_array_of_tables: true) }
    | ']' { array = @handler.end_(:table); @handler.set_context(array) }
    | table_identifier table_next
    ;
  table_next
    : ']' ']' { array = @handler.end_(:array_of_tables); @handler.set_context(array, is_array_of_tables: true) }
    | ']' { array = @handler.end_(:table); @handler.set_context(array) }
    | '.' table_continued
    ;
  table_identifier
    : table_identifier '.' table_identifier_component { @handler.push(val[2]) }
    | table_identifier '.' FLOAT { val[2].split('.').each { |k| @handler.push(k) } }
    | FLOAT {
      keys = val[0].split('.')
      @handler.start_(:table)
      keys.each { |key| @handler.push(key) }
    }
    | table_identifier_component { @handler.push(val[0]) }
    ;
  table_identifier_component
    : IDENTIFIER
    | STRING_BASIC
    | STRING_LITERAL
    | INTEGER
    | HEX_INTEGER
    | OCT_INTEGER
    | BIN_INTEGER
    | FLOAT_INF
    | FLOAT_NAN
    | TRUE
    | FALSE
    ;
  inline_table
    : inline_table_start inline_table_end
    | inline_table_start inline_continued inline_table_end
    ;
  inline_table_start
    : '{' { @handler.start_(:inline) }
    ;
  inline_table_end
    : '}' {
      array = @handler.end_(:inline)
      array.map!.with_index{ |n,i| i.even? ? n.to_sym : n } if @handler.symbolize_keys
      @handler.push(Hash[*array])
    }
    ;
  inline_continued
    : inline_assignment_key inline_assignment_value
    | inline_assignment_key inline_assignment_value inline_next
    ;
  inline_next
    : ',' inline_continued
    ;
  inline_assignment_key
    : inline_assignment_key '.' IDENTIFIER {
      array = @handler.end_(:inline)
      array.each { |key| @handler.push(key) }
      @handler.start_(:inline)
      @handler.push(val[2])
    }
    | IDENTIFIER { @handler.push(val[0]) }
    ;
  inline_assignment_value
    : '=' value
    ;
  assignment
    : assignment_key '=' value EOS {
      keys = @handler.end_(:keys)
      @handler.push(keys.pop)
      @handler.assign(keys)
    }
    | assignment_key '=' value NEWLINE {
      keys = @handler.end_(:keys)
      @handler.push(keys.pop)
      @handler.assign(keys)
    }
    ;
  assignment_key
    : assignment_key '.' assignment_key_component { @handler.push(val[2]) }
    | assignment_key '.' FLOAT { val[2].split('.').each { |k| @handler.push(k) } }
    | FLOAT {
      keys = val[0].split('.')
      @handler.start_(:keys)
      keys.each { |key| @handler.push(key) }
    }
    | assignment_key_component { @handler.start_(:keys); @handler.push(val[0]) }
    ;
  assignment_key_component
    : IDENTIFIER
    | STRING_BASIC
    | STRING_LITERAL
    | INTEGER
    | HEX_INTEGER
    | OCT_INTEGER
    | BIN_INTEGER
    | FLOAT_INF
    | FLOAT_NAN
    | TRUE
    | FALSE
    ;
  array
    : start_array array_continued
    ;
  array_continued
    : ']' { array = @handler.end_(:array); @handler.push(array) }
    | value array_next
    | NEWLINE array_continued
    ;
  array_next
    : ']' { array = @handler.end_(:array); @handler.push(array) }
    | ',' array_continued
    | NEWLINE array_continued
    | ',' NEWLINE array_continued
    ;
  start_array
    : '[' { @handler.start_(:array) }
    ;
  value
    : scalar { @handler.push(val[0]) }
    | array
    | inline_table
    ;
  scalar
    : string
    | literal
    ;
  literal
    | FLOAT { result = val[0].to_f }
    | FLOAT_INF { result = (val[0][0] == '-' ? -1 : 1) * Float::INFINITY }
    | FLOAT_NAN { result = Float::NAN }
    | INTEGER { result = val[0].to_i }
    | HEX_INTEGER { result = val[0].to_i(16) }
    | OCT_INTEGER { result = val[0].to_i(8) }
    | BIN_INTEGER { result = val[0].to_i(2) }
    | TRUE   { result = true }
    | FALSE  { result = false }
    | DATETIME { result = Time.new(*val[0])}
    | LOCAL_DATETIME { result = LocalDateTime.new(*val[0]) }
    | LOCAL_DATE { result = LocalDate.new(*val[0]) }
    | LOCAL_TIME { result = LocalTime.new(*val[0]) }
    ;
  string
    : STRING_MULTI { result = StringUtils.replace_escaped_chars(StringUtils.multiline_replacements(val[0])) }
    | STRING_BASIC { result = StringUtils.replace_escaped_chars(val[0]) }
    | STRING_LITERAL_MULTI { result = StringUtils.strip_spaces(val[0]) }
    | STRING_LITERAL { result = val[0] }
    ;
