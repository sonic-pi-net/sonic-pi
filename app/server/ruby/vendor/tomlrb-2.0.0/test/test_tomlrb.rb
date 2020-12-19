require 'minitest_helper'
describe Tomlrb::Parser do
  it "parses a toml example file" do
    parsed_file = Tomlrb.load_file('./test/example.toml')
    _(parsed_file).must_equal TomlExamples.example
  end

  it "parses a toml v0.4.0 file" do
    parsed_file = Tomlrb.load_file('./test/example-v0.4.0.toml')
    _(parsed_file).must_equal TomlExamples.example_v_0_4_0
  end

  it "parses a toml hard file" do
    parsed_file = Tomlrb.load_file('./test/hard_example.toml')
    _(parsed_file).must_equal TomlExamples.hard_example
  end

  it "parses a Cargo file" do
    parsed_file = Tomlrb.load_file('./test/Cargo.toml')
    _(parsed_file).must_equal TomlExamples.cargo_example
  end

  it "parses tables whose keys include inf" do
    parsed_file = Tomlrb.load_file('./test/inf_in_keys.toml')
    _(parsed_file).must_equal TomlExamples.inf_in_keys_example
  end

  it "raises an error when defining a table with the same name as an already established array" do
    _{ Tomlrb.load_file('./test/error.toml') }.must_raise(Tomlrb::ParseError)
  end

  it "raises an error when parsing an unclosed table" do
    _{ Tomlrb.parse('''[[missingbracket]\na = 1''') }.must_raise(Tomlrb::ParseError)
  end

  it "does not fail with a false table value (GitHub issue #23)" do
    _( Tomlrb.parse("table=[ {name='name1', visible=true}, {name='name2', visible=false} ]") )
      .must_equal({"table"=>[{"name"=>"name1", "visible"=>true}, {"name"=>"name2", "visible"=>false}]})
  end

  it "raises an error when parsing a float with leading underscore" do
    _{ Tomlrb.parse('x = _1.0') }.must_raise(Tomlrb::ParseError)
  end

  it "raises an error when parsing a float with trailing underscore" do
    _{ Tomlrb.parse('x = 2.0_') }.must_raise(Tomlrb::ParseError)
  end

  it "raises an error when parsing a float without integer before dot" do
    _{ Tomlrb.parse('x = .1') }.must_raise(Tomlrb::ParseError)
  end

  it "raises an error when parsing a float without integer behind dot" do
    _{ Tomlrb.parse('x = 0.') }.must_raise(Tomlrb::ParseError)
  end

  it "raises an error when parsing a float with leading 0, even in exponent" do
    _{ Tomlrb.parse('x = 01.2') }.must_raise(Tomlrb::ParseError)
  end
end

class TomlExamples
  def self.example_v_0_4_0
    {"table"=>{"key"=>"value", "subtable"=>{"key"=>"another value"}, "inline"=>{"name"=>{"first"=>"Tom", "last"=>"Preston-Werner"}, "point"=>{"x"=>1, "y"=>2}}},
     "x"=>{"y"=>{"z"=>{"w"=>{}}}},
     "1"=>{"2"=>{},"2a"=>{}},
     "a"=>{"2"=>{}},
     "2"=>{"b"=>{}},
     "string"=>
    {"basic"=>{"basic"=>"I'm a string. \"You can quote me\". Name\tJosé\nLocation\tSF."},
     "multiline"=>
    {"key1"=>"One\nTwo",
     "key2"=>"One\nTwo",
     "key3"=>"One\nTwo",
     "continued"=>
    {"key1"=>"The quick brown fox jumps over the lazy dog.",
     "key2"=>"The quick brown fox jumps over the lazy dog.",
     "key3"=>"The quick brown fox jumps over the lazy dog."}},
     "literal"=>
    {"winpath"=>"C:\\Users\\nodejs\\templates",
     "winpath2"=>"\\\\ServerX\\admin$\\system32\\",
     "quoted"=>"Tom \"Dubs\" Preston-Werner",
     "regex"=>"<\\i\\c*\\s*>",
     "multiline"=>{"regex2"=>"I [dw]on't need \\d{2} apples", "lines"=>"The first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n"}}},
    "integer"=>{"key1"=>99, "key2"=>42, "key3"=>0, "key4"=>-17, "underscores"=>{"key1"=>1000, "key2"=>5349221, "key3"=>12345}},
    "float"=>
    {"fractional"=>{"key1"=>1.0, "key2"=>3.1415, "key3"=>-0.01},
     "exponent"=>{"key1"=>5.0e+22, "key2"=>1000000.0, "key3"=>-0.02},
     "both"=>{"key"=>6.626e-34},
     "underscores"=>{"key1"=>9224617.445991227, "key2"=>1e1_0_0}},
    "boolean"=>{"True"=>true, "False"=>false},
    "datetime"=>{"key1"=>Time.utc(1979, 05, 27, 07, 32, 0), "key2"=>Time.new(1979, 05, 27, 00, 32, 0, '-07:00'), "key3"=>Time.new(1979, 05, 27, 00, 32, 0.999999, '-07:00'), "key4"=>Tomlrb::LocalDateTime.new(1979, 05, 27, 0, 32, 0.0), "key5"=>Tomlrb::LocalDate.new(1979, 05, 27)}, "array"=>{"key1"=>[1, 2, 3], "key2"=>["red", "yellow", "green"], "key3"=>[[1, 2], [3, 4, 5]], "key4"=>[[1, 2], ["a", "b", "c"]], "key5"=>[1, 2, 3], "key6"=>[1, 2]},
    "products"=>[{"name"=>"Hammer", "sku"=>738594937}, {}, {"name"=>"Nail", "sku"=>284758393, "color"=>"gray"}],
    "fruit"=>
    [{"name"=>"apple", "physical"=>{"color"=>"red", "shape"=>"round"}, "variety"=>[{"name"=>"red delicious"}, {"name"=>"granny smith"}]},
     {"name"=>"banana", "variety"=>[{"name"=>"plantain"}]}]}
  end

  def self.example
    { "1a" => "Bar key starting with number",
      "title"=>"TOML Example",
     "owner"=>{"name"=>"Tom Preston-Werner", "organization"=>"GitHub", "bio"=>"GitHub Cofounder & CEO\nLikes tater tots and beer.", "1-a" => "Another bar key starting with number", "dob"=>Time.parse('1979-05-27 07:32:00 +0000')},
     "database"=>{"server"=>"192.168.1.1", "ports"=>[8001, 8001, 8002], "connection_max"=>5000, "enabled"=>true},
     "servers"=>{"alpha"=>{"ip"=>"10.0.0.1", "dc"=>"eqdc10"}, "beta"=>{"ip"=>"10.0.0.2", "dc"=>"eqdc10", "country"=>"中国"}},
     "clients"=>{"data"=>[["gamma", "delta"], [1, 2]], "hosts"=>["alpha", "omega"]},
     "products"=>[{"name"=>"Hammer", "sku"=>738594937}, {"name"=>"Nail", "sku"=>284758393, "color"=>"gray"}]}
  end

  def self.hard_example
    {"the"=>
     {"test_string"=>"You'll hate me after this - #",
      "hard"=>
      {"test_array"=>["] ", " # "],
       "test_array2"=>["Test #11 ]proved that", "Experiment #9 was a success"],
       "another_test_string"=>" Same thing, but with a string #",
       "harder_test_string"=>" And when \"'s are in the string, along with # \"",
       "bit#"=>
      {"what?"=>"You don't think some user won't do that?",
       "multi_line_array"=>["]"]}}}}
  end

  def self.cargo_example
    {"package"=>{"name"=>"uutils", "version"=>"0.0.1", "authors"=>[], "build"=>"build.rs"}, "features"=>{"unix"=>["arch", "chmod", "chown", "chroot", "du", "groups", "hostid", "hostname", "id", "install", "kill", "logname", "mkfifo", "mknod", "mv", "nice", "nohup", "pathchk", "pinky", "stat", "stdbuf", "timeout", "touch", "tty", "uname", "unlink", "uptime", "users"], "generic"=>["base64", "basename", "cat", "cksum", "comm", "cp", "cut", "dircolors", "dirname", "echo", "env", "expand", "expr", "factor", "false", "fmt", "fold", "hashsum", "head", "link", "ln", "ls", "mkdir", "mktemp", "nl", "nproc", "od", "paste", "printenv", "printf", "ptx", "pwd", "readlink", "realpath", "relpath", "rm", "rmdir", "seq", "shred", "shuf", "sleep", "sort", "split", "sum", "sync", "tac", "tail", "tee", "test", "tr", "true", "truncate", "tsort", "unexpand", "uniq", "wc", "whoami", "yes"], "test_unimplemented"=>[], "nightly"=>[], "default"=>["generic", "unix"]}, "dependencies"=>{"uucore"=>{"path"=>"src/uucore"}, "arch"=>{"optional"=>true, "path"=>"src/arch"}, "base64"=>{"optional"=>true, "path"=>"src/base64"}, "basename"=>{"optional"=>true, "path"=>"src/basename"}, "cat"=>{"optional"=>true, "path"=>"src/cat"}, "chmod"=>{"optional"=>true, "path"=>"src/chmod"}, "chown"=>{"optional"=>true, "path"=>"src/chown"}, "chroot"=>{"optional"=>true, "path"=>"src/chroot"}, "cksum"=>{"optional"=>true, "path"=>"src/cksum"}, "comm"=>{"optional"=>true, "path"=>"src/comm"}, "cp"=>{"optional"=>true, "path"=>"src/cp"}, "cut"=>{"optional"=>true, "path"=>"src/cut"}, "dircolors"=>{"optional"=>true, "path"=>"src/dircolors"}, "dirname"=>{"optional"=>true, "path"=>"src/dirname"}, "du"=>{"optional"=>true, "path"=>"src/du"}, "echo"=>{"optional"=>true, "path"=>"src/echo"}, "env"=>{"optional"=>true, "path"=>"src/env"}, "expand"=>{"optional"=>true, "path"=>"src/expand"}, "expr"=>{"optional"=>true, "path"=>"src/expr"}, "factor"=>{"optional"=>true, "path"=>"src/factor"}, "false"=>{"optional"=>true, "path"=>"src/false"}, "fmt"=>{"optional"=>true, "path"=>"src/fmt"}, "fold"=>{"optional"=>true, "path"=>"src/fold"}, "groups"=>{"optional"=>true, "path"=>"src/groups"}, "hashsum"=>{"optional"=>true, "path"=>"src/hashsum"}, "head"=>{"optional"=>true, "path"=>"src/head"}, "hostid"=>{"optional"=>true, "path"=>"src/hostid"}, "hostname"=>{"optional"=>true, "path"=>"src/hostname"}, "id"=>{"optional"=>true, "path"=>"src/id"}, "install"=>{"optional"=>true, "path"=>"src/install"}, "kill"=>{"optional"=>true, "path"=>"src/kill"}, "link"=>{"optional"=>true, "path"=>"src/link"}, "ln"=>{"optional"=>true, "path"=>"src/ln"}, "ls"=>{"optional"=>true, "path"=>"src/ls"}, "logname"=>{"optional"=>true, "path"=>"src/logname"}, "mkdir"=>{"optional"=>true, "path"=>"src/mkdir"}, "mkfifo"=>{"optional"=>true, "path"=>"src/mkfifo"}, "mknod"=>{"optional"=>true, "path"=>"src/mknod"}, "mktemp"=>{"optional"=>true, "path"=>"src/mktemp"}, "mv"=>{"optional"=>true, "path"=>"src/mv"}, "nice"=>{"optional"=>true, "path"=>"src/nice"}, "nl"=>{"optional"=>true, "path"=>"src/nl"}, "nohup"=>{"optional"=>true, "path"=>"src/nohup"}, "nproc"=>{"optional"=>true, "path"=>"src/nproc"}, "od"=>{"optional"=>true, "path"=>"src/od"}, "paste"=>{"optional"=>true, "path"=>"src/paste"}, "pathchk"=>{"optional"=>true, "path"=>"src/pathchk"}, "pinky"=>{"optional"=>true, "path"=>"src/pinky"}, "printenv"=>{"optional"=>true, "path"=>"src/printenv"}, "printf"=>{"optional"=>true, "path"=>"src/printf"}, "ptx"=>{"optional"=>true, "path"=>"src/ptx"}, "pwd"=>{"optional"=>true, "path"=>"src/pwd"}, "readlink"=>{"optional"=>true, "path"=>"src/readlink"}, "realpath"=>{"optional"=>true, "path"=>"src/realpath"}, "relpath"=>{"optional"=>true, "path"=>"src/relpath"}, "rm"=>{"optional"=>true, "path"=>"src/rm"}, "rmdir"=>{"optional"=>true, "path"=>"src/rmdir"}, "seq"=>{"optional"=>true, "path"=>"src/seq"}, "shred"=>{"optional"=>true, "path"=>"src/shred"}, "shuf"=>{"optional"=>true, "path"=>"src/shuf"}, "sleep"=>{"optional"=>true, "path"=>"src/sleep"}, "sort"=>{"optional"=>true, "path"=>"src/sort"}, "split"=>{"optional"=>true, "path"=>"src/split"}, "stat"=>{"optional"=>true, "path"=>"src/stat"}, "stdbuf"=>{"optional"=>true, "path"=>"src/stdbuf"}, "sum"=>{"optional"=>true, "path"=>"src/sum"}, "sync"=>{"optional"=>true, "path"=>"src/sync"}, "tac"=>{"optional"=>true, "path"=>"src/tac"}, "tail"=>{"optional"=>true, "path"=>"src/tail"}, "tee"=>{"optional"=>true, "path"=>"src/tee"}, "test"=>{"optional"=>true, "path"=>"src/test"}, "timeout"=>{"optional"=>true, "path"=>"src/timeout"}, "touch"=>{"optional"=>true, "path"=>"src/touch"}, "tr"=>{"optional"=>true, "path"=>"src/tr"}, "true"=>{"optional"=>true, "path"=>"src/true"}, "truncate"=>{"optional"=>true, "path"=>"src/truncate"}, "tsort"=>{"optional"=>true, "path"=>"src/tsort"}, "tty"=>{"optional"=>true, "path"=>"src/tty"}, "uname"=>{"optional"=>true, "path"=>"src/uname"}, "unexpand"=>{"optional"=>true, "path"=>"src/unexpand"}, "uniq"=>{"optional"=>true, "path"=>"src/uniq"}, "unlink"=>{"optional"=>true, "path"=>"src/unlink"}, "uptime"=>{"optional"=>true, "path"=>"src/uptime"}, "users"=>{"optional"=>true, "path"=>"src/users"}, "wc"=>{"optional"=>true, "path"=>"src/wc"}, "whoami"=>{"optional"=>true, "path"=>"src/whoami"}, "yes"=>{"optional"=>true, "path"=>"src/yes"}}, "dev-dependencies"=>{"time"=>"*", "kernel32-sys"=>"*", "winapi"=>"*", "filetime"=>"*", "libc"=>"*", "memchr"=>"*", "primal"=>"*", "aho-corasick"=>"*", "regex-syntax"=>"*", "regex"=>"*", "rand"=>"*", "tempdir"=>"*"}, "bin"=>[{"name"=>"uutils", "path"=>"src/uutils/uutils.rs"}], "test"=>[{"name"=>"tests"}]}
  end

  def self.inf_in_keys_example
    {
      "info"=>{"this"=>"something"},
      "inf"=>{"this"=>"something"},
      "key1"=>{"inf"=>"something"},
      "key2"=>{"inf"=>{}},
      "nan"=>{"inf"=>{}}
    }
  end
end
