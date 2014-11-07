#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "Buffer#total" do
  [1,2,3].each do |i|
    { :char => 1, :uchar => 1, :short => 2, :ushort => 2, :int => 4, 
      :uint => 4, :long => FFI::Type::LONG.size, :ulong => FFI::Type::ULONG.size, 
      :long_long => 8, :ulong_long => 8, :float => 4, :double => 8
    }.each_pair do |t, s|

      it "Buffer.alloc_in(#{t}, #{i}).total == #{i * s}" do
        expect(FFI::Buffer.alloc_in(t, i).total).to eq(i * s)
      end

      it "Buffer.alloc_out(#{t}, #{i}).total == #{i * s}" do
        expect(FFI::Buffer.alloc_out(t, i).total).to eq(i * s)
      end

      it "Buffer.alloc_inout(#{t}, #{i}).total == #{i * s}" do
        expect(FFI::Buffer.alloc_inout(t, i).total).to eq(i * s)
      end
    end
  end
end

describe "Buffer#put_char" do
  bufsize = 4
  (0..127).each do |i|
    (0..bufsize-1).each do |offset|
      it "put_char(#{offset}, #{i}).get_char(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_char(offset, i).get_char(offset)).to eq(i)
      end
    end
  end
end

describe "Buffer#put_uchar" do
  bufsize = 4
  (0..255).each do |i|
    (0..bufsize-1).each do |offset|
      it "Buffer.put_uchar(#{offset}, #{i}).get_uchar(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_uchar(offset, i).get_uchar(offset)).to eq(i)
      end
    end
  end 
end

describe "Buffer#put_short" do
  bufsize = 4
  [0, 1, 128, 32767].each do |i|
    (0..bufsize-2).each do |offset|
      it "put_short(#{offset}, #{i}).get_short(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_short(offset, i).get_short(offset)).to eq(i)
      end
    end
  end
end

describe "Buffer#put_ushort" do
  bufsize = 4
  [ 0, 1, 128, 32767, 65535, 0xfee1, 0xdead, 0xbeef, 0xcafe ].each do |i|
    (0..bufsize-2).each do |offset|
      it "put_ushort(#{offset}, #{i}).get_ushort(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_ushort(offset, i).get_ushort(offset)).to eq(i)
      end
    end
  end
end

describe "Buffer#put_int" do
  bufsize = 8
  [0, 1, 128, 32767, 0x7ffffff ].each do |i|
    (0..bufsize-4).each do |offset|
      it "put_int(#{offset}, #{i}).get_int(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_int(offset, i).get_int(offset)).to eq(i)
      end
    end
  end
end

describe "Buffer#put_uint" do
  bufsize = 8
  [ 0, 1, 128, 32767, 65535, 0xfee1dead, 0xcafebabe, 0xffffffff ].each do |i|
    (0..bufsize-4).each do |offset|
      it "put_uint(#{offset}, #{i}).get_uint(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_uint(offset, i).get_uint(offset)).to eq(i)
      end
    end
  end
end

describe "Buffer#put_long" do
  bufsize = 16
  [0, 1, 128, 32767, 0x7ffffff ].each do |i|
    (0..bufsize-FFI::Type::LONG.size).each do |offset|
      it "put_long(#{offset}, #{i}).get_long(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_long(offset, i).get_long(offset)).to eq(i)
      end
    end
  end
end

describe "Buffer#put_ulong" do
  bufsize = 16
  [ 0, 1, 128, 32767, 65535, 0xfee1dead, 0xcafebabe, 0xffffffff ].each do |i|
    (0..bufsize-FFI::Type::LONG.size).each do |offset|
      it "put_ulong(#{offset}, #{i}).get_ulong(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_ulong(offset, i).get_ulong(offset)).to eq(i)
      end
    end
  end
end

describe "Buffer#put_long_long" do
  bufsize = 16
  [0, 1, 128, 32767, 0x7ffffffffffffff ].each do |i|
    (0..bufsize-8).each do |offset|
      it "put_long_long(#{offset}, #{i}).get_long_long(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_long_long(offset, i).get_long_long(offset)).to eq(i)
      end
    end
  end
end

describe "Buffer#put_ulong_long" do
  bufsize = 16
  [ 0, 1, 128, 32767, 65535, 0xdeadcafebabe, 0x7fffffffffffffff ].each do |i|
    (0..bufsize-8).each do |offset|
      it "put_ulong_long(#{offset}, #{i}).get_ulong_long(#{offset}) == #{i}" do
        expect(FFI::Buffer.alloc_in(bufsize).put_ulong_long(offset, i).get_ulong_long(offset)).to eq(i)
      end
    end
  end
end

describe "Reading/Writing binary strings" do
  it "Buffer#put_bytes" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    buf.put_bytes(0, str);
    s2 = buf.get_bytes(0, 11);
    expect(s2).to eq(str)
  end

  it "Buffer#put_bytes with index and length" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    buf.put_bytes(0, str, 5, 6);
    s2 = buf.get_bytes(0, 6);
    expect(s2).to eq(str[5..-1])
  end

  it "Buffer#put_bytes with only index" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    buf.put_bytes(0, str, 5);
    s2 = buf.get_bytes(0, 6);
    expect(s2).to eq(str[5..-1])
  end

  it "Buffer#put_bytes with index > str.length" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    expect { buf.put_bytes(0, str, 12); }.to raise_error
  end

  it "Buffer#put_bytes with length > str.length" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    expect { buf.put_bytes(0, str, 0, 12); }.to raise_error
  end

  it "Buffer#put_bytes with negative index" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    expect { buf.put_bytes(0, str, -1, 12); }.to raise_error
  end

  it "Buffer#write_bytes" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    buf.write_bytes(str)
    s2 = buf.get_bytes(0, 11)
    expect(s2).to eq(str)
  end

  it "Buffer#write_bytes with index and length" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    buf.write_bytes(str, 5, 6)
    s2 = buf.get_bytes(0, 6)
    expect(s2).to eq(str[5..-1])
  end

  it "Buffer#write_bytes with only index" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    buf.write_bytes(str, 5)
    s2 = buf.get_bytes(0, 6)
    expect(s2).to eq(str[5..-1])
  end

  it "Buffer#write_bytes with index > str.length" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    expect { buf.write_bytes(str, 12) }.to raise_error
  end

  it "Buffer#put_bytes with length > str.length" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    expect { buf.put_bytes(0, str, 0, 12) }.to raise_error
  end

  it "Buffer#write_bytes with negative index" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    expect { buf.write_bytes(str, -1, 12) }.to raise_error
  end
end

describe "Reading/Writing ascii strings" do
  it "Buffer#put_string with string containing zero byte" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    buf.put_string(0, str);
    s2 = buf.get_bytes(0, 11);
    expect(s2).to eq(str)
  end

  it "Buffer#get_string with string containing zero byte" do
    str = "hello\0world"
    buf = FFI::Buffer.new 1024
    buf.put_bytes(0, str);
    s2 = buf.get_string(0, 11);
    expect(s2).to eq("hello")
  end

  it "Buffer#put_string without length should NUL terminate" do
    str = "hello"
    buf = FFI::Buffer.new 1024
    buf.put_string(0, str);
    s2 = buf.get_bytes(0, 6);
    expect(s2).to eq("hello\0")
  end
end

describe "Buffer#put_pointer" do
  it "put_pointer(0, p).get_pointer(0) == p" do
    p = FFI::MemoryPointer.new :ulong_long
    p.put_uint(0, 0xdeadbeef)
    buf = FFI::Buffer.alloc_inout 8
    p2 = buf.put_pointer(0, p).get_pointer(0)
    expect(p2).not_to be_nil
    expect(p2).to eq(p)
    expect(p2.get_uint(0)).to eq(0xdeadbeef)
  end
end

describe "Buffer#size" do
  it "should return size" do
    buf = FFI::Buffer.new 14
    expect(buf.size).to eq(14)
  end
end

describe "Buffer#initialize" do
  it "with block should execute block" do
    block_executed = false
    FFI::Buffer.new(:pointer) do |ptr|
      block_executed = true
    end
    expect(block_executed).to be true
  end
end
