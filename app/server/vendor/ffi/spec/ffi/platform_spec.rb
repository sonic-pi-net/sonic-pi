#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

require File.expand_path(File.join(File.dirname(__FILE__), "spec_helper"))

describe "FFI::Platform::LIBSUFFIX" do
  case OS
  when "linux"
    it "returns 'so'" do
      expect(FFI::Platform::LIBSUFFIX).to eq('so')
    end
  when "windows"
    it "returns 'dll'" do
      expect(FFI::Platform::LIBSUFFIX).to eq('dll')
    end
  when "darwin"
    it "returns 'dylib'" do
      expect(FFI::Platform::LIBSUFFIX).to eq('dylib')
    end
  end
end

describe "FFI::Platform::IS_WINDOWS" do
  case OS
  when "linux"
    it "returns false" do
      expect(FFI::Platform::IS_WINDOWS).to be false
    end
  when "windows"
    it "returns true" do
      expect(FFI::Platform::IS_WINDOWS).to be true
    end
  when "darwin"
    it "returns false" do
      expect(FFI::Platform::IS_WINDOWS).to be false
    end
  end
end

describe "FFI::Platform::ARCH" do
  it "returns the architecture type" do
    expect(FFI::Platform::ARCH).to eq(CPU)
  end
end

describe "FFI::Platform::OS" do
  case OS
  when "linux"
    it "returns 'linux' as a string" do
      expect(FFI::Platform::OS).to eq('linux')
    end
  when "windows"
    it "returns 'windows' as a string" do
      expect(FFI::Platform::OS).to eq('windows')
    end
  when "darwin"
    it "returns 'darwin' as a string" do
      expect(FFI::Platform::OS).to eq('darwin')
    end
  end
end

describe "FFI::Platform.windows?" do
  case OS
  when "linux"
    it "returns false" do
      expect(FFI::Platform.windows?).to be false
    end
  when "windows"
    it "returns true" do
      expect(FFI::Platform.windows?).to be true
    end
  when "darwin"
    it "returns false" do
      expect(FFI::Platform.windows?).to be false
    end
  end
end

describe "FFI::Platform.mac?" do
  case OS
  when "linux"
    it "returns false" do
      expect(FFI::Platform.mac?).to be false
    end
  when "windows"
    it "returns false" do
      expect(FFI::Platform.mac?).to be false
    end
  when "darwin"
    it "returns true" do
      expect(FFI::Platform.mac?).to be true
    end
  end
end

describe "FFI::Platform.unix?" do
  case OS
  when "linux"
    it "returns true" do
      expect(FFI::Platform.unix?).to be true
    end
  when "windows"
    it "returns false" do
      expect(FFI::Platform.unix?).to be false
    end
  when "darwin"
    it "returns true" do
      expect(FFI::Platform.unix?).to be true
    end
  end
end
