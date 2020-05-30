# this needs to be set on the command line when running this script
# ENV['LD_LIBRARY_PATH'] = "/usr/local/opt/llvm35/lib/llvm-3.5/lib"
require 'ffi_gen'

FFIGen.generate(
  module_name: "Aubio::Api",
  ffi_lib:     "/usr/local/Cellar/aubio/0.4.4/lib/libaubio.dylib",
  headers:     Dir["/usr/local/Cellar/aubio/0.4.4/include/aubio/**/*.h"],
  cflags:      `/usr/local/opt/llvm35/bin/llvm-config-3.5 --cflags`.split(" "),
  # the following can be used to trim the aubio_ from the generated function names
  # prefixes:    ["aubio_", "CX"],
  prefixes:    [],
  output:      "lib/aubio/aubio-ffi.rb"
)
