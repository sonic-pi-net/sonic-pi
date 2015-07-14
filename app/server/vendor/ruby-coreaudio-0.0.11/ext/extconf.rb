require 'mkmf'

$CFLAGS << " " << "-ObjC"

unless defined?(have_framework)
  def have_framework(fw, &b)
    checking_for fw do
      src = cpp_include("#{fw}/#{fw}.h") << "\n" "int main(void){return 0;}"
      if try_link(src, opt = "-ObjC -framework #{fw}", &b)
        $defs.push(format("-DHAVE_FRAMEWORK_%s", fw.tr_cpp))
        $LDFLAGS << " " << opt
        true
      else
        false
      end
    end
  end
end

begin
  files = Gem.find_files("narray.h")
  if files.empty?
    narray_dir = $sitearchdir
  else
    narray_dir = File.dirname(files.first)
  end
rescue
  narray_dir = $sitearchdir
end
dir_config("narray", narray_dir, narray_dir)

if not(have_header("narray.h") and have_header("narray_config.h"))
  print <<-EOS
** configure error **
narray.h or narray_config.h is not found.
If you have installed narray to /path/to/narray, try the following:

 % ruby extconf.rb --with-narray-dir=/path/to/narray

or
 % gem install coreaudio -- --with-narray-dir=/path/to/narray

  EOS
  exit false
end

if have_framework("CoreAudio") and
   have_framework("AudioToolbox") and
   have_framework("CoreFoundation") and
   have_framework("Cocoa")

  # check ruby API
  have_func("rb_alloc_tmp_buffer", "ruby.h")
  have_func("rb_free_tmp_buffer", "ruby.h")

  create_header

  # create Makefile
  create_makefile("coreaudio/coreaudio_ext")

  # workaround for mkmf.rb in 1.9.2
  if RUBY_VERSION < "1.9.3"
    open("Makefile", "a") do |f|
      f.puts <<-EOS
.m.o:
	$(CC) $(INCFLAGS) $(CPPFLAGS) $(CFLAGS) $(COUTFLAG)$@ -c $<
      EOS
    end
  end
end
