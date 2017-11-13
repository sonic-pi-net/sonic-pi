open("src/narray.h") do |f|
  f.each_line do |l|
    if /NARRAY_VERSION "([\d.]+)"/ =~ l
      NARRAY_VERSION = $1
      break
    end
  end
end

GEMSPEC = Gem::Specification.new do |s|
  s.name = "narray"
  s.version = NARRAY_VERSION

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Masahiro Tanaka"]
  s.date = Time.now.strftime("%F")
  s.description = "Numerical N-dimensional Array class"
  s.email = "masa16.tanaka@gmail.com"
  s.extensions = ["src/extconf.rb"]
  s.files = %w[
ChangeLog
MANIFEST
README.md
README.ja.md
SPEC.en.txt
SPEC.ja.txt
src/depend
src/extconf.rb
src/mkmath.rb
src/mknafunc.rb
src/mkop.rb
src/na_array.c
src/na_func.c
src/na_index.c
src/na_linalg.c
src/na_random.c
src/narray.c
src/narray.def
src/narray.h
src/narray_local.h
src/lib/narray_ext.rb
src/lib/nmatrix.rb
]
  s.rdoc_options = %w[
    --title NArray
    --main NArray
    --exclude mk.*
    --exclude extconf\.rb
    --exclude src/.*\.h
    --exclude src/lib/
    --exclude .*\.o
    --exclude narray\.so
    --exclude libnarray\.*
  ]
  s.homepage = "http://masa16.github.io/narray/"
  s.require_paths = ["."]
  s.rubygems_version = "1.8.10"
  s.summary = "N-dimensional Numerical Array class for Ruby"
  s.license = "Ruby"

  if s.respond_to? :specification_version then
    s.specification_version = 2

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
    else
    end
  else
  end
end
