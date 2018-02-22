# These libs require chromaprint and sox to be installed
# brew install chromaprint sox
# should work on a mac

require 'chromaprint'
require 'ruby-sox'

# Change these paths accordingly
orig = Dir.glob("/tmp/*.rb.wav")
linux = Dir.glob("/Users/xavierriley/Desktop/linux_sp_test/*.rb.wav")

file_pairs = orig.map do |filename|
  b_file = linux.find {|linux_filepath| File.basename(linux_filepath) == File.basename(filename) }
  b_file.nil? ? nil : [filename, b_file]
end.compact

file_pairs.each do |a, b|
  begin
    a_trimmed = Tempfile.new([File.basename(a), '.wav']).path
    b_trimmed = Tempfile.new([File.basename(b), '.wav']).path

    sox = Sox::Cmd.new
      .add_input(a)
      .set_output(a_trimmed)
      .set_effects(:silence => "1 0.1 0.1% reverse silence 1 0.1 0.1% reverse")

    #puts "Trimming silence: #{sox.to_s}"
    sox.run

    sox = Sox::Cmd.new
      .add_input(b)
      .set_output(b_trimmed)
      .set_effects(:silence => "1 0.1 0.1% reverse silence 1 0.1 0.1% reverse")

    #puts "Trimming silence: #{sox.to_s}"
    sox.run

    # Create context for rate=44100 and channel=1.
    context1 = Chromaprint::Context.new(44100, 1)
    context2 = Chromaprint::Context.new(44100, 1)

    fingerprint1 = context1.get_fingerprint(File.binread(a_trimmed))
    fingerprint2 = context2.get_fingerprint(File.binread(b_trimmed))

    # Compressed fingerprint, returned by chromaprint_get_fingerprint() C function.
    # fingerprint1.compressed # => "AQAALOkUKdlChE92NNeFn8EjF9..."

    # Raw fingerprint, returned by chromaprint_get_raw_fingerprint() C function.
    # fingerprint1.raw # => [294890785, 328373552, 315802880, 303481088, ...]

    # Compare 2 fingerprints, result is 0..1 range, where 1 is 100% similarity.
    comparison = fingerprint1.compare(fingerprint2)
    puts "Comparing #{File.basename(a)}"
    puts "Score: #{comparison}"
    if comparison < 0.9
      puts "WARNING: these files were not similar"
    end
  rescue
    puts "Error handling file #{a}"
    next
  end
end
