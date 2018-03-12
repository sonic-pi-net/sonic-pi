FILE = File.open(ARGV[0], "rb")

UNSIGNED_INT_8  = "C"
UNSIGNED_INT_16 = "v"
UNSIGNED_INT_32 = "V"

def read_bytes(pack_str)
  bytes = []
  
  if pack_str.start_with?("a")
    if pack_str.length > 1
      size = pack_str[1...(pack_str.length)].to_i
    else
      size = 1
    end

    size.times { bytes << FILE.sysread(1).unpack("a") }
    full_string = bytes.join()
    
    return {:actual => full_string, :bytes => bytes }
  elsif pack_str == UNSIGNED_INT_32
    4.times { bytes << FILE.sysread(1) }
    val = bytes.join().unpack(UNSIGNED_INT_32).first

    # Although the bytes are stored little-endian, I think it's easier to read them big-endian
    bytes.reverse!

    return {:actual => val, :bytes => bytes }
  elsif pack_str == UNSIGNED_INT_16
    2.times { bytes << FILE.sysread(1) }
    val = bytes.join().unpack(UNSIGNED_INT_16).first

    # Although the bytes are stored little-endian, I think it's easier to read them big-endian
    bytes.reverse!

    return {:actual => val, :bytes => bytes }
  elsif pack_str.start_with?("b")
    if pack_str.length > 1
      size = pack_str[1...(pack_str.length)].to_i
    else
      size = 1
    end
    size /= 8

    size.times { bytes << FILE.sysread(1).unpack("b8") }
    full_string = bytes.join()

    return {:actual => full_string, :bytes => bytes }
  end
end


def display_line(label, expected, h)
  actual = h[:actual]
  bytes = h[:bytes]

  if Integer === actual
    formatted_bytes = bytes.map {|byte| "#{byte.unpack(UNSIGNED_INT_8)}" }.join(" ")
  elsif String === actual
    formatted_bytes = bytes.inspect.gsub('[[', '[').gsub(']]', ']').gsub(',', '')
  else
    formatted_bytes = bytes
  end

  puts "#{(label + ":").ljust(22)} #{expected.ljust(10)} | #{actual.to_s.ljust(10).gsub("\n\n", "")} | #{formatted_bytes}"
end


def display_chunk_header(heading, expected_chunk_id, actual_chunk_id, chunk_size)
  puts heading
  puts "=================================================================================="
  display_line "Chunk ID", expected_chunk_id, actual_chunk_id
  display_line "Chunk size", "int_32", chunk_size
  puts "----------------------------------+------------+----------------------------------"
end


def read_format_chunk(chunk_id_data, chunk_size_data)
  display_chunk_header("Format Chunk", "fmt ", chunk_id_data, chunk_size_data)
  audio_format_code = read_bytes(UNSIGNED_INT_16)
  display_line "Audio format",    "int_16", audio_format_code
  display_line "Channels",        "int_16", read_bytes(UNSIGNED_INT_16)
  display_line "Sample rate",     "int_32", read_bytes(UNSIGNED_INT_32)
  display_line "Byte rate",       "int_32", read_bytes(UNSIGNED_INT_32)
  display_line "Block align",     "int_16", read_bytes(UNSIGNED_INT_16)
  display_line "Bits per sample", "int_16", read_bytes(UNSIGNED_INT_16)
  if chunk_size_data[:actual] > 16
    extension_size_data = read_bytes(UNSIGNED_INT_16)
    display_line "Extension size", "int_16", extension_size_data
    if extension_size_data[:actual] > 0
      if audio_format_code[:actual] == 65534
        display_line "Valid bits per sample", "int_16", read_bytes(UNSIGNED_INT_16)
        display_line "Speaker mapping", "binary", read_bytes("b32")
        display_line "Sub format GUID", "a16", read_bytes("a16")
      else
        extension_pack_code = "a#{extension_size_data[:actual]}"
        display_line "Raw extension", "alpha_#{extension_size_data[:actual]}", read_bytes(extension_pack_code)
      end
    end
  else
    puts "* NO EXTENSION *"
  end
  puts ""
  puts ""
end


def read_fact_chunk(chunk_id_data, chunk_size_data)
  display_chunk_header("Fact Chunk", "fact", chunk_id_data, chunk_size_data)
  display_line "Sample count", "int_32", read_bytes(UNSIGNED_INT_32)

  if chunk_size_data[:actual] > 4
    FILE.sysread(chunk_size_data[:actual] - 4)
  end

  puts ""
  puts ""
end


def read_peak_chunk(chunk_id_data, chunk_size_data)
  display_chunk_header("Peak Chunk", "PEAK", chunk_id_data, chunk_size_data)

  display_line "Version",          "int_32", read_bytes(UNSIGNED_INT_32)
  display_line "Timestamp",        "int_32", read_bytes(UNSIGNED_INT_32)

  ((chunk_size_data[:actual] - 8) / 8).times do |i|
    # TODO: Fix this to be a 4 byte signed float
    display_line "Chan. #{i + 1} Value",    "int_32", read_bytes(UNSIGNED_INT_32)
    display_line "Chan. #{i + 1} Position", "int_32", read_bytes(UNSIGNED_INT_32)
  end

  puts ""
  puts ""
end


def read_cue_chunk(chunk_id_data, chunk_size_data)
  display_chunk_header("Cue Chunk", "cue ", chunk_id_data, chunk_size_data)

  display_line "Cue point count", "int_32", read_bytes(UNSIGNED_INT_32)

  ((chunk_size_data[:actual] - 4) / 24).times do |i|
    display_line "ID #{i + 1}", "a4", read_bytes("a4")
    display_line "Position #{i + 1}", "int_32", read_bytes(UNSIGNED_INT_32)
    display_line "Data chunk ID #{i + 1}", "alpha_4", read_bytes("a4")
    display_line "Chunk start #{i + 1}", "int_32", read_bytes(UNSIGNED_INT_32)
    display_line "Block start #{i + 1}", "int_32", read_bytes(UNSIGNED_INT_32)
    display_line "Sample offset #{i + 1}", "int_32", read_bytes(UNSIGNED_INT_32)
  end

  puts ""
  puts ""
end


def read_list_chunk(chunk_id_data, chunk_size_data)
  display_chunk_header("List Chunk", "list", chunk_id_data, chunk_size_data)

  display_line "Type id", "alpha_4", read_bytes("a4")
  display_line "FOO", "alpha_4", read_bytes("a4")
  display_line "BAR", "int_32", read_bytes(UNSIGNED_INT_32)
  display_line "BAZ", "alpha_4", read_bytes("a4")
  display_line "REMAIN", "a#{chunk_size_data[:actual] - 16}", read_bytes("a#{chunk_size_data[:actual] - 16}")

  puts ""
  puts ""
end


# RIFF header
puts ""
display_chunk_header("Riff Chunk Header", "RIFF", read_bytes("a4"), read_bytes(UNSIGNED_INT_32))
display_line "Format code", "WAVE", read_bytes("a4")
puts ""
puts ""

count = 0
chunk_id_data = read_bytes("a4")
chunk_size_data = read_bytes(UNSIGNED_INT_32)
while chunk_id_data[:actual] != "data"
  if chunk_id_data[:actual] == "fmt "
    read_format_chunk(chunk_id_data, chunk_size_data)
  elsif chunk_id_data[:actual] == "fact"
    read_fact_chunk(chunk_id_data, chunk_size_data)
  elsif chunk_id_data[:actual] == "PEAK"
    read_peak_chunk(chunk_id_data, chunk_size_data)
  elsif chunk_id_data[:actual] == "cue "
    read_cue_chunk(chunk_id_data, chunk_size_data)
  elsif chunk_id_data[:actual] == "LIST"
    read_list_chunk(chunk_id_data, chunk_size_data)
  else
    chunk_size = chunk_size_data[:actual]
    if chunk_size.odd?
      chunk_size += 1
    end

    FILE.sysread(chunk_size)
    puts "#{chunk_id_data[:actual]} chunk of size #{chunk_size_data[:actual]}, skipping."
    puts ""
    puts ""
  end

  chunk_id_data = read_bytes("a4")
  chunk_size_data = read_bytes(UNSIGNED_INT_32)
end


# Data Chunk
display_chunk_header("Data Chunk", "data", chunk_id_data, chunk_size_data)
display_line "Data Start", "alpha_10", read_bytes("a10")


FILE.close()
