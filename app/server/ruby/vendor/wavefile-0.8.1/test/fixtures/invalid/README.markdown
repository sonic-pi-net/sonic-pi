The files in this folder are not valid wave files, and should not be readable by the WaveFile gem and other audio tools/programs.

* **empty.wav** - An empty file, size of 0 bytes.
* **incomplete_riff_header.wav** - The file consists of the characters "RIFF" and nothing else.
* **bad_riff_header.wav** - The first four bytes in the file are not "RIFF", which is a magic number required for wave files.
* **bad_wavefile_format.wav** - The format code inside the RIFF header is not "WAVE".
* **no_format_chunk.wav** - The file contains a valid RIFF header, but not after that.
* **empty_format_chunk.wav** - The format chunk has no data in it, despite the format chunk header indicating is is 16 bytes long.
* **insufficient_format_chunk.wav** - The format chunk has some data, but not the minimum amount required.
* **no_data_chunk.wav** - The RIFF header and format chunk are valid, but there is no data chunk.
