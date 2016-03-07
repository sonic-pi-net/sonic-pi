The files in this folder are wave files that do not violate the wave file spec, but can not be read by the WaveFile gem.

* **unsupported_audio_format.wav** - The audio format defined in the format chunk is 2, or ADPCM. While a valid format, this gem only supports 1 (PCM).
* **unsupported_bits_per_sample.wav** - The bits per sample is 24. While this is a valid sample rate, this gem only supports 8, 16, and 32 bits per sample.
* **bad_channel_count.wav** - The channel count defined in the format chunk is 0.
* **bad_sample_rate.wav** - The sample rate defined in the format chunk is 0.
