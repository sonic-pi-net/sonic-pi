require 'ffi'

module Aubio
  extend FFI::Library
  ffi_lib '/usr/local/Cellar/aubio/0.4.2/lib/libaubio.4.2.2.dylib'

	# tempo
	attach_function :new_aubio_tempo, [ :string, :int, :int, :int ], :pointer
	attach_function :aubio_tempo_do, [:pointer, :pointer, :pointer], :void
	attach_function :aubio_tempo_get_last, [:pointer], :int
	attach_function :aubio_tempo_get_last_s, [:pointer], :float
	attach_function :aubio_tempo_get_last_ms, [:pointer], :float
	attach_function :aubio_tempo_set_silence, [:pointer, :float], :int
	attach_function :aubio_tempo_get_silence, [:pointer], :float
	attach_function :aubio_tempo_set_threshold, [:pointer, :float], :int
	attach_function :aubio_tempo_get_threshold, [:pointer], :float
	attach_function :aubio_tempo_get_bpm, [:pointer], :float
	attach_function :aubio_tempo_get_confidence, [:pointer], :float
	attach_function :del_aubio_tempo, [:pointer], :void

  # beattracking / misc
	attach_function :new_aubio_beattracking, [:int, :int, :int], :pointer
	attach_function :aubio_beattracking_do, [:pointer, :pointer, :pointer], :void
	attach_function :aubio_beattracking_get_bpm, [:pointer], :float
	attach_function :aubio_filter_do, [:pointer, :pointer], :void
	attach_function :new_aubio_filter_a_weighting, [:int], :pointer

  # source
	attach_function :new_aubio_source, [:string, :int, :int], :pointer
	attach_function :aubio_source_do, [:pointer, :pointer, :pointer], :void
	attach_function :aubio_source_do_multi, [:pointer, :pointer, :pointer], :void
	attach_function :aubio_source_get_samplerate, [:pointer], :int
	attach_function :aubio_source_get_channels, [:pointer], :int
	attach_function :aubio_source_seek, [:pointer, :int], :int
	attach_function :aubio_source_close, [:pointer], :int
	attach_function :del_aubio_source, [:pointer], :void

  # sink
	attach_function :new_aubio_sink, [:string, :int], :pointer
	attach_function :aubio_sink_preset_samplerate, [:pointer, :int], :void
	attach_function :aubio_sink_preset_channels, [:pointer, :int], :void
	attach_function :aubio_sink_get_samplerate, [:pointer], :int
	attach_function :aubio_sink_get_channels, [:pointer], :int
	attach_function :aubio_sink_do, [:pointer, :pointer, :int], :void
	attach_function :aubio_sink_do_multi, [:pointer, :pointer, :int], :void
	attach_function :aubio_sink_close, [:pointer], :int
	attach_function :del_aubio_sink, [:pointer], :void

  # onset
	attach_function :new_aubio_onset, [:string, :int, :int, :int], :pointer
	attach_function :aubio_onset_do, [:pointer, :pointer, :pointer], :void
	attach_function :aubio_onset_get_last, [:pointer], :int
	attach_function :aubio_onset_get_last_s, [:pointer], :float
	attach_function :aubio_onset_get_last_ms, [:pointer], :float
	attach_function :aubio_onset_set_silence, [:pointer, :float], :int
	attach_function :aubio_onset_get_silence, [:pointer], :float
	attach_function :aubio_onset_get_descriptor, [:pointer], :float
	attach_function :aubio_onset_get_thresholded_descriptor, [:pointer], :float
	attach_function :aubio_onset_set_threshold, [:pointer, :float], :int
	attach_function :aubio_onset_set_minioi, [:pointer, :int], :int
	attach_function :aubio_onset_set_minioi_s, [:pointer, :int], :int
	attach_function :aubio_onset_set_minioi_ms, [:pointer, :float], :int
	attach_function :aubio_onset_set_delay, [:pointer, :int], :int
	attach_function :aubio_onset_set_delay_s, [:pointer, :int], :int
	attach_function :aubio_onset_set_delay_ms, [:pointer, :float], :int
	attach_function :aubio_onset_get_minioi, [:pointer], :int
	attach_function :aubio_onset_get_minioi_s, [:pointer], :float
	attach_function :aubio_onset_get_minioi_ms, [:pointer], :float
	attach_function :aubio_onset_get_delay, [:pointer], :int
	attach_function :aubio_onset_get_delay_s, [:pointer], :float
	attach_function :aubio_onset_get_delay_ms, [:pointer], :float
	attach_function :aubio_onset_get_threshold, [:pointer], :float
	attach_function :del_aubio_onset, [:pointer], :void

  # pitch
	attach_function :new_aubio_pitch, [:string, :int, :int, :int], :pointer
	attach_function :aubio_pitch_do, [:pointer, :pointer, :pointer], :void
	attach_function :aubio_pitch_set_tolerance, [:pointer, :int], :int
	attach_function :aubio_pitch_set_unit, [:pointer, :string], :int
	attach_function :aubio_pitch_set_silence, [:pointer, :float], :int
	attach_function :aubio_pitch_get_silence, [:pointer], :float
	attach_function :aubio_pitch_get_confidence, [:pointer], :float
	attach_function :del_aubio_pitch, [:pointer], :void

  # new fvec
	attach_function :new_fvec, [:int], :pointer
	attach_function :del_fvec, [:pointer], :void
	attach_function :fvec_get_sample, [:pointer, :int], :float
	attach_function :fvec_set_sample, [:pointer, :float, :int], :void
	attach_function :fvec_get_data, [:pointer], :float
	attach_function :fvec_print, [:pointer], :void
	attach_function :fvec_set_all, [:pointer, :float], :void
	attach_function :fvec_zeros, [:pointer], :void
	attach_function :fvec_rev, [:pointer], :void
	attach_function :fvec_weight, [:pointer, :pointer], :void
	attach_function :fvec_copy, [:pointer, :pointer], :void
	attach_function :fvec_ones, [:pointer], :void

  def self.onsets(path, params={})
		sample_rate = params[:sample_rate] || 44100
		window_size = params[:window_size] || 1024
		hop_size    = params[:hop_size]    || 512

    # parser.add_option("-O","--onset-method",
    #         action="store", dest="onset_method", default='default',
    #         metavar = "<onset_method>",
    #         help="onset detection method [default=default] \
    #                 complexdomain|hfc|phase|specdiff|energy|kl|mkl")
    onset_method = params[:onset_method] || "default"

    # # cutting methods
    # parser.add_option("-b","--beat",
    #         action="store_true", dest="beat", default=False,
    #         help="use beat locations")
		beat = params[:beat] || false
    # """
    # parser.add_option("-S","--silencecut",
    #         action="store_true", dest="silencecut", default=False,
    #         help="use silence locations")
		silencecut = params[:silencecut] || false

    # parser.add_option("-s","--silence",
    #         metavar = "<value>",
    #         action="store", dest="silence", default=-70,
    #         help="silence threshold [default=-70]")
		silence = params[:silence] || -70

    #         """
    # # algorithm parameters
    # parser.add_option("-r", "--samplerate",
    #         metavar = "<freq>", type='int',
    #         action="store", dest="samplerate", default=0,
    #         help="samplerate at which the file should be represented")
    # parser.add_option("-B","--bufsize",
    #         action="store", dest="bufsize", default=512,
    #         metavar = "<size>", type='int',
    #         help="buffer size [default=512]")
    # parser.add_option("-H","--hopsize",
    #         metavar = "<size>", type='int',
    #         action="store", dest="hopsize", default=256,
    #         help="overlap size [default=256]")
    # parser.add_option("-t","--onset-threshold",
    #         metavar = "<value>", type="float",
    #         action="store", dest="threshold", default=0.3,
    #         help="onset peak picking threshold [default=0.3]")
    # parser.add_option("-c","--cut",
    #         action="store_true", dest="cut", default=False,
    #         help="cut input sound file at detected labels \
    #                 best used with option -L")

    # # minioi
    # parser.add_option("-M","--minioi",
    #         metavar = "<value>", type='string',
    #         action="store", dest="minioi", default="12ms",
    #         help="minimum inter onset interval [default=12ms]")

    # """
    # parser.add_option("-D","--delay",
    #         action = "store", dest = "delay", type = "float",
    #         metavar = "<seconds>", default=0,
    #         help="number of seconds to take back [default=system]\
    #                 default system delay is 3*hopsize/samplerate")
    # parser.add_option("-C","--dcthreshold",
    #         metavar = "<value>",
    #         action="store", dest="dcthreshold", default=1.,
    #         help="onset peak picking DC component [default=1.]")
    # parser.add_option("-L","--localmin",
    #         action="store_true", dest="localmin", default=False,
    #         help="use local minima after peak detection")
    # parser.add_option("-d","--derivate",
    #         action="store_true", dest="derivate", default=False,
    #         help="derivate onset detection function")
    # parser.add_option("-z","--zerocross",
    #         metavar = "<value>",
    #         action="store", dest="zerothres", default=0.008,
    #         help="zero-crossing threshold for slicing [default=0.00008]")
    #         """
    # # plotting functions
    # """
    # parser.add_option("-p","--plot",
    #         action="store_true", dest="plot", default=False,
    #         help="draw plot")
    # parser.add_option("-x","--xsize",
    #         metavar = "<size>",
    #         action="store", dest="xsize", default=1.,
    #         type='float', help="define xsize for plot")
    # parser.add_option("-y","--ysize",
    #         metavar = "<size>",
    #         action="store", dest="ysize", default=1.,
    #         type='float', help="define ysize for plot")
    # parser.add_option("-f","--function",
    #         action="store_true", dest="func", default=False,
    #         help="print detection function")
    # parser.add_option("-n","--no-onsets",
    #         action="store_true", dest="nplot", default=False,
    #         help="do not plot detected onsets")
    # parser.add_option("-O","--outplot",
    #         metavar = "<output_image>",
    #         action="store", dest="outplot", default=None,
    #         help="save plot to output.{ps,png}")
    # parser.add_option("-F","--spectrogram",
    #         action="store_true", dest="spectro", default=False,
    #         help="add spectrogram to the plot")
    # """
    # parser.add_option("-o","--output", type = str,
    #         metavar = "<outputdir>",
    #         action="store", dest="output_directory", default=None,
    #         help="specify path where slices of the original file should be created")
    # parser.add_option("--cut-until-nsamples", type = int,
    #         metavar = "<samples>",
    #         action = "store", dest = "cut_until_nsamples", default = None,
    #         help="how many extra samples should be added at the end of each slice")
    # parser.add_option("--cut-until-nslices", type = int,
    #         metavar = "<slices>",
    #         action = "store", dest = "cut_until_nslices", default = None,
    #         help="how many extra slices should be added at the end of each slice")

    # parser.add_option("-v","--verbose",
    #         action="store_true", dest="verbose", default=True,
    #         help="make lots of noise [default]")
    # parser.add_option("-q","--quiet",
    #         action="store_false", dest="verbose", default=True,
    #         help="be quiet")
    # (options, args) = parser.parse_args()
    # if not options.source_file:
    #     import os.path
    #     if len(args) == 1:
    #         options.source_file = args[0]
    #     else:
    #         print "no file name given\n", usage
    #         sys.exit(1)
    # return options, args

		source = new_aubio_source(path, sample_rate, hop_size)
    onset  = new_aubio_onset("default", 512, hop_size)
    aubio_onset_set_minioi_ms(onset, 12.0)
		aubio_onset_set_threshold(onset, 0.3)

    timestamps = []
    total_frames = 0
		# create output for source
		samples = new_fvec(hop_size)
		total_frames_counter = 0
		tmp_read = FFI::MemoryPointer.new(:int)

		loop do
			aubio_source_do(source, samples, tmp_read)

		end
    # if options.beat:
    #     o = tempo(options.onset_method, bufsize, hopsize)
    # else:
    #     o = onset(options.onset_method, bufsize, hopsize)
    #     if options.minioi:
    #         if options.minioi.endswith('ms'):
    #             o.set_minioi_ms(int(options.minioi[:-2]))
    #         elif options.minioi.endswith('s'):
    #             o.set_minioi_s(int(options.minioi[:-1]))
    #         else:
    #             o.set_minioi(int(options.minioi))
    # o.set_threshold(options.threshold)

    # timestamps = []
    # total_frames = 0
    # # analyze pass
    # while True:
    #     samples, read = s()
    #     if o(samples):
    #         timestamps.append (o.get_last())
    #         if options.verbose: print "%.4f" % o.get_last_s()
    #     total_frames += read
    #     if read < hopsize: break
    # del s
    # # print some info
    # nstamps = len(timestamps)
    # duration = float (total_frames) / float(samplerate)
    # info = 'found %(nstamps)d timestamps in %(source_file)s' % locals()
    # info += ' (total %(duration).2fs at %(samplerate)dHz)\n' % locals()
    # sys.stderr.write(info)

    # # cutting pass
    # if options.cut and nstamps > 0:
    #     # generate output files
    #     from aubio.slicing import slice_source_at_stamps
    #     timestamps_end = None
    #     if options.cut_until_nslices and options.cut_until_nsamples:
    #         print "warning: using cut_until_nslices, but cut_until_nsamples is set"
    #     if options.cut_until_nsamples:
    #         timestamps_end = [t + options.cut_until_nsamples for t in timestamps[1:]]
    #         timestamps_end += [ 1e120 ]
    #     if options.cut_until_nslices:
    #         timestamps_end = [t for t in timestamps[1 + options.cut_until_nslices:]]
    #         timestamps_end += [ 1e120 ] * (options.cut_until_nslices + 1)
    #     slice_source_at_stamps(source_file, timestamps, timestamps_end = timestamps_end,
    #             output_dir = options.output_directory,
    #             samplerate = samplerate)

    #     # print some info
    #     duration = float (total_frames) / float(samplerate)
    #     info = 'created %(nstamps)d slices from %(source_file)s' % locals()
    #     info += ' (total %(duration).2fs at %(samplerate)dHz)\n' % locals()
    #     sys.stderr.write(info)
	end

  def self.get_features(path, params={})
    sample_rate = params[:sample_rate] || 44100
		window_size = params[:window_size] || 1024
    hop_size    = params[:hop_size]    || 512

    source = new_aubio_source(path, sample_rate, hop_size)
    calculated_sample_rate = aubio_source_get_samplerate(source)
    puts "samplerate: #{calculated_sample_rate}"

		onset = new_aubio_onset('default', window_size, hop_size, sample_rate)
		aubio_onset_set_minioi_ms(onset, 12.0)
		aubio_onset_set_threshold(onset, 0.3)
		onsets = []

	  pitch = new_aubio_pitch('default', window_size, hop_size, sample_rate)
	  aubio_pitch_set_unit(pitch, 'Hz')

	  # create output for source
	  samples = new_fvec(hop_size)
	  # create output for pitch and beat
	  out_fvec = new_fvec(1)
		total_frames_counter = 0
		tmp_read = FFI::MemoryPointer.new(:int)

		loop do
			aubio_source_do(source, samples, tmp_read)

			aubio_pitch_do(pitch, samples, out_fvec)

			cur_time = total_frames_counter / sample_rate
			last_pitch = fvec_get_sample(out_fvec, 0);

			#puts "pitch at #{cur_time} seconds: #{last_pitch} Hz"

			aubio_onset_do(onset, samples, out_fvec)
			is_onset = fvec_get_sample(out_fvec, 0)

			if is_onset > 0.0
				last_onset = aubio_onset_get_last_s(onset)
				onsets << last_onset
				puts "onset at #{last_onset}"
			end

			read = tmp_read.read_int
			total_frames_counter += read
			if(read != hop_size) then
				break
			end
		end

	  cur_time = total_frames_counter.to_f / sample_rate;
	  puts "total time : #{cur_time} seconds (#{total_frames_counter} frames)"
	  puts "found #{onsets.length} onsets total"

    # cleanup
	  del_aubio_source(source);
	  del_aubio_onset(onset);
	  del_aubio_pitch(pitch);

    onsets
  end

# # change comments, swap args, convert to sym

# intPtr = 'int'
# stringPtr = "string" #ref.refType(ref.types.CString);

# {
# 	"aubio_tempo_do": [ "void", [ "pointer", "pointer", "pointer"]],
# 	"aubio_tempo_get_last": [ "int", ["pointer"]],
# 	"aubio_tempo_get_last_s": [ "float", ["pointer"]],
# 	"aubio_tempo_get_last_ms": [ "float", ["pointer"]],
# 	"aubio_tempo_set_silence": [ "int", ["pointer", "float"]],
# 	"aubio_tempo_get_silence": [ "float", ["pointer"]],
# 	"aubio_tempo_set_threshold": [ "int", ["pointer", "float"]],
# 	"aubio_tempo_get_threshold": [ "float", ["pointer"]],
# 	"aubio_tempo_get_bpm": [ "float", ["pointer"]],
# 	"aubio_tempo_get_confidence": [ "float", ["pointer"]],
# 	"del_aubio_tempo": [ "void", ["pointer"]],

# 	# beattracking / misc
# 	"new_aubio_beattracking": [ "pointer", [ "int", "int", "int"]],
# 	"aubio_beattracking_do": [ "void", [ "pointer", "pointer", "pointer"]],
# 	"aubio_beattracking_get_bpm": [ "float", ["pointer"]],
# 	"aubio_filter_do": [ "void", [ "pointer", "pointer" ]],
# 	"new_aubio_filter_a_weighting": [ "pointer", [ "int" ]],

# 	# source
# 	"new_aubio_source": [ "pointer", [ "string", "int", "int" ]],
# 	"aubio_source_do": [ "void", [ "pointer", "pointer", intPtr ]],
# 	"aubio_source_do_multi": [ "void", [ "pointer", "pointer", intPtr ]],
# 	"aubio_source_get_samplerate": [ "int", [ "pointer" ]],
# 	"aubio_source_get_channels": [ "int", [ "pointer" ]],
# 	"aubio_source_seek": [ "int", [ "pointer", "int" ]],
# 	"aubio_source_close": [ "int", [ "pointer" ]],
# 	"del_aubio_source": [ "void", [ "pointer" ]],

# 	# sink
# 	"new_aubio_sink": [ "pointer", [ "string", "int" ]],
# 	"aubio_sink_preset_samplerate": [ "void", [ "pointer", "int" ]],
# 	"aubio_sink_preset_channels": [ "void", [ "pointer", "int" ]],
# 	"aubio_sink_get_samplerate": [ "int", [ "pointer" ]],
# 	"aubio_sink_get_channels": [ "int", [ "pointer" ]],
# 	"aubio_sink_do": ["void", ["pointer", "pointer", "int"]],
# 	"aubio_sink_do_multi": ["void", ["pointer", "pointer", "int"]],
# 	"aubio_sink_close": [ "int", [ "pointer" ]],
# 	"del_aubio_sink": [ "void", [ "pointer" ]],

# 	# onset
# 	"new_aubio_onset": [ "pointer", [ "string", "int", "int", "int"]],
# 	"aubio_onset_do": [ "void", [ "pointer", "pointer", "pointer"]],
# 	"aubio_onset_get_last": [ "int", ["pointer"]],
# 	"aubio_onset_get_last_s": [ "float", ["pointer"]],
# 	"aubio_onset_get_last_ms": [ "float", ["pointer"]],
# 	"aubio_onset_set_silence": [ "int", ["pointer", "float"]],
# 	"aubio_onset_get_silence": [ "float", ["pointer"]],
# 	"aubio_onset_get_descriptor": [ "float", ["pointer"]],
# 	"aubio_onset_get_thresholded_descriptor": [ "float", ["pointer"]],
# 	"aubio_onset_set_threshold": [ "int", ["pointer", "float"]],
# 	"aubio_onset_set_minioi": [ "int", ["pointer", "int"]],
# 	"aubio_onset_set_minioi_s": [ "int", ["pointer", "int"]],
# 	"aubio_onset_set_minioi_ms": [ "int", ["pointer", "float"]],
# 	"aubio_onset_set_delay": [ "int", ["pointer", "int"]],
# 	"aubio_onset_set_delay_s": [ "int", ["pointer", "int"]],
# 	"aubio_onset_set_delay_ms": [ "int", ["pointer", "float"]],
# 	"aubio_onset_get_minioi": [ "int", ["pointer"]],
# 	"aubio_onset_get_minioi_s": [ "float", ["pointer"]],
# 	"aubio_onset_get_minioi_ms": [ "float", ["pointer"]],
# 	"aubio_onset_get_delay": [ "int", ["pointer"]],
# 	"aubio_onset_get_delay_s": [ "float", ["pointer"]],
# 	"aubio_onset_get_delay_ms": [ "float", ["pointer"]],
# 	"aubio_onset_get_threshold": [ "float", ["pointer"]],
# 	"del_aubio_onset": [ "void", ["pointer"]],

# 	# pitch
# 	"new_aubio_pitch": [ "pointer", [ "string", "int", "int", "int"]],
# 	"aubio_pitch_do": ["void", ["pointer", "pointer", "pointer"]],
# 	"aubio_pitch_set_tolerance": [ "int", ["pointer", "int"]],
# 	"aubio_pitch_set_unit": ["int", ["pointer", "string"]],
# 	"aubio_pitch_set_silence": ["int", ["pointer", "float"]],
# 	"aubio_pitch_get_silence": ["float", ["pointer"]],
# 	"aubio_pitch_get_confidence": ["float", ["pointer"]],
# 	"del_aubio_pitch": [ "void", ["pointer"]],

# 	# fvec
# 	"new_fvec": [ "pointer", [ "int" ]],
# 	"del_fvec": [ "void", [ "pointer" ]],
# 	"fvec_get_sample": [ "float", [ "pointer", "int" ]],
# 	"fvec_set_sample": [ "void", [ "pointer", "float", "int" ]],
# 	"fvec_get_data": [ "float", [ "pointer" ]],
# 	"fvec_print": [ "void", [ "pointer" ]],
# 	"fvec_set_all": [ "void", [ "pointer", "float" ]],
# 	"fvec_zeros": [ "void", [ "pointer" ]],
# 	"fvec_rev": [ "void", [ "pointer" ]],
# 	"fvec_weight": [ "void", [ "pointer", "pointer" ]],
# 	"fvec_copy": [ "void", [ "pointer", "pointer" ]],
# 	"fvec_ones": [ "void", [ "pointer" ]],
# }.each do |k,v|
#   puts "attach_function :#{k.to_sym}, #{v.last.map(&:to_sym)}, :#{v.first.to_sym}"
# end
end

puts Aubio.get_features("/Applications/Sonic Pi.app/etc/samples/loop_amen.flac", hop_size: 256, sample_rate: 44100).inspect
