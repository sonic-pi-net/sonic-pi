# SPDX-License-Identifier: AGPL-3.0-or-later
# Samples as the language sees them: which files exist, what is in them,
# and Sonic Pi's rules for turning names, folders, filters and indexes into
# a path. The host tells the runtime what files exist (install); nothing
# here opens a file, so the same code runs in the browser.
module SonicPi
  module Samples
    EXTENSIONS = %w[wav wave aif aiff flac mp3 ogg oga]

    # What is known about one sound file.
    class Info
      attr_reader :path, :num_frames, :num_chans, :sample_rate

      def initialize(path, num_frames, num_chans, sample_rate)
        @path = path
        @num_frames = num_frames
        @num_chans = num_chans
        @sample_rate = sample_rate
        @slices = {}
      end

      def duration = @num_frames / @sample_rate.to_f

      # Onset times as fractions of the length, from the onsets the host
      # installed (for now, Sonic Pi's aubio_onset output as data).
      def onsets(stretch = 1)
        data = Samples.onsets_for(@path)
        raise "Unable to find onsets for sample with path #{@path}" unless data
        SonicPi::Ring.new(data.map { |el| [1, el / duration].min * stretch })
      end

      def onset_slices
        return @onset_slices if @onset_slices
        bounds = onsets.to_a.dup
        bounds << 0 if bounds.empty?
        bounds << 1 if bounds[-1] != 1
        res = []
        (0...(bounds.size - 1)).each { |i| res << { start: bounds[i], finish: bounds[i + 1], index: i } }
        @onset_slices = SonicPi::Ring.new(res)
      end

      def slices(num = 16, start = 0, finish = 1)
        key = [num, start, finish]
        return @slices[key] if @slices[key]
        raise "start arg must be a number, got: #{start.inspect}" unless start.is_a?(Numeric)
        raise "finish arg must be a number, got: #{finish.inspect}" unless finish.is_a?(Numeric)
        res = []
        slice_size = (finish - start) / num.to_f
        prev = start
        val = start + slice_size
        num.to_i.times do |n|
          res << { start: prev, finish: val, index: n }
          prev = val
          val += slice_size
        end
        @slices[key] = SonicPi::Ring.new(res)
      end

      def to_s = "#<SampleBuffer @num_chans=#{@num_chans}, @num_frames=#{@num_frames}, @sample_rate=#{@sample_rate}, @duration=#{SonicPi::FloatFormat.to_s(duration)}, @path=#{@path}>"
      def inspect = to_s
    end

    class << self
      def reset!
        @files = {}          # path → Info
        @dirs = {}           # dir → sorted paths in it
        @onsets = {}         # path → [seconds]
        @loaded = {}         # path → true once triggered or loaded
        @candidates = {}
        @builtin_dir = nil
      end

      # A run starts with no sample loaded, whatever earlier runs did.
      def start_run!
        @loaded = {}
      end

      def builtin_dir = @builtin_dir
      def builtin_dir=(dir)
        @builtin_dir = dir
      end

      def install(path, num_frames, num_chans, sample_rate)
        @files[path] = Info.new(path, num_frames, num_chans, sample_rate)
        dir = dirname(path)
        (@dirs[dir] ||= []) << path
        @dirs[dir].sort!
        @dirs[dir].uniq!
      end

      def install_onsets(path, seconds) = (@onsets[path] = seconds)
      # Installed onsets first; otherwise the built-in table by file name.
      def onsets_for(path) = @onsets[path] || (SonicPi::Data::ONSETS[basename(path)] if SonicPi::Data.const_defined?(:ONSETS))

      def info(path) = @files[path]
      def exist?(path) = @files.key?(path) || @dirs.key?(path)
      def directory?(path) = @dirs.key?(path)
      def loaded?(path) = @loaded.key?(path)
      def mark_loaded(path) = (@loaded[path] = true)
      def unload(path) = @loaded.delete(path)
      def unload_all = @loaded.clear
      def loaded_paths = @loaded.keys

      def dirname(path)
        i = path.rindex("/")
        i ? (i == 0 ? "/" : path[0, i]) : "."
      end

      def basename(path, strip_ext = false)
        i = path.rindex("/")
        b = i ? path[(i + 1)..] : path
        if strip_ext
          j = b.rindex(".")
          b = b[0, j] if j && j > 0
        end
        b
      end

      def expand(path)
        path = path.to_s
        path = "#{@builtin_dir}/#{path[2..]}" if path.start_with?("~/") && @builtin_dir   # no home here; "~" is not a sample path
        path
      end

      def ls_samples(dir, recursive = false)
        return [] unless @dirs[dir]
        paths = @dirs[dir]
        if recursive
          paths = @dirs.select { |d, _| d == dir || d.start_with?(dir + "/") }.values.flatten.sort
        end
        paths.select { |p| EXTENSIONS.include?(basename(p).split(".")[-1].to_s.downcase) }
      end

      # Sonic Pi's SampleLoader#find_candidates: leading strings that are
      # files or folders are sources, the rest are filters; with no source,
      # the built-in folder is searched.
      def find_candidates(filts_and_sources)
        filts_and_sources = filts_and_sources.flatten.compact
        return [] if filts_and_sources.empty?
        cached = @candidates[filts_and_sources]
        return cached if cached
        sources, filters = split_candidates_and_filts(filts_and_sources)
        candidates = extract_candidates(sources).dup
        candidates.concat(ls_samples(@builtin_dir)) if sources.empty? && @builtin_dir
        found_proc = false
        filters.each do |f|
          case f
          when String
            candidates = candidates.select { |v| basename(v, true).downcase.include?(f.downcase) || basename(v) == f }
          when Symbol
            candidates = candidates.select { |v| basename(v, true) == f.to_s }
          when Numeric
            candidates = [candidates[f.round % candidates.size]] unless candidates.empty?
          when NilClass
          when Proc
            raise "Sample Pack Proc needs to accept 1 argument only. Found #{f.arity}" unless f.arity == 1
            found_proc = true
            candidates = f.call(candidates)
            candidates = [candidates] unless candidates.is_a?(Array) || candidates.is_a?(SonicPi::Ring)
            candidates = candidates.to_a
          else
            if Object.const_defined?(:Regexp) && f.is_a?(Regexp)
              candidates = candidates.select { |v| basename(v, true).match(f) }
            else
              raise "Unknown sample filter type: #{f.class} - got: #{f.inspect}"
            end
          end
        end
        @candidates[filts_and_sources] = candidates unless found_proc
        candidates
      end

      def split_candidates_and_filts(filts_and_sources)
        sources = []
        idx = 0
        filts_and_sources.each do |el|
          break unless el.is_a?(String)
          p = expand(el)
          if exist?(p) || (p.end_with?("**") && directory?(p[0, p.size - 2]))
            idx += 1
            sources << p
          else
            break
          end
        end
        [sources, filts_and_sources[idx..]]
      end

      def extract_candidates(sources)
        all = []
        sources.each do |c|
          expanded = expand(c)
          if expanded.end_with?("**") && directory?(expanded[0, expanded.size - 2])
            all.concat(ls_samples(expanded[0, expanded.size - 2], true))
          elsif directory?(expanded)
            all.concat(ls_samples(expanded))
          elsif @files.key?(expanded)
            all << expanded
          else
            raise "Unknown sample candidate kind: #{expanded.inspect}. Not a file, directory or /** glob."
          end
        end
        all
      end
    end
    reset!
  end
end
