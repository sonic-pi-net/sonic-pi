# SPDX-License-Identifier: AGPL-3.0-or-later
# What a program can do where Sonic Pi's runtime runs with files beside it: a native host (runtime/host/
# runtime_host.cpp), in a plugin or an app, rather than a page. Samples and synthdefs by their paths, folders of
# them, a file's code run. Built only into a native runtime (runtime/sources-native.txt): the web's carries none of
# it. Each method asks the host first (Native.files?), so a native runtime whose host has no files behaves as the
# web's. Loaded last, so a method here is the one that stands.
module SonicPi
  module Native
    # The host can read files: runtime_host.cpp says so (Native.has_files?).
    def self.files? = respond_to?(:has_files?) && has_files?
  end

  module Samples
    class << self
      # The folders the host has listed whole. A folder the runtime knows only through a file of it, used on its
      # own, is not one of them: what else is in it has not been asked.
      alias_method :__reset_web!, :reset!
      def reset!
        __reset_web!
        @listed = {}
      end

      # A path the runtime was never told about: the host says what it is. A file is installed with its facts
      # (frames, channels, rate); a folder with every sound file in it.
      def probe(path)
        return false unless Native.files?
        return true if @files.key?(path) || (@listed ||= {}).key?(path)
        if (facts = Native.describe_sample(path))
          install(path, facts[0], facts[1], facts[2])
          return true
        end
        if (files = Native.list_sound_files(path, false))
          @listed[path] = true
          (@dirs[path] ||= [])   # an empty folder is still a folder
          files.each do |f|
            next if @files.key?(f)
            fx = Native.describe_sample(f)
            install(f, fx[0], fx[1], fx[2]) if fx
          end
          return true
        end
        false
      end

      # A path as a program wrote it, as the host knows it: ~ is the home folder, a folder's trailing slash is
      # dropped, so the one spelling keys the tables whichever way it was written ("dir/" and "dir" are one folder).
      alias_method :__expand_web, :expand
      def expand(path)
        path = path.to_s
        return __expand_web(path) unless Native.files?
        Native.expand_path(path)
      end

      # A sample's onsets, found by the host's detector when the runtime has none for it (the built-in samples
      # carry theirs), once per file.
      alias_method :__onsets_for_web, :onsets_for
      def onsets_for(path)
        found = __onsets_for_web(path)
        return found if found || !Native.files?
        data = Native.sample_onsets(path)
        install_onsets(path, data) if data
        data
      end

      # A folder is asked about until the host has listed it: knowing it through one file is not knowing what is
      # in it.
      alias_method :__exist_without_probe?, :exist?
      def exist?(path) = @files.key?(path) || probe(path) || __exist_without_probe?(path)

      alias_method :__directory_without_probe?, :directory?
      def directory?(path) = (probe(path) && @dirs.key?(path)) || __directory_without_probe?(path)
    end
  end

  class Language
    # A sample's buffer, named by its path: the host loads the file itself.
    alias_method :__buffer_file_web, :__buffer_file
    def __buffer_file(path) = Native.files? ? path : __buffer_file_web(path)

    # ── Synthdefs from files ──────────────────────────────────────────────
    # load_synthdef takes a path (a URL too); load_synthdefs a folder, as native Sonic Pi always has.
    alias_method :__load_synthdef_web, :load_synthdef
    def load_synthdef(path = nil)
      return __load_synthdef_web(path) unless Native.files?
      raise "load_synthdef needs the path of a synth design, e.g. load_synthdef \"~/synths/whoosh.scsyndef\"" if path.nil? || path.to_s.empty?
      path = path.to_s
      file = path.split(/[?#]/, 2)[0].split("/").last.to_s
      unless file.downcase.end_with?(".scsyndef")
        raise "load_synthdef needs a .scsyndef file (to load a folder of them, use load_synthdefs). Got: #{path.inspect}"
      end
      name = file[0...-9]
      (SonicPi.external_synthdefs ||= {})[name.to_sym] = path
      @sched.record_synthdef_load(__p, path)
      __log("Loaded synthdef: #{path}")
      name.to_sym
    end

    alias_method :__load_synthdefs_web, :load_synthdefs
    def load_synthdefs(paths = nil)
      return __load_synthdefs_web(paths) unless Native.files?
      list = paths.is_a?(Array) || paths.is_a?(SonicPi::Ring) ? paths.to_a : [paths]
      list.flat_map do |p|
        p = p.to_s
        if p.downcase.end_with?(".scsyndef")
          [load_synthdef(p)]
        elsif (files = Native.list_sound_files(p, false))   # a folder: the host lists it (sound files and synthdefs alike)
          defs = files.select { |f| f.downcase.end_with?(".scsyndef") }
          raise "load_synthdefs found no .scsyndef files in #{p.inspect}" if defs.empty?
          defs.map { |f| load_synthdef(f) }
        else
          raise "load_synthdefs needs a folder of .scsyndef files, or a list of them. Got: #{p.inspect}"
        end
      end
    end

    # ── Files ─────────────────────────────────────────────────────────────
    # run_file: a file's code as a new run, as native's always has (~ is the home folder).
    def run_file(path = nil)
      raise "run_file is never on the web: a browser has no files" unless Native.files?
      raise ArgumentError, "run_file needs a path, e.g. run_file \"~/music/intro.rb\"" if path.nil? || path.to_s.empty?
      code = Native.read_file(path.to_s)
      raise IOError, "Unable to run file - no file found with path: #{path}" if code.nil?
      run_code(code)
    end

    # ── What this Sonic Pi has not got ────────────────────────────────────
    # The web's words name the browser; a host with files is not one.
    alias_method :__no_osc_web, :__no_osc
    def __no_osc(name)
      return __no_osc_web(name) unless Native.files?
      raise "#{name} is not available in this Sonic Pi: it cannot send or receive OSC"
    end

    alias_method :__no_tracks_web, :__no_tracks
    def __no_tracks(name)
      return __no_tracks_web(name) unless Native.files?
      raise "#{name} is not available in this Sonic Pi: it has no tracks"
    end
  end
end
