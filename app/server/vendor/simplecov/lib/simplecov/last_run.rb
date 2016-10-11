require "json"

module SimpleCov
  module LastRun
    class << self
      def last_run_path
        File.join(SimpleCov.coverage_path, ".last_run.json")
      end

      def read
        return nil unless File.exist?(last_run_path)
        JSON.parse(File.read(last_run_path))
      end

      def write(json)
        File.open(last_run_path, "w+") do |f|
          f.puts JSON.pretty_generate(json)
        end
      end
    end
  end
end
