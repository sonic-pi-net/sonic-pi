# Load default formatter gem
require "simplecov-html"
require "pathname"

SimpleCov.profiles.define "root_filter" do
  # Exclude all files outside of simplecov root
  root_filter = nil
  add_filter do |src|
    root_filter ||= /\A#{Regexp.escape(SimpleCov.root)}/io
    !(src.filename =~ root_filter)
  end
end

SimpleCov.profiles.define "test_frameworks" do
  add_filter "/test/"
  add_filter "/features/"
  add_filter "/spec/"
  add_filter "/autotest/"
end

SimpleCov.profiles.define "bundler_filter" do
  add_filter "/vendor/bundle/"
end

SimpleCov.profiles.define "rails" do
  load_profile "test_frameworks"

  add_filter "/config/"
  add_filter "/db/"

  add_group "Controllers", "app/controllers"
  add_group "Models", "app/models"
  add_group "Mailers", "app/mailers"
  add_group "Helpers", "app/helpers"
  add_group "Jobs", %w(app/jobs app/workers)
  add_group "Libraries", "lib"

  track_files "{app,lib}/**/*.rb"
end

# Default configuration
SimpleCov.configure do
  formatter SimpleCov::Formatter::HTMLFormatter
  load_profile "bundler_filter"
  # Exclude files outside of SimpleCov.root
  load_profile "root_filter"
end

# Gotta stash this a-s-a-p, see the CommandGuesser class and i.e. #110 for further info
SimpleCov::CommandGuesser.original_run_command = "#{$PROGRAM_NAME} #{ARGV.join(' ')}"

at_exit do
  # If we are in a different process than called start, don't interfere.
  next if SimpleCov.pid != Process.pid

  @exit_status = if $! # was an exception thrown?
                   # if it was a SystemExit, use the accompanying status
                   # otherwise set a non-zero status representing termination by
                   # some other exception (see github issue 41)
                   $!.is_a?(SystemExit) ? $!.status : SimpleCov::ExitCodes::EXCEPTION
                 else
                   # Store the exit status of the test run since it goes away
                   # after calling the at_exit proc...
                   SimpleCov::ExitCodes::SUCCESS
                 end

  SimpleCov.at_exit.call

  if SimpleCov.result? # Result has been computed
    covered_percent = SimpleCov.result.covered_percent.round(2)
    covered_percentages = SimpleCov.result.covered_percentages.map { |p| p.round(2) }

    if @exit_status == SimpleCov::ExitCodes::SUCCESS # No other errors
      if covered_percent < SimpleCov.minimum_coverage # rubocop:disable Metrics/BlockNesting
        $stderr.printf("Coverage (%.2f%%) is below the expected minimum coverage (%.2f%%).\n", covered_percent, SimpleCov.minimum_coverage)
        @exit_status = SimpleCov::ExitCodes::MINIMUM_COVERAGE
      elsif covered_percentages.any? { |p| p < SimpleCov.minimum_coverage_by_file } # rubocop:disable Metrics/BlockNesting
        $stderr.printf("File (%s) is only (%.2f%%) covered. This is below the expected minimum coverage per file of (%.2f%%).\n", SimpleCov.result.least_covered_file, covered_percentages.min, SimpleCov.minimum_coverage_by_file)
        @exit_status = SimpleCov::ExitCodes::MINIMUM_COVERAGE
      elsif (last_run = SimpleCov::LastRun.read) # rubocop:disable Metrics/BlockNesting
        diff = last_run["result"]["covered_percent"] - covered_percent
        if diff > SimpleCov.maximum_coverage_drop # rubocop:disable Metrics/BlockNesting
          $stderr.printf("Coverage has dropped by %.2f%% since the last time (maximum allowed: %.2f%%).\n", diff, SimpleCov.maximum_coverage_drop)
          @exit_status = SimpleCov::ExitCodes::MAXIMUM_COVERAGE_DROP
        end
      end
    end

    SimpleCov::LastRun.write(:result => {:covered_percent => covered_percent})
  end

  # Force exit with stored status (see github issue #5)
  # unless it's nil or 0 (see github issue #281)
  Kernel.exit @exit_status if @exit_status && @exit_status > 0
end

# Autoload config from ~/.simplecov if present
require "etc"
home_dir = File.expand_path("~") || Etc.getpwuid.dir || (ENV["USER"] && File.expand_path("~#{ENV['USER']}"))
if home_dir
  global_config_path = File.join(home_dir, ".simplecov")
  load global_config_path if File.exist?(global_config_path)
end

# Autoload config from .simplecov if present
# Recurse upwards until we find .simplecov or reach the root directory

config_path = Pathname.new(SimpleCov.root)
loop do
  filename = config_path.join(".simplecov")
  if filename.exist?
    begin
      load filename
    rescue LoadError, StandardError
      $stderr.puts "Warning: Error occurred while trying to load #{filename}. " \
        "Error message: #{$!.message}"
    end
    break
  end
  config_path, = config_path.split
  break if config_path.root?
end
