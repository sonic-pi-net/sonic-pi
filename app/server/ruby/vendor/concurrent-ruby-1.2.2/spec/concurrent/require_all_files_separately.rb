RSpec.describe 'Every file', if: ENV['ISOLATED'] do
  it 'can be required on its own' do
    root = File.expand_path('../..', __dir__)
    require_paths = []
    dirs = ["#{root}/lib/concurrent-ruby", "#{root}/lib/concurrent-ruby-edge"]
    dirs.each do |dir|
      Dir.glob("#{dir}/**/*.rb") do |file|
        require_path = file[dir.size + 1...-3]
        private_file = %w[ruby_ java_ jruby_ truffleruby_].any? { |prefix|
          File.basename(require_path).start_with?(prefix)
        }
        unless private_file
          require_paths << require_path
        end
      end
    end

    require_paths.each do |require_path|
      # An easy way to see the output and backtrace without RSpec formatting it
      # raise require_path unless system RbConfig.ruby, '-w', '-e', 'require ARGV.first', require_path

      # puts require_path
      out = IO.popen([RbConfig.ruby, '-w', '-e', 'require ARGV.first', require_path], err: [:child, :out], &:read)
      status = $?
      expect(out).to eq ""
      expect(status).to be_success
    end
  end
end
