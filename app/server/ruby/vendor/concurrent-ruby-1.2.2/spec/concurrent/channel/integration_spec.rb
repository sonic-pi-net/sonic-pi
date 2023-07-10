require 'concurrent/utility/engine'

RSpec.describe 'channel integration tests', edge: true do

  let!(:examples_root) { File.expand_path(File.join(File.dirname(__FILE__), '../../../examples')) }

  context 'A Tour of Go' do

    let!(:script_root) { File.join(examples_root, 'a-tour-of-go-channels') }

    specify 'channels.rb' do
      expected = [-5, 17, 12]
      result = `ruby #{File.join(script_root, 'channels.rb')}`
      results = result.split(' ').map(&:chomp).collect{|i| i.to_i}

      expect($?.to_i).to eq 0
      expect(results.length).to eq 3
      expected.each do |n|
        expect(results).to include(n)
      end
    end

    specify 'buffered-channels.rb' do
expected = <<-STDOUT
1
2
STDOUT
      result = `ruby #{File.join(script_root, 'buffered-channels.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'range-and-close.rb' do
expected = <<-STDOUT
0
1
1
2
3
5
8
13
21
34
STDOUT
      result = `ruby #{File.join(script_root, 'range-and-close.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'select.rb' do
expected = <<-STDOUT
0
1
1
2
3
5
8
13
21
34
quit
STDOUT
      result = `ruby #{File.join(script_root, 'select.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'default-selection.rb' do
      skip('flaky') if Concurrent.on_jruby? || Concurrent.on_truffleruby?
expected = <<-STDOUT
    .
    .
tick.
    .
    .
tick.
    .
    .
tick.
    .
    .
tick.
    .
    .
tick.
BOOM!
STDOUT
      result = `ruby #{File.join(script_root, 'default-selection.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end
  end

  context 'Go By Example' do

    let!(:script_root) { File.join(examples_root, 'go-by-example-channels') }

    specify 'channels.rb' do
expected = <<-STDOUT
ping
STDOUT
      result = `ruby #{File.join(script_root, 'channels.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'channel-buffering.rb' do
expected = <<-STDOUT
buffered
channel
STDOUT
      result = `ruby #{File.join(script_root, 'channel-buffering.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'channel-synchronization.rb' do
expected = <<-STDOUT
working...
done
STDOUT
      result = `ruby #{File.join(script_root, 'channel-synchronization.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'channel-directions.rb' do
expected = <<-STDOUT
passed message
STDOUT
      result = `ruby #{File.join(script_root, 'channel-directions.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'select.rb' do
expected = <<-STDOUT
received one
received two
STDOUT
      result = `ruby #{File.join(script_root, 'select.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'timeouts.rb' do
expected = <<-STDOUT
timeout 1
result 2
STDOUT
      result = `ruby #{File.join(script_root, 'timeouts.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'non-blocking-channel-operations.rb' do
expected = <<-STDOUT
no message received
no message sent
no activity
STDOUT
      result = `ruby #{File.join(script_root, 'non-blocking-channel-operations.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'closing-channels.rb' do
      expected = [
        'sent job 1',
        'received job 1',
        'sent job 2',
        'received job 2',
        'sent job 3',
        'received job 3',
        'sent all jobs',
        'received all jobs',
      ]
      result = `ruby #{File.join(script_root, 'closing-channels.rb')}`
      expect($?.to_i).to eq 0
      expected.each do |line|
        expect(result).to match(/^#{line}$/)
      end
    end

    specify 'range-over-channels.rb' do
expected = <<-STDOUT
one
two
STDOUT
      result = `ruby #{File.join(script_root, 'range-over-channels.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'timers.rb' do
expected = <<-STDOUT
Timer 1 expired
Timer 2 stopped
STDOUT
      result = `ruby #{File.join(script_root, 'timers.rb')}`
      expect($?.to_i).to eq 0
      expect(result).to eq expected
    end

    specify 'ticker.rb' do
      result = `ruby #{File.join(script_root, 'ticker.rb')}`
      results = result.lines.map(&:chomp)

      expect($?.to_i).to eq 0
      expect(results.length).to eq 4

      (0..2).each do |i|
        expect(results[i]).to match(/^Tick at /)
      end
      expect(results.last).to match('Ticker stopped')
    end

    specify 'worker-pools.rb' do
      expected = [
        /^worker \d processing job 1$/,
        /^worker \d processing job 2$/,
        /^worker \d processing job 3$/,
        /^worker \d processing job 4$/,
        /^worker \d processing job 5$/,
        /^worker \d processing job 6$/,
        /^worker \d processing job 7$/,
        /^worker \d processing job 8$/,
        /^worker \d processing job 9$/,
      ]
      result = `ruby #{File.join(script_root, 'worker-pools.rb')}`
      expect($?.to_i).to eq 0
      expected.each do |regex|
        expect(result).to match(regex)
      end
    end

    specify 'rate-limiting.rb' do
      result = `ruby #{File.join(script_root, 'rate-limiting.rb')}`
      results = result.lines.map(&:chomp)

      expect($?.to_i).to eq 0
      expect(results.length).to eq 11

      (0..4).each do |i|
        expect(results[i]).to match(/^request #{i+1}/)
        expect(results[i+6]).to match(/^request #{i+1}/)
      end
    end
  end
end
