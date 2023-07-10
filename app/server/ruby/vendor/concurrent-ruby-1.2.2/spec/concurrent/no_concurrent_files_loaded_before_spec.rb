files_loaded_before = $LOADED_FEATURES.grep(/\/concurrent\//).grep_v(/\/version\.rb$/)

RSpec.describe 'The test harness', if: ENV['ISOLATED'] do
  it 'does not load concurrent-ruby files to ensure there are no missing requires' do
    expect(files_loaded_before).to eq []
  end
end
