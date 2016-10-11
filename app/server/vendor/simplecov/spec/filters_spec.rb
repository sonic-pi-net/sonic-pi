require "helper"

describe SimpleCov::SourceFile do
  subject do
    SimpleCov::SourceFile.new(source_fixture("sample.rb"), [nil, 1, 1, 1, nil, nil, 1, 0, nil, nil])
  end

  it "doesn't match a new SimpleCov::StringFilter 'foobar'" do
    expect(SimpleCov::StringFilter.new("foobar")).not_to be_matches subject
  end

  it "doesn't match a new SimpleCov::StringFilter 'some/path'" do
    expect(SimpleCov::StringFilter.new("some/path")).not_to be_matches subject
  end

  it "matches a new SimpleCov::StringFilter 'spec/fixtures'" do
    expect(SimpleCov::StringFilter.new("spec/fixtures")).to be_matches subject
  end

  it "matches a new SimpleCov::StringFilter 'spec/fixtures/sample.rb'" do
    expect(SimpleCov::StringFilter.new("spec/fixtures/sample.rb")).to be_matches subject
  end

  it "matches a new SimpleCov::StringFilter 'sample.rb'" do
    expect(SimpleCov::StringFilter.new("sample.rb")).to be_matches subject
  end

  it "doesn't match a new SimpleCov::BlockFilter that is not applicable" do
    expect(SimpleCov::BlockFilter.new(proc { |s| File.basename(s.filename) == "foo.rb" })).not_to be_matches subject
  end

  it "matches a new SimpleCov::BlockFilter that is applicable" do
    expect(SimpleCov::BlockFilter.new(proc { |s| File.basename(s.filename) == "sample.rb" })).to be_matches subject
  end

  it "matches a new SimpleCov::ArrayFilter when 'sample.rb' is passed as array" do
    expect(SimpleCov::ArrayFilter.new(["sample.rb"])).to be_matches subject
  end

  it "doesn't match a new SimpleCov::ArrayFilter when a file path different than 'sample.rb' is passed as array" do
    expect(SimpleCov::ArrayFilter.new(["other_file.rb"])).not_to be_matches subject
  end

  it "matches a new SimpleCov::ArrayFilter when two file paths including 'sample.rb' are passed as array" do
    expect(SimpleCov::ArrayFilter.new(["sample.rb", "other_file.rb"])).to be_matches subject
  end

  context "with no filters set up and a basic source file in an array" do
    before do
      @prev_filters = SimpleCov.filters
      SimpleCov.filters = []
    end

    subject do
      [SimpleCov::SourceFile.new(source_fixture("sample.rb"), [nil, 1, 1, 1, nil, nil, 1, 0, nil, nil])]
    end

    after do
      SimpleCov.filters = @prev_filters
    end

    it 'returns 0 items after executing SimpleCov.filtered on files when using a "sample" string filter' do
      SimpleCov.add_filter "sample"
      expect(SimpleCov.filtered(subject).count).to be_zero
    end

    it 'returns 0 items after executing SimpleCov.filtered on files when using a "spec/fixtures" string filter' do
      SimpleCov.add_filter "spec/fixtures"
      expect(SimpleCov.filtered(subject).count).to be_zero
    end

    it 'returns 1 item after executing SimpleCov.filtered on files when using a "fooo" string filter' do
      SimpleCov.add_filter "fooo"
      expect(SimpleCov.filtered(subject).count).to eq(1)
    end

    it "returns 0 items after executing SimpleCov.filtered on files when using a block filter that returns true" do
      SimpleCov.add_filter do
        true
      end
      expect(SimpleCov.filtered(subject).count).to be_zero
    end

    it "returns 1 item after executing SimpleCov.filtered on files when using an always-false block filter" do
      SimpleCov.add_filter do
        false
      end
      expect(SimpleCov.filtered(subject).count).to eq(1)
    end

    it "returns a FileList after filtering" do
      SimpleCov.add_filter "fooo"
      expect(SimpleCov.filtered(subject)).to be_a SimpleCov::FileList
    end
  end
end if SimpleCov.usable?
