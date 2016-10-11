require "helper"

describe SimpleCov::Result do
  subject do
    original_result = {
      source_fixture("sample.rb") => [nil, 1, 1, 1, nil, nil, 1, 1, nil, nil],
      source_fixture("app/models/user.rb") => [nil, 1, 1, 1, nil, nil, 1, 0, nil, nil],
      source_fixture("app/controllers/sample_controller.rb") => [nil, 2, 2, 0, nil, nil, 0, nil, nil, nil],
    }
    SimpleCov::Result.new(original_result).files
  end

  it "has 11 covered lines" do
    expect(subject.covered_lines).to eq(11)
  end

  it "has 3 missed lines" do
    expect(subject.missed_lines).to eq(3)
  end

  it "has 19 never lines" do
    expect(subject.never_lines).to eq(19)
  end

  it "has 14 lines of code" do
    expect(subject.lines_of_code).to eq(14)
  end

  it "has 3 skipped lines" do
    expect(subject.skipped_lines).to eq(3)
  end

  it "has the correct covered percent" do
    expect(subject.covered_percent).to eq(78.57142857142857)
  end

  it "has the correct covered percentages" do
    expect(subject.covered_percentages).to eq([50.0, 80.0, 100.0])
  end

  it "has the correct least covered file" do
    expect(subject.least_covered_file).to match(/sample_controller.rb/)
  end

  it "has the correct covered strength" do
    expect(subject.covered_strength).to eq(0.9285714285714286)
  end
end if SimpleCov.usable?
