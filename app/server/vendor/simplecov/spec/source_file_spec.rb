require "helper"

describe SimpleCov::SourceFile do
  COVERAGE_FOR_SAMPLE_RB = [nil, 1, 1, 1, nil, nil, 1, 0, nil, nil, nil, nil, nil, nil, nil, nil].freeze
  context "a source file initialized with some coverage data" do
    subject do
      SimpleCov::SourceFile.new(source_fixture("sample.rb"), COVERAGE_FOR_SAMPLE_RB)
    end

    it "has a filename" do
      expect(subject.filename).not_to be_nil
    end

    it "has source equal to src" do
      expect(subject.src).to eq(subject.source)
    end

    it "has source_lines equal to lines" do
      expect(subject.lines).to eq(subject.source_lines)
    end

    it "has 16 source lines" do
      expect(subject.lines.count).to eq(16)
    end

    it "has all source lines of type SimpleCov::SourceFile::Line" do
      subject.lines.each do |line|
        expect(line).to be_a SimpleCov::SourceFile::Line
      end
    end

    it "has 'class Foo' as line(2).source" do
      expect(subject.line(2).source).to eq("class Foo\n")
    end

    it "returns lines number 2, 3, 4, 7 for covered_lines" do
      expect(subject.covered_lines.map(&:line)).to eq([2, 3, 4, 7])
    end

    it "returns lines number 8 for missed_lines" do
      expect(subject.missed_lines.map(&:line)).to eq([8])
    end

    it "returns lines number 1, 5, 6, 9, 10, 11, 15, 16 for never_lines" do
      expect(subject.never_lines.map(&:line)).to eq([1, 5, 6, 9, 10, 11, 15, 16])
    end

    it "returns line numbers 12, 13, 14 for skipped_lines" do
      expect(subject.skipped_lines.map(&:line)).to eq([12, 13, 14])
    end

    it "has 80% covered_percent" do
      expect(subject.covered_percent).to eq(80.0)
    end
  end

  context "simulating potential Ruby 1.9 defect -- see Issue #56" do
    subject do
      SimpleCov::SourceFile.new(source_fixture("sample.rb"), COVERAGE_FOR_SAMPLE_RB + [nil])
    end

    it "has 16 source lines regardless of extra data in coverage array" do
      # Do not litter test output with known warning
      capture_stderr { expect(subject.lines.count).to eq(16) }
    end

    it "prints a warning to stderr if coverage array contains more data than lines in the file" do
      captured_output = capture_stderr do
        subject.lines
      end

      expect(captured_output).to match(/^Warning: coverage data provided/)
    end
  end
end if SimpleCov.usable?
