require "helper"

describe SimpleCov::SourceFile::Line do
  context "a source line" do
    subject do
      SimpleCov::SourceFile::Line.new("# the ruby source", 5, 3)
    end

    it 'returns "# the ruby source" as src' do
      expect(subject.src).to eq("# the ruby source")
    end

    it "returns the same for source as for src" do
      expect(subject.src).to eq(subject.source)
    end

    it "has line number 5" do
      expect(subject.line_number).to eq(5)
    end

    it "has equal line_number, line and number" do
      expect(subject.line).to eq(subject.line_number)
      expect(subject.number).to eq(subject.line_number)
    end

    context "flagged as skipped!" do
      before do
        subject.skipped!
      end
      it "is not covered" do
        expect(subject).not_to be_covered
      end

      it "is skipped" do
        expect(subject).to be_skipped
      end

      it "is not missed" do
        expect(subject).not_to be_missed
      end

      it "is not never" do
        expect(subject).not_to be_never
      end

      it "status is skipped" do
        expect(subject.status).to eq("skipped")
      end
    end
  end

  context "A source line with coverage" do
    subject do
      SimpleCov::SourceFile::Line.new("# the ruby source", 5, 3)
    end

    it "has coverage of 3" do
      expect(subject.coverage).to eq(3)
    end

    it "is covered" do
      expect(subject).to be_covered
    end

    it "is not skipped" do
      expect(subject).not_to be_skipped
    end

    it "is not missed" do
      expect(subject).not_to be_missed
    end

    it "is not never" do
      expect(subject).not_to be_never
    end

    it "status is covered" do
      expect(subject.status).to eq("covered")
    end
  end

  context "A source line without coverage" do
    subject do
      SimpleCov::SourceFile::Line.new("# the ruby source", 5, 0)
    end

    it "has coverage of 0" do
      expect(subject.coverage).to be_zero
    end

    it "is not covered" do
      expect(subject).not_to be_covered
    end

    it "is not skipped" do
      expect(subject).not_to be_skipped
    end

    it "is missed" do
      expect(subject).to be_missed
    end

    it "is not never" do
      expect(subject).not_to be_never
    end

    it "status is missed" do
      expect(subject.status).to eq("missed")
    end
  end

  context "A source line with no code" do
    subject do
      SimpleCov::SourceFile::Line.new("# the ruby source", 5, nil)
    end

    it "has nil coverage" do
      expect(subject.coverage).to be_nil
    end

    it "is not covered" do
      expect(subject).not_to be_covered
    end

    it "is not skipped" do
      expect(subject).not_to be_skipped
    end

    it "is not missed" do
      expect(subject).not_to be_missed
    end

    it "is never" do
      expect(subject).to be_never
    end

    it "status is never" do
      expect(subject.status).to eq("never")
    end
  end

  it "raises ArgumentError when initialized with invalid src" do
    expect { SimpleCov::SourceFile::Line.new(:symbol, 5, 3) }.to raise_error(ArgumentError)
  end

  it "raises ArgumentError when initialized with invalid line_number" do
    expect { SimpleCov::SourceFile::Line.new("some source", "five", 3) }.to raise_error(ArgumentError)
  end

  it "raises ArgumentError when initialized with invalid coverage" do
    expect { SimpleCov::SourceFile::Line.new("some source", 5, "three") }.to raise_error(ArgumentError)
  end
end if SimpleCov.usable?
