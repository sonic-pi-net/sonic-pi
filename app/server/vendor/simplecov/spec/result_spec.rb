require "helper"

describe "result" do
  context "with a (mocked) Coverage.result" do
    before do
      @prev_filters = SimpleCov.filters
      SimpleCov.filters = []
      @prev_groups = SimpleCov.groups
      SimpleCov.groups = {}
      @prev_formatter = SimpleCov.formatter
      SimpleCov.formatter = nil
    end

    after do
      SimpleCov.filters   = @prev_filters
      SimpleCov.groups    = @prev_groups
      SimpleCov.formatter = @prev_formatter
    end

    let(:original_result) do
      {
        source_fixture("sample.rb") => [nil, 1, 1, 1, nil, nil, 1, 1, nil, nil],
        source_fixture("app/models/user.rb") => [nil, 1, 1, 1, nil, nil, 1, 0, nil, nil],
        source_fixture("app/controllers/sample_controller.rb") => [nil, 1, 1, 1, nil, nil, 1, 0, nil, nil],
      }
    end

    context "a simple cov result initialized from that" do
      subject { SimpleCov::Result.new(original_result) }

      it "has 3 filenames" do
        expect(subject.filenames.count).to eq(3)
      end

      it "has 3 source files" do
        expect(subject.source_files.count).to eq(3)
        subject.source_files.each do |source_file|
          expect(source_file).to be_a SimpleCov::SourceFile
        end
      end

      it "returns an instance of SimpleCov::FileList for source_files and files" do
        expect(subject.files).to be_a SimpleCov::FileList
        expect(subject.source_files).to be_a SimpleCov::FileList
      end

      it "has files equal to source_files" do
        expect(subject.files).to eq(subject.source_files)
      end

      it "has accurate covered percent" do
        # in our fixture, there are 13 covered line (result in 1) in all 15 relevant line (result in non-nil)
        expect(subject.covered_percent).to eq(86.66666666666667)
      end

      it "has accurate covered percentages" do
        expect(subject.covered_percentages).to eq([80.0, 80.0, 100.0])
      end

      it "has accurate least covered file" do
        expect(subject.least_covered_file).to match(/sample_controller.rb/)
      end

      [:covered_percent, :covered_percentages, :least_covered_file, :covered_strength, :covered_lines, :missed_lines, :total_lines].each do |msg|
        it "responds to #{msg}" do
          expect(subject).to respond_to(msg)
        end
      end

      context "dumped with to_hash" do
        it "is a hash" do
          expect(subject.to_hash).to be_a Hash
        end

        context "loaded back with from_hash" do
          let(:dumped_result) do
            SimpleCov::Result.from_hash(subject.to_hash)
          end

          it "has 3 source files" do
            expect(dumped_result.source_files.count).to eq(subject.source_files.count)
          end

          it "has the same covered_percent" do
            expect(dumped_result.covered_percent).to eq(subject.covered_percent)
          end

          it "has the same covered_percentages" do
            expect(dumped_result.covered_percentages).to eq(subject.covered_percentages)
          end

          it "has the same timestamp" do
            expect(dumped_result.created_at.to_i).to eq(subject.created_at.to_i)
          end

          it "has the same command_name" do
            expect(dumped_result.command_name).to eq(subject.command_name)
          end

          it "has the same original_result" do
            expect(dumped_result.original_result).to eq(subject.original_result)
          end
        end
      end
    end

    context "with some filters set up" do
      before do
        SimpleCov.add_filter "sample.rb"
      end

      it "has 2 files in a new simple cov result" do
        expect(SimpleCov::Result.new(original_result).source_files.length).to eq(2)
      end

      it "has 80 covered percent" do
        expect(SimpleCov::Result.new(original_result).covered_percent).to eq(80)
      end

      it "has [80.0, 80.0] covered percentages" do
        expect(SimpleCov::Result.new(original_result).covered_percentages).to eq([80.0, 80.0])
      end
    end

    context "with groups set up for all files" do
      before do
        SimpleCov.add_group "Models", "app/models"
        SimpleCov.add_group "Controllers", ["app/controllers"]
        SimpleCov.add_group "Other" do |src_file|
          File.basename(src_file.filename) == "sample.rb"
        end
      end

      subject do
        SimpleCov::Result.new(original_result)
      end

      it "has 3 groups" do
        expect(subject.groups.length).to eq(3)
      end

      it "has user.rb in 'Models' group" do
        expect(File.basename(subject.groups["Models"].first.filename)).to eq("user.rb")
      end

      it "has sample_controller.rb in 'Controllers' group" do
        expect(File.basename(subject.groups["Controllers"].first.filename)).to eq("sample_controller.rb")
      end

      context "and simple formatter being used" do
        before do
          SimpleCov.formatter = SimpleCov::Formatter::SimpleFormatter
        end

        it "returns a formatted string with result.format!" do
          expect(subject.format!).to be_a String
        end
      end

      context "and multi formatter being used" do
        before do
          SimpleCov.formatters = [
            SimpleCov::Formatter::SimpleFormatter,
            SimpleCov::Formatter::SimpleFormatter,
          ]
        end

        it "returns an array containing formatted string with result.format!" do
          formatted = subject.format!
          expect(formatted.count).to eq(2)
          expect(formatted.first).to be_a String
        end
      end
    end

    context "with groups set up that do not match all files" do
      before do
        SimpleCov.configure do
          add_group "Models", "app/models"
          add_group "Controllers", "app/controllers"
        end
      end

      subject { SimpleCov::Result.new(original_result) }

      it "has 3 groups" do
        expect(subject.groups.length).to eq(3)
      end

      it "has 1 item per group" do
        subject.groups.each_value do |files|
          expect(files.length).to eq(1)
        end
      end

      it 'has sample.rb in "Ungrouped" group' do
        expect(File.basename(subject.groups["Ungrouped"].first.filename)).to eq("sample.rb")
      end

      it "returns all groups as instances of SimpleCov::FileList" do
        subject.groups.each_value do |files|
          expect(files).to be_a SimpleCov::FileList
        end
      end
    end
  end
end if SimpleCov.usable?
