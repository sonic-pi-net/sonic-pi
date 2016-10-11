require "helper"

describe "merge helpers" do
  describe "with two faked coverage resultsets" do
    before do
      SimpleCov.use_merging true
      @resultset1 = {
        source_fixture("sample.rb") => [nil, 1, 1, 1, nil, nil, 1, 1, nil, nil],
        source_fixture("app/models/user.rb") => [nil, 1, 1, 1, nil, nil, 1, 0, nil, nil],
        source_fixture("app/controllers/sample_controller.rb") => [nil, 1, 1, 1, nil, nil, 1, 0, nil, nil],
        source_fixture("resultset1.rb") => [1, 1, 1, 1],
        source_fixture("parallel_tests.rb") => [nil, 0, nil, 0],
        source_fixture("conditionally_loaded_1.rb") => [nil, 0, 1],  # loaded only in the first resultset
      }.extend(SimpleCov::HashMergeHelper)

      @resultset2 = {
        source_fixture("sample.rb") => [1, nil, 1, 1, nil, nil, 1, 1, nil, nil],
        source_fixture("app/models/user.rb") => [nil, 1, 5, 1, nil, nil, 1, 0, nil, nil],
        source_fixture("app/controllers/sample_controller.rb") => [nil, 3, 1, nil, nil, nil, 1, 0, nil, nil],
        source_fixture("resultset2.rb") => [nil, 1, 1, nil],
        source_fixture("parallel_tests.rb") => [nil, nil, 0, 0],
        source_fixture("conditionally_loaded_2.rb") => [nil, 0, 1],  # loaded only in the second resultset
      }
    end

    context "a merge" do
      subject do
        @resultset1.merge_resultset(@resultset2)
      end

      it "has proper results for sample.rb" do
        expect(subject[source_fixture("sample.rb")]).to eq([1, 1, 2, 2, nil, nil, 2, 2, nil, nil])
      end

      it "has proper results for user.rb" do
        expect(subject[source_fixture("app/models/user.rb")]).to eq([nil, 2, 6, 2, nil, nil, 2, 0, nil, nil])
      end

      it "has proper results for sample_controller.rb" do
        expect(subject[source_fixture("app/controllers/sample_controller.rb")]).to eq([nil, 4, 2, 1, nil, nil, 2, 0, nil, nil])
      end

      it "has proper results for resultset1.rb" do
        expect(subject[source_fixture("resultset1.rb")]).to eq([1, 1, 1, 1])
      end

      it "has proper results for resultset2.rb" do
        expect(subject[source_fixture("resultset2.rb")]).to eq([nil, 1, 1, nil])
      end

      it "has proper results for parallel_tests.rb" do
        expect(subject[source_fixture("parallel_tests.rb")]).to eq([nil, nil, nil, 0])
      end

      it "has proper results for conditionally_loaded_1.rb" do
        expect(subject[source_fixture("conditionally_loaded_1.rb")]).to eq([nil, 0, 1])
      end

      it "has proper results for conditionally_loaded_2.rb" do
        expect(subject[source_fixture("conditionally_loaded_2.rb")]).to eq([nil, 0, 1])
      end
    end

    # See Github issue #6
    it "returns an empty hash when the resultset cache file is empty" do
      File.open(SimpleCov::ResultMerger.resultset_path, "w+") { |f| f.puts "" }
      expect(SimpleCov::ResultMerger.resultset).to be_empty
    end

    # See Github issue #6
    it "returns an empty hash when the resultset cache file is not present" do
      system "rm #{SimpleCov::ResultMerger.resultset_path}" if File.exist?(SimpleCov::ResultMerger.resultset_path)
      expect(SimpleCov::ResultMerger.resultset).to be_empty
    end

    context "and results generated from those" do
      before do
        system "rm #{SimpleCov::ResultMerger.resultset_path}" if File.exist?(SimpleCov::ResultMerger.resultset_path)
        @result1 = SimpleCov::Result.new(@resultset1)
        @result1.command_name = "result1"
        @result2 = SimpleCov::Result.new(@resultset2)
        @result2.command_name = "result2"
      end

      context "with stored results" do
        before do
          SimpleCov::ResultMerger.store_result(@result1)
          SimpleCov::ResultMerger.store_result(@result2)
        end

        it "has stored data in resultset_path JSON file" do
          expect(File.readlines(SimpleCov::ResultMerger.resultset_path).length).to be > 50
        end

        it "returns a hash containing keys ['result1' and 'result2'] for resultset" do
          expect(SimpleCov::ResultMerger.resultset.keys.sort).to eq %w(result1 result2)
        end

        it "returns proper values for merged_result" do
          expect(SimpleCov::ResultMerger.merged_result.source_files.find { |s| s.filename =~ /user/ }.lines.map(&:coverage)).to eq([nil, 2, 6, 2, nil, nil, 2, 0, nil, nil])
        end

        context "with second result way above the merge_timeout" do
          before do
            @result2.created_at = Time.now - 172_800 # two days ago
            SimpleCov::ResultMerger.store_result(@result2)
          end

          it "has only one result in SimpleCov::ResultMerger.results" do
            expect(SimpleCov::ResultMerger.results.length).to eq(1)
          end
        end

        context "with merging disabled" do
          before { SimpleCov.use_merging false }

          it "returns nil for SimpleCov.result" do
            expect(SimpleCov.result).to be_nil
          end
        end
      end
    end
  end
end if SimpleCov.usable?
