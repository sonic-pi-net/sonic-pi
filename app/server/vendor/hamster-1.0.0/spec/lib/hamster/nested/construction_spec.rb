require "spec_helper"
require "hamster/nested"
require "set"

describe Hamster do
  describe ".from" do
    context "with deep Ruby structure" do
      let(:ruby_obj) {
        { "a" => 1,
          "b" => [2, {"c" => 3}, 4],
          "d" => ::Set.new([5, 6, 7]),
          "e" => {"f" => 8, "g" => 9},
          "h" => Regexp.new("ijk") }
      }

      it "creates nested hamster objects" do
        result = Hamster.from(ruby_obj)
        result.should eql(Hamster::Hash["a" => 1,
                                        "b" => Hamster::Vector[2, Hamster::Hash["c" => 3], 4],
                                        "d" => Hamster::Set[5, 6, 7],
                                        "e" => Hamster::Hash["f" => 8, "g" => 9],
                                        "h" => Regexp.new("ijk")])
      end
    end

    [
      # [input, expected_result]
      [ {}, Hamster::Hash[] ],
      [ {"a" => 1, "b" => 2, "c" => 3}, Hamster::Hash["a" => 1, "b" => 2, "c" => 3] ],
      [ [], Hamster::Vector[] ],
      [ [1, 2, 3], Hamster::Vector[1, 2, 3] ],
      [ ::Set.new, Hamster::Set[] ],
      [ ::Set.new([1, 2, 3]), Hamster::Set[1, 2, 3] ],
      [ 42, 42 ],
      [ STDOUT, STDOUT ]
    ].each do |input, expected_result|
      context "with #{input.inspect} as input" do
        it "should return #{expected_result.inspect}" do
          Hamster.from(input).should eql(expected_result)
        end
      end
    end
  end
end
