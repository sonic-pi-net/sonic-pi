require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  [:empty?, :null?].each do |method|

    describe "##{method}" do

      [
        [[], true],
        [["A" => "aye"], false],
        [["A" => "aye", "B" => "bee", "C" => "see"], false],
      ].each do |pairs, result|

        it "returns #{result} for #{pairs.inspect}" do
          Hamster.hash(*pairs).send(method).should == result
        end

      end

    end

  end

end
