require "helper"

class UniMIDI::ClassMethodsTest < Test::Unit::TestCase

  context "ClassMethods" do

    context "#first" do

      context "no block given" do
        should "return first input" do
          i = UniMIDI::Input.first
          assert_equal(UniMIDI::Input.all.first, i)
        end
      end

      context "block given" do
        should "pass and return first input" do
          sleep(1)
          i = UniMIDI::Input.first do |i|
            assert_equal(true, i.enabled?)
          end
          assert_equal(UniMIDI::Input.all.first, i)
        end

      end
    end

    context "#last" do

      context "no block given" do
        should "return last input" do
          i = UniMIDI::Input.last
          assert_equal(UniMIDI::Input.all.last, i)    
        end
      end

      context "block given" do
        should "pass and return last input" do
          sleep(1)
          i = UniMIDI::Input.last do |i|
            assert_equal(true, i.enabled?)     
          end
          assert_equal(UniMIDI::Input.all.last, i)     
        end

      end
    end

    context "#[]" do

      should "return correct input" do
        i = UniMIDI::Input[0]
        assert_equal(UniMIDI::Input.first, i)
        assert_equal(UniMIDI::Input.all.first, i)
      end

    end

    context "#use" do

      context "block given" do
        should "return and enable an input" do
          sleep(1)
          i = UniMIDI::Input.use(0) do |i|
            assert_equal(true, i.enabled?) 
          end
          assert_equal(UniMIDI::Input.first, i)
          assert_equal(UniMIDI::Input.all.first, i)    
        end

      end

      context "with symbol" do

        should "return an enabled input" do
          sleep(1)
          input = UniMIDI::Input.use(:first)
          assert_equal(true, input.enabled?) 
          assert_equal(UniMIDI::Input.first, input)
          assert_equal(UniMIDI::Input.all.first, input)       
        end

      end

    end

    context "#all" do
      should "return all devices" do
        assert_equal(UniMIDI::Loader.devices(:direction => :input), UniMIDI::Input.all)
      end
    end

  end

end
