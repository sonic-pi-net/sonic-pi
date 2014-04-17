require File.join( File.dirname(__FILE__) , '..', 'spec_helper' )

describe OSC::AddressPattern do
  # it "..." do
  #   ap = OSC::AddressPattern.new("/bob/test/**")

  #   ap.match?("/bob/test/monkey").should be_true
  #   ap.match?("/bob/test/monkey/shine/rainy/day").should be_true

  #   ap.match?("/bob/test").should be_false
  #   ap.match?("/bob").should be_false

  # end

  it "should match anything if the pattern is nil" do
    ap = OSC::AddressPattern.new( nil )

    ap.match?( "/some/nonsense").should be_true
    ap.match?( "/completely.different").should be_true
  end

  it "should match based on a regex" do
    ap = OSC::AddressPattern.new( /hi/ )

    ap.match?( '/hi' ).should be_true
    ap.match?( '/hidden' ).should be_true

    ap.match?( '/bye' ).should be_false
  end

  it "should return a regex if the pattern is a string" do
    ap = OSC::AddressPattern.new( "/hi" )

    ap.match?('/hi').should be_true

    ap.match?('   /hi').should be_false
    ap.match?('/ahi').should be_false
    ap.match?( '/hidden' ).should be_false
    ap.match?( '/bye' ).should be_false
  end

  it "should match with question mark" do
    ap = OSC::AddressPattern.new( "/h?l" )

    ap.match?('/hal').should be_true
    ap.match?('/hel').should be_true
    ap.match?('/hil').should be_true
    ap.match?('/hol').should be_true
    ap.match?('/hul').should be_true
    ap.match?('/hub').should be_false
  end

  it "should match with *" do
    ap = OSC::AddressPattern.new( "/believ*d" )

    ap.match?('/believd').should be_true
    ap.match?('/believed').should be_true
    ap.match?('/believeeed').should be_true
    ap.match?('/believaeeeioud').should be_true
    ap.match?('/believaeeeioud').should be_true
  end

  it "should match with []" do
    ap = OSC::AddressPattern.new( "/believ[aeiou]d" )

    ap.match?('/believad').should be_true
    ap.match?('/believed').should be_true
    ap.match?('/believid').should be_true
    ap.match?('/believod').should be_true
    ap.match?('/believud').should be_true
    ap.match?('/believkd').should be_false
  end

  it "should match with [!]" do
    ap = OSC::AddressPattern.new( "/believ[!aeiou]d" )

    ap.match?('/believad').should be_false
    ap.match?('/believed').should be_false
    ap.match?('/believid').should be_false
    ap.match?('/believod').should be_false
    ap.match?('/believud').should be_false
    ap.match?('/believkd').should be_true
    ap.match?('/believzd').should be_true
  end

  it "should match with {}" do
    ap = OSC::AddressPattern.new( "/{hi,bye}" )

    ap.match?('/hi').should be_true
    ap.match?('/bye').should be_true
    ap.match?('/greetings').should be_false
  end

end