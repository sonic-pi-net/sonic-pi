require "minitest_helper"

describe Tomlrb::Key do
  subject { Tomlrb::Keys.new }

  it "can define a super-table afterward" do
    subject.add_table_key %w[x y z w]
    subject.add_table_key %w[x]
    assert true
  end

  it "cannot define a table more than once" do
    subject.add_table_key %w[fruit]
    subject.add_pair_key %w[apple], %w[fruit]
    _{ subject.add_table_key %w[fruit] }.must_raise Tomlrb::Key::KeyConflict
  end

  it "cannot define a table more than once" do
    subject.add_table_key %w[fruit]
    subject.add_pair_key %w[apple], %w[fruit]
    _{ subject.add_table_key %w[fruit apple] }.must_raise Tomlrb::Key::KeyConflict
  end

  it "can define tables out of order" do
    subject.add_table_key %w[fruit apple]
    subject.add_table_key %w[animal]
    subject.add_table_key %w[fruit orange]
    assert true
  end

  it "is recommended to define tables in order" do
    subject.add_table_key %w[fruit apple]
    subject.add_table_key %w[fruit orange]
    subject.add_table_key %w[animal]
    assert true
  end

  it "can define sub-tables within tables defined via dotted keys" do
    subject.add_table_key %w[fruit]
    subject.add_pair_key %w[apple color], %w[fruit]
    subject.add_pair_key %w[apple taste sweet], %w[fruit]
    subject.add_table_key %w[fruit apple texture]
    subject.add_pair_key %w[smooth], %w[fruit apple texture]
    assert true
  end

  it "cannot define table key already defined as pair key" do
    subject.add_table_key %w[fruit]
    subject.add_pair_key %w[apple color], %w[fruit]
    subject.add_pair_key %w[apple taste sweet], %w[fruit]
   _{ subject.add_table_key %w[fruit apple] }.must_raise Tomlrb::Key::KeyConflict
  end

  it "cannot define table key already defined as pair key" do
    subject.add_table_key %w[fruit]
    subject.add_pair_key %w[apple color], %w[fruit]
    subject.add_pair_key %w[apple taste sweet], %w[fruit]
   _{ subject.add_table_key %w[fruit apple taste] }.must_raise Tomlrb::Key::KeyConflict
  end
end
