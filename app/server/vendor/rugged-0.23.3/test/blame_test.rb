require "test_helper"
require "time"

class BlameTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("testrepo")
    @blame = Rugged::Blame.new(@repo, "branch_file.txt")
  end

  def test_blame_index
    assert_equal 2, @blame.count

    assert_equal({
      :lines_in_hunk => 1,
      :final_commit_id => "c47800c7266a2be04c571c04d5a6614691ea99bd",
      :final_start_line_number => 1,
      :final_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2010-05-25 11:58:14 -0700")
      },
      :orig_commit_id => "c47800c7266a2be04c571c04d5a6614691ea99bd",
      :orig_path => "branch_file.txt",
      :orig_start_line_number => 1,
      :orig_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2010-05-25 11:58:14 -0700")
      },
      :boundary => false
    }, @blame[0])

    assert_equal({
      :lines_in_hunk => 1,
      :final_commit_id => "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
      :final_start_line_number => 2,
      :final_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2011-08-09 19:33:46 -0700")
      },
      :orig_commit_id => "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
      :orig_path => "branch_file.txt",
      :orig_start_line_number => 2,
      :orig_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2011-08-09 19:33:46 -0700")
      },
      :boundary => false
    }, @blame[1])

    assert_equal @blame[1], @blame[-1]
    assert_equal @blame[0], @blame[-2]

    assert_nil @blame[-3]
  end

  def test_blame_options
    blame = Rugged::Blame.new(@repo, "branch_file.txt", max_line: 1)
    assert_equal 1, blame.count

    assert_equal({
      :lines_in_hunk => 1,
      :final_commit_id => "c47800c7266a2be04c571c04d5a6614691ea99bd",
      :final_start_line_number => 1,
      :final_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2010-05-25 11:58:14 -0700")
      },
      :orig_commit_id => "c47800c7266a2be04c571c04d5a6614691ea99bd",
      :orig_path => "branch_file.txt",
      :orig_start_line_number => 1,
      :orig_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2010-05-25 11:58:14 -0700")
      },
      :boundary => false
    }, blame[0])


    blame = Rugged::Blame.new(@repo, "branch_file.txt", min_line: 2)
    assert_equal 1, blame.count

    assert_equal({
      :lines_in_hunk => 1,
      :final_commit_id => "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
      :final_start_line_number => 2,
      :final_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2011-08-09 19:33:46 -0700")
      },
      :orig_commit_id => "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
      :orig_path => "branch_file.txt",
      :orig_start_line_number => 2,
      :orig_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2011-08-09 19:33:46 -0700")
      },
      :boundary => false
    }, blame[0])
  end

  def test_blame_for_line
    assert_equal({
      :lines_in_hunk => 1,
      :final_commit_id => "c47800c7266a2be04c571c04d5a6614691ea99bd",
      :final_start_line_number => 1,
      :final_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2010-05-25 11:58:14 -0700")
      },
      :orig_commit_id => "c47800c7266a2be04c571c04d5a6614691ea99bd",
      :orig_path => "branch_file.txt",
      :orig_start_line_number => 1,
      :orig_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2010-05-25 11:58:14 -0700")
      },
      :boundary => false
    }, @blame.for_line(1))

    assert_equal({
      :lines_in_hunk => 1,
      :final_commit_id => "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
      :final_start_line_number => 2,
      :final_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2011-08-09 19:33:46 -0700")
      },
      :orig_commit_id => "a65fedf39aefe402d3bb6e24df4d4f5fe4547750",
      :orig_path => "branch_file.txt",
      :orig_start_line_number => 2,
      :orig_signature => {
        :name => "Scott Chacon",
        :email => "schacon@gmail.com",
        :time => Time.parse("2011-08-09 19:33:46 -0700")
      },
      :boundary => false
    }, @blame.for_line(2))
  end

  def test_blame_with_invalid_line
    assert_nil @blame.for_line(0)
    assert_nil @blame.for_line(1000000)

    assert_raises ArgumentError do
      @blame.for_line(-1)
    end
  end

  def test_each
    hunks = []
    @blame.each do |hunk|
      hunks << hunk
    end

    assert_equal 2, hunks.count
    assert_equal @blame[0], hunks[0]
    assert_equal @blame[1], hunks[1]
  end
end
