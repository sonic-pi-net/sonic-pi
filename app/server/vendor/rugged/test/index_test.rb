require "test_helper"
require 'base64'
require 'tempfile'
require 'fileutils'

class IndexTest < Rugged::TestCase
  def self.new_index_entry
      now = Time.now
      {
        :path => "new_path",
        :oid => "d385f264afb75a56a5bec74243be9b367ba4ca08",
        :mtime => now,
        :ctime => now,
        :file_size => 1000,
        :dev => 234881027,
        :ino => 88888,
        :mode => 33199,
        :uid => 502,
        :gid => 502,
        :stage => 3,
      }
  end

  def setup
    path = File.dirname(__FILE__) + '/fixtures/testrepo.git/index'
    @index = Rugged::Index.new(path)
  end

  def test_iteration
    enum = @index.each
    assert enum.kind_of? Enumerable

    i = 0
    @index.each { |e| i += 1 }
    assert_equal @index.count, i
  end

  def test_index_size
    assert_equal 2, @index.count
  end

  def test_empty_index
    @index.clear
    assert_equal 0, @index.count
  end

  def test_remove_entries
    @index.remove 'new.txt'
    assert_equal 1, @index.count
  end

  def test_remove_dir
    @index.remove_dir 'does-not-exist'
    assert_equal 2, @index.count

    @index.remove_dir '', 2
    assert_equal 2, @index.count

    @index.remove_dir ''
    assert_equal 0, @index.count
  end

  def test_get_entry_data
    e = @index[0]
    assert_equal 'README', e[:path]
    assert_equal '1385f264afb75a56a5bec74243be9b367ba4ca08', e[:oid]
    assert_equal 1273360380, e[:mtime].to_i
    assert_equal 1273360380, e[:ctime].to_i
    assert_equal 4, e[:file_size]
    assert_equal 234881026, e[:dev]
    assert_equal 6674088, e[:ino]
    assert_equal 33188, e[:mode]
    assert_equal 501, e[:uid]
    assert_equal 0, e[:gid]
    assert_equal false, e[:valid]
    assert_equal 0, e[:stage]

    e = @index[1]
    assert_equal 'new.txt', e[:path]
    assert_equal 'fa49b077972391ad58037050f2a75f74e3671e92', e[:oid]
  end

  def test_iterate_entries
    itr_test = @index.sort { |a, b| a[:oid] <=> b[:oid] }.map { |e| e[:path] }.join(':')
    assert_equal "README:new.txt", itr_test
  end

  def test_update_entries
    now = Time.at Time.now.to_i
    e = @index[0]

    e[:oid] = "12ea3153a78002a988bb92f4123e7e831fd1138a"
    e[:mtime] = now
    e[:ctime] = now
    e[:file_size] = 1000
    e[:dev] = 234881027
    e[:ino] = 88888
    e[:mode] = 33199
    e[:uid] = 502
    e[:gid] = 502
    e[:stage] = 3

    @index.add(e)
    new_e = @index.get e[:path], 3

    assert_equal e, new_e
  end

  def test_add_new_entries
    e = IndexTest.new_index_entry
    @index << e
    assert_equal 3, @index.count
    itr_test = @index.sort { |a, b| a[:oid] <=> b[:oid] }.map { |x| x[:path] }.join(':')
    assert_equal "README:new_path:new.txt", itr_test
  end
end

class IndexWriteTest < Rugged::TestCase
  def setup
    path = File.dirname(__FILE__) + '/fixtures/testrepo.git/index'

    @tmpfile = Tempfile.new('index', Dir.tmpdir, encoding: "binary")
    @tmpfile.write(File.binread(path))
    @tmpfile.close

    @index = Rugged::Index.new(@tmpfile.path)
  end

  def teardown
    @tmpfile.unlink
  end

  def test_raises_when_writing_invalid_entries
    assert_raises TypeError do
      @index.add(21)
    end
  end

  def test_can_write_index
    e = IndexTest.new_index_entry
    @index << e

    e[:path] = "else.txt"
    @index << e

    @index.write

    index2 = Rugged::Index.new(@tmpfile.path)

    itr_test = index2.sort { |a, b| a[:oid] <=> b[:oid] }.map { |x| x[:path] }.join(':')
    assert_equal "README:else.txt:new_path:new.txt", itr_test
    assert_equal 4, index2.count
  end
end

class IndexWorkdirTest < Rugged::TestCase
  def setup
    @tmppath = Dir.mktmpdir
    @repo = Rugged::Repository.init_at(@tmppath, false)
    @index = @repo.index
  end

  def teardown
    @repo.close
    FileUtils.remove_entry_secure(@tmppath)
  end

  def test_adding_a_path
    File.open(File.join(@tmppath, 'test.txt'), 'w') do |f|
      f.puts "test content"
    end
    @index.add('test.txt')
    @index.write

    index2 = Rugged::Index.new(@tmppath + '/.git/index')
    assert_equal index2[0][:path], 'test.txt'
  end

  def test_reloading_index
    File.open(File.join(@tmppath, 'test.txt'), 'w') do |f|
      f.puts "test content"
    end
    @index.add('test.txt')
    @index.write

    rindex = Rugged::Index.new(File.join(@tmppath, '/.git/index'))
    e = rindex['test.txt']
    assert_equal 0, e[:stage]

    rindex << IndexTest.new_index_entry
    rindex.write

    assert_equal 1, @index.count
    @index.reload
    assert_equal 2, @index.count

    e = @index.get 'new_path', 3
    assert_equal e[:mode], 33199
  end
end

class IndexConflictsTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init("mergedrepo")
  end

  def teardown
    @repo.close

    super
  end

  def test_conflicts?
    assert @repo.index.conflicts?
  end

  def test_conflicts
    conflicts = @repo.index.conflicts

    assert_equal 2, conflicts.size

    assert_equal "conflicts-one.txt", conflicts[0][:ancestor][:path]
    assert_equal "conflicts-one.txt", conflicts[0][:ours][:path]
    assert_equal "conflicts-one.txt", conflicts[0][:theirs][:path]
    assert_equal 1, conflicts[0][:ancestor][:stage]
    assert_equal 2, conflicts[0][:ours][:stage]
    assert_equal 3, conflicts[0][:theirs][:stage]

    assert_equal "conflicts-two.txt", conflicts[1][:ancestor][:path]
    assert_equal "conflicts-two.txt", conflicts[1][:ours][:path]
    assert_equal "conflicts-two.txt", conflicts[1][:theirs][:path]
    assert_equal 1, conflicts[1][:ancestor][:stage]
    assert_equal 2, conflicts[1][:ours][:stage]
    assert_equal 3, conflicts[1][:theirs][:stage]
  end

  def test_conflict_get
    conflict = @repo.index.conflict_get("conflicts-one.txt")

    assert_equal "conflicts-one.txt", conflict[:ancestor][:path]
    assert_equal "conflicts-one.txt", conflict[:ours][:path]
    assert_equal "conflicts-one.txt", conflict[:theirs][:path]
    assert_equal 1, conflict[:ancestor][:stage]
    assert_equal 2, conflict[:ours][:stage]
    assert_equal 3, conflict[:theirs][:stage]

    refute @repo.index.conflict_get("conflict-does-not-exists.txt")
  end

  def test_conflict_remove
    @repo.index.conflict_remove("conflicts-one.txt")
    assert_equal @repo.index.conflicts.size, 1

    @repo.index.conflict_remove("conflicts-two.txt")
    assert_equal @repo.index.conflicts.size, 0

    refute @repo.index.conflicts?
  end

  def test_conflict_add
    conflict = @repo.index.conflict_get("conflicts-one.txt")

    conflict[:ancestor][:path] = conflict[:ours][:path] = conflict[:theirs][:path] = "new-conflict.txt"
    @repo.index.conflict_add(conflict)

    assert_equal @repo.index.conflicts.size, 3

    conflict[:ancestor] = nil
    conflict[:ours][:path] = conflict[:theirs][:path] = "another-new-conflict.txt"

    @repo.index.conflict_add(conflict)

    assert_equal @repo.index.conflicts.size, 4
  end

  def test_conflict_cleanup
    @repo.index.conflict_cleanup

    assert_equal @repo.index.conflicts.size, 0
    refute @repo.index.conflicts?
  end
end

class IndexMergeFileTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = sandbox_init("mergedrepo")
  end

  def teardown
    @repo.close

    super
  end

  def test_merge_file
    merge_file_result = @repo.index.merge_file("conflicts-one.txt")

    assert !merge_file_result[:automergeable]
    assert_equal merge_file_result[:path], "conflicts-one.txt"
    assert_equal merge_file_result[:data], "<<<<<<< conflicts-one.txt\nThis is most certainly a conflict!\n=======\nThis is a conflict!!!\n>>>>>>> conflicts-one.txt\n"
  end

end

class IndexRepositoryTest < Rugged::TestCase
  include Rugged::TempRepositoryAccess

  def test_idempotent_read_write
    head_sha = @repo.references['HEAD'].resolve.target_id
    tree = @repo.lookup(head_sha).tree
    index = @repo.index
    index.read_tree(tree)

    index_tree_sha = index.write_tree
    index_tree = @repo.lookup(index_tree_sha)
    assert_equal tree.oid, index_tree.oid
  end

  def test_build_tree_from_index
    head_sha = @repo.references['refs/remotes/origin/packed'].resolve.target_id
    tree = @repo.lookup(head_sha).tree

    index = @repo.index
    index.read_tree(tree)
    index.remove('second.txt')

    new_tree_sha = index.write_tree
    assert head_sha != new_tree_sha
    assert_nil @repo.lookup(new_tree_sha)['second.txt']
  end
end

class IndexAddAllTest < Rugged::SandboxedTestCase
  def setup
    super

    @repo = Rugged::Repository.init_at(File.join(@_sandbox_path, "add-all"))

    Dir.chdir(@repo.workdir) do
      File.open("file.foo", "w") { |f| f.write "a file" }
      File.open("file.bar", "w") { |f| f.write "another file" }
      File.open("file.zzz", "w") { |f| f.write "yet another one" }
      File.open("other.zzz", "w") { |f| f.write "yet another one" }
      File.open("more.zzz", "w") { |f| f.write "yet another one" }
      File.open(".gitignore", "w") { |f| f.write "*.foo\n" }
    end
  end

  def teardown
    @repo.close

    super
  end

  def test_add_all_lifecycle
    Dir.chdir(@repo.workdir) do
      @repo.index.add_all("file.*")

      assert @repo.index["file.bar"]
      assert @repo.index["file.zzz"]
      refute @repo.index["file.foo"]
      refute @repo.index["other.zzz"]
      refute @repo.index["more.zzz"]

      @repo.index.add_all("*.zzz")

      assert @repo.index["file.bar"]
      assert @repo.index["file.zzz"]
      assert @repo.index["other.zzz"]
      assert @repo.index["more.zzz"]
      refute @repo.index["file.foo"]
    end
  end

  def test_add_all_dry_run
    Dir.chdir(@repo.workdir) do
      yielded = []
      @repo.index.add_all do |path, pathspec|
        yielded << path
        false
      end

      assert_equal [".gitignore", "file.bar", "file.zzz", "more.zzz", "other.zzz"], yielded

      yielded.each do |path|
        refute @repo.index[path]
      end

      yielded = []
      @repo.index.add_all(["file.*", "*.zzz"]) do |path, pathspec|
        yielded << [path, pathspec]
        false
      end

      assert_equal [
        ["file.bar", "file.*"],
        ["file.zzz", "file.*"],
        ["more.zzz", "*.zzz"],
        ["other.zzz", "*.zzz"]
      ], yielded
    end
  end

  def test_update_all
    Dir.chdir(@repo.workdir) do
      @repo.index.add_all("file.*")

      File.open("file.bar", "w") { |f| f.write "new content for file" }
      @repo.index.update_all("file.*")

      assert @repo.index["file.bar"]
      assert_equal "new content for file", @repo.lookup(@repo.index["file.bar"][:oid]).content

      refute @repo.index["other.zzz"], "#update_all should only update files in the index"
      refute @repo.index["more.zzz"], "#update_all should only update files in the index"

      File.unlink("file.bar")
      @repo.index.update_all

      refute @repo.index["file.bar"], "#update_all should remove index entries that are removed from the workdir"
    end
  end

  def test_remove_all
    Dir.chdir(@repo.workdir) do
      @repo.index.add_all("file.*")
      @repo.index.remove_all("*.zzz")

      assert @repo.index["file.bar"]
      refute @repo.index["file.zzz"]
    end
  end

end
