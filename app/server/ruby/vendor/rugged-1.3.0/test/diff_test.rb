require "test_helper"

class PatchFromStringsTest < Rugged::TestCase
  def test_from_strings_no_args
    patch = Rugged::Patch.from_strings()
    assert_equal 0, patch.size
    assert_equal "", patch.to_s
  end

  def test_from_strings_create_file
    patch = Rugged::Patch.from_strings(nil, "added\n")
    assert_equal 1, patch.size
    assert_equal <<-EOS, patch.to_s
diff --git a/file b/file
new file mode 100644
index 0000000..d5f7fc3
--- /dev/null
+++ b/file
@@ -0,0 +1 @@
+added
EOS
  end

  def test_from_strings_delete_file
    patch = Rugged::Patch.from_strings("deleted\n", nil)
    assert_equal 1, patch.size
    assert_equal <<-EOS, patch.to_s
diff --git a/file b/file
deleted file mode 100644
index 71779d2..0000000
--- a/file
+++ /dev/null
@@ -1 +0,0 @@
-deleted
EOS
  end

  def test_from_strings_without_paths
    patch = Rugged::Patch.from_strings("deleted\n", "added\n")
    assert_equal 1, patch.size
    assert_equal <<-EOS, patch.to_s
diff --git a/file b/file
index 71779d2..d5f7fc3 100644
--- a/file
+++ b/file
@@ -1 +1 @@
-deleted
+added
EOS
  end

  def test_from_strings_with_custom_paths
    patch = Rugged::Patch.from_strings("deleted\n", "added\n", old_path: "old", new_path: "new")
    assert_equal 1, patch.size
    assert_equal <<-EOS, patch.to_s
diff --git a/old b/new
index 71779d2..d5f7fc3 100644
--- a/old
+++ b/new
@@ -1 +1 @@
-deleted
+added
EOS
  end
end

class RepoDiffTest < Rugged::TestCase
  def test_new_from_buffer
    repo    = FixtureRepo.from_libgit2("attr")
    patch1  = Rugged::Patch.from_strings("deleted\n", "added\n", old_path: "old", new_path: "new").to_s
    diff1   = repo.diff_from_buffer(patch1)
    assert_equal diff1.patch, patch1
    
    diff2   = repo.diff("605812a", "370fe9ec22", :context_lines => 1, :interhunk_lines => 1)
    patch2  = diff2.patch
    diff3   = repo.diff_from_buffer(patch2)
    assert_equal diff3.patch, patch2
  end
  
  def test_with_oid_string
    repo = FixtureRepo.from_libgit2("attr")
    diff = repo.diff("605812a", "370fe9ec22", :context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten
    bytesize = patches.inject(0) {|n, p| n += p.bytesize(exclude_context: true)}

    assert_equal 5, diff.size
    assert_equal 5, deltas.size
    assert_equal 5, patches.size
    assert_equal 1589, bytesize

    assert_equal 2, deltas.select(&:added?).size
    assert_equal 1, deltas.select(&:deleted?).size
    assert_equal 2, deltas.select(&:modified?).size

    assert_equal 5, hunks.size

    assert_equal((7 + 24 + 1 + 6 + 6), lines.size)
    assert_equal((1), lines.select(&:context?).size)
    assert_equal((24 + 1 + 5 + 5), lines.select(&:addition?).size)
    assert_equal((7 + 1), lines.select(&:deletion?).size)
  end

  def test_delta_status_char
    repo = FixtureRepo.from_libgit2("attr")
    diff = repo.diff("605812a", "370fe9ec22", :context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas

    assert_equal :D, deltas[0].status_char
    assert_equal :A, deltas[1].status_char
    assert_equal :A, deltas[2].status_char
    assert_equal :M, deltas[3].status_char
    assert_equal :M, deltas[4].status_char
  end

  def test_with_nil_on_left_side
    repo = FixtureRepo.from_libgit2("attr")
    diff = repo.diff("605812a", nil, :context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 16, diff.size
    assert_equal 16, deltas.size
    assert_equal 16, patches.size

    assert_equal 0, deltas.select(&:added?).size
    assert_equal 16, deltas.select(&:deleted?).size
    assert_equal 0, deltas.select(&:modified?).size

    assert_equal 15, hunks.size

    assert_equal 115, lines.size
    assert_equal 0, lines.select(&:context?).size
    assert_equal 0, lines.select(&:addition?).size
    assert_equal 113, lines.select(&:deletion?).size
  end

  def test_with_nil_on_right_side
    repo = FixtureRepo.from_libgit2("attr")
    diff = repo.diff(nil, "605812a", :context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 16, diff.size
    assert_equal 16, deltas.size
    assert_equal 16, patches.size

    assert_equal 16, deltas.select(&:added?).size
    assert_equal 0, deltas.select(&:deleted?).size
    assert_equal 0, deltas.select(&:modified?).size

    assert_equal 15, hunks.size

    assert_equal 115, lines.size
    assert_equal 0, lines.select(&:context?).size
    assert_equal 113, lines.select(&:addition?).size
    assert_equal 0, lines.select(&:deletion?).size
  end
end

class RepoWorkdirDiffTest < Rugged::TestCase
  def test_basic_diff
    repo = FixtureRepo.from_libgit2("status")
    diff = repo.diff_workdir("26a125ee1bf",
      :context_lines => 3,
      :interhunk_lines => 1,
      :include_ignored => true,
      :include_untracked => true
    )

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 14, deltas.size
    assert_equal 14, patches.size

    assert_equal 0, deltas.select(&:added?).size
    assert_equal 4, deltas.select(&:deleted?).size
    assert_equal 4, deltas.select(&:modified?).size
    assert_equal 1, deltas.select(&:ignored?).size
    assert_equal 5, deltas.select(&:untracked?).size

    assert_equal 8, hunks.size

    assert_equal 13, lines.size
    assert_equal 4, lines.select(&:context?).size
    assert_equal 5, lines.select(&:addition?).size
    assert_equal 4, lines.select(&:deletion?).size
  end
end

class CommitDiffTest < Rugged::TestCase
  def test_diff_with_parent
    repo = FixtureRepo.from_libgit2("attr")
    commit = Rugged::Commit.lookup(repo, "605812a")

    diff = commit.diff(:context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 5, diff.size
    assert_equal 5, deltas.size
    assert_equal 5, patches.size

    assert_equal 3, deltas.select(&:added?).size
    assert_equal 0, deltas.select(&:deleted?).size
    assert_equal 2, deltas.select(&:modified?).size

    assert_equal 4, hunks.size

    assert_equal(51, lines.size)
    assert_equal(2, lines.select(&:context?).size)
    assert_equal(46, lines.select(&:addition?).size)
    assert_equal(3, lines.select(&:deletion?).size)
  end

  def test_diff_with_parent_no_options
    repo = FixtureRepo.from_libgit2("attr")
    commit = Rugged::Commit.lookup(repo, "605812a")

    diff = commit.diff

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 5, diff.size
    assert_equal 5, deltas.size
    assert_equal 5, patches.size

    assert_equal 3, deltas.select(&:added?).size
    assert_equal 0, deltas.select(&:deleted?).size
    assert_equal 2, deltas.select(&:modified?).size

    assert_equal 4, hunks.size

    assert_equal(53, lines.size)
    assert_equal(4, lines.select(&:context?).size)
    assert_equal(46, lines.select(&:addition?).size)
    assert_equal(3, lines.select(&:deletion?).size)
  end

  def test_diff_with_parent_for_initial_commit
    repo = FixtureRepo.from_libgit2("attr")
    commit = Rugged::Commit.lookup(repo, "6bab5c79cd5140d0f800917f550eb2a3dc32b0da")

    diff = commit.diff(:context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 13, diff.size
    assert_equal 13, deltas.size
    assert_equal 13, patches.size

    assert_equal 13, deltas.select(&:added?).size
    assert_equal 0, deltas.select(&:deleted?).size
    assert_equal 0, deltas.select(&:modified?).size

    assert_equal 13, hunks.size

    assert_equal(72, lines.size)
    assert_equal(0, lines.select(&:context?).size)
    assert_equal(70, lines.select(&:addition?).size)
    assert_equal(0, lines.select(&:deletion?).size)
  end
end

class CommitToWorkdirDiffTest < Rugged::TestCase
  def test_basic_diff
    repo = FixtureRepo.from_libgit2("status")
    a = Rugged::Commit.lookup(repo, "26a125ee1bf")

    diff = a.diff_workdir(
      :context_lines => 3,
      :interhunk_lines => 1,
      :include_ignored => true,
      :include_untracked => true
    )

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 14, deltas.size
    assert_equal 14, patches.size

    assert_equal 0, deltas.select(&:added?).size
    assert_equal 4, deltas.select(&:deleted?).size
    assert_equal 4, deltas.select(&:modified?).size
    assert_equal 1, deltas.select(&:ignored?).size
    assert_equal 5, deltas.select(&:untracked?).size

    assert_equal 8, hunks.size

    assert_equal 13, lines.size
    assert_equal 4, lines.select(&:context?).size
    assert_equal 5, lines.select(&:addition?).size
    assert_equal 4, lines.select(&:deletion?).size
  end
end

class TreeToWorkdirDiffTest < Rugged::TestCase
  def test_basic_diff
    repo = FixtureRepo.from_libgit2("status")
    a = Rugged::Commit.lookup(repo, "26a125ee1bf").tree

    diff = a.diff_workdir(
      :context_lines => 3,
      :interhunk_lines => 1,
      :include_ignored => true,
      :include_untracked => true
    )

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 14, deltas.size
    assert_equal 14, patches.size

    assert_equal 0, deltas.select(&:added?).size
    assert_equal 4, deltas.select(&:deleted?).size
    assert_equal 4, deltas.select(&:modified?).size
    assert_equal 1, deltas.select(&:ignored?).size
    assert_equal 5, deltas.select(&:untracked?).size

    assert_equal 8, hunks.size

    assert_equal 13, lines.size
    assert_equal 4, lines.select(&:context?).size
    assert_equal 5, lines.select(&:addition?).size
    assert_equal 4, lines.select(&:deletion?).size
  end

  def test_diff_merge
    repo = FixtureRepo.from_libgit2("status")
    index = repo.index

    a = Rugged::Commit.lookup(repo, "26a125ee1bf").tree

    # merge diffs to simulate "git diff 26a125ee1bf"

    diff  = a.diff(index, :include_ignored => true, :include_untracked => true)
    diff2 = index.diff(:include_ignored => true, :include_untracked => true)
    diff.merge!(diff2)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    # expected values differ from "git diff --porcelain 26a125ee1bf"
    # because that includes the file "staged_delete_modified_file" twice,
    # once in the deleted list and again the untracked list and libgit2
    # does not do that with a merged diff (though it would with status)

    assert_equal 15, deltas.size
    assert_equal 15, patches.size

    assert_equal 2, deltas.select(&:added?).size
    assert_equal 5, deltas.select(&:deleted?).size
    assert_equal 4, deltas.select(&:modified?).size
    assert_equal 1, deltas.select(&:ignored?).size
    assert_equal 3, deltas.select(&:untracked?).size

    assert_equal 11, hunks.size

    assert_equal 17, lines.size
    assert_equal 4, lines.select(&:context?).size
    assert_equal 8, lines.select(&:addition?).size
    assert_equal 5, lines.select(&:deletion?).size
  end

  def test_stats
    repo = FixtureRepo.from_libgit2("status")
    index = repo.index

    a = Rugged::Commit.lookup(repo, "26a125ee1bf").tree

    # merge diffs to simulate "git diff 26a125ee1bf"

    diff  = a.diff(index, :include_ignored => true, :include_untracked => true)
    diff2 = index.diff(:include_ignored => true, :include_untracked => true)
    diff.merge!(diff2)

    # expected values from: git diff --stat 26a125ee1bf
    files, adds, dels = diff.stat
    assert_equal 11, files
    assert_equal 8, adds
    assert_equal 5, dels

    # expected per-file values from the diff --stat output plus total lines
    expected_patch_stat = [
      [ 0, 1, 1, 1 ], [ 1, 0, 2, 1 ], [ 1, 0, 2, 1 ], [ 0, 1, 1, 1 ], [ 2, 0, 3, 2 ],
      [ 0, 1, 1, 1 ], [ 0, 1, 1, 1 ], [ 1, 0, 1, 1 ], [ 2, 0, 2, 2 ], [ 0, 1, 1, 1 ],
      [ 1, 0, 2, 1 ]
    ]

    diff.each_patch do |patch|
      next if [:unmodified, :ignored, :untracked].include? patch.delta.status

      expected_adds, expected_dels, expected_lines, lines_wo_context = expected_patch_stat.shift

      actual_adds, actual_dels = patch.stat

      assert_equal expected_adds, actual_adds
      assert_equal expected_dels, actual_dels
      assert_equal expected_adds + expected_dels, patch.changes

      assert_equal expected_lines, patch.lines
      assert_equal lines_wo_context, patch.lines(exclude_context: true)
    end
  end
end

class TreeToTreeDiffTest < Rugged::TestCase
  def test_basic_diff
    repo = FixtureRepo.from_libgit2("attr")
    a = Rugged::Commit.lookup(repo, "605812a").tree
    b = Rugged::Commit.lookup(repo, "370fe9ec22").tree
    c = Rugged::Commit.lookup(repo, "f5b0af1fb4f5c").tree

    diff = a.diff(b, :context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 5, diff.size
    assert_equal 5, deltas.size
    assert_equal 5, patches.size

    assert_equal 2, deltas.select(&:added?).size
    assert_equal 1, deltas.select(&:deleted?).size
    assert_equal 2, deltas.select(&:modified?).size

    assert_equal 5, hunks.size

    assert_equal((7 + 24 + 1 + 6 + 6), lines.size)
    assert_equal((1), lines.select(&:context?).size)
    assert_equal((24 + 1 + 5 + 5), lines.select(&:addition?).size)
    assert_equal((7 + 1), lines.select(&:deletion?).size)


    diff = c.diff(b, :context_lines => 1, :interhunk_lines => 1)
    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 2, deltas.size
    assert_equal 2, patches.size

    assert_equal 0, deltas.select(&:added?).size
    assert_equal 0, deltas.select(&:deleted?).size
    assert_equal 2, deltas.select(&:modified?).size

    assert_equal 2, hunks.size

    assert_equal((8 + 15), lines.size)
    assert_equal((1), lines.select(&:context?).size)
    assert_equal((1), lines.select(&:addition?).size)
    assert_equal((7 + 14), lines.select(&:deletion?).size)
  end

  def test_diff_with_empty_tree
    repo = FixtureRepo.from_libgit2("attr")
    a = Rugged::Commit.lookup(repo, "605812a").tree

    diff = a.diff(nil, :context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 16, diff.size
    assert_equal 16, deltas.size
    assert_equal 16, patches.size

    assert_equal 0, deltas.select(&:added?).size
    assert_equal 16, deltas.select(&:deleted?).size
    assert_equal 0, deltas.select(&:modified?).size

    assert_equal 15, hunks.size

    assert_equal 115, lines.size
    assert_equal 0, lines.select(&:context?).size
    assert_equal 0, lines.select(&:addition?).size
    assert_equal 113, lines.select(&:deletion?).size
  end

  def test_diff_with_rev_string
    repo = FixtureRepo.from_libgit2("attr")
    a = Rugged::Commit.lookup(repo, "605812a").tree

    diff = a.diff("370fe9ec22", :context_lines => 1, :interhunk_lines => 1)

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 5, diff.size
    assert_equal 5, deltas.size
    assert_equal 5, patches.size

    assert_equal 2, deltas.select(&:added?).size
    assert_equal 1, deltas.select(&:deleted?).size
    assert_equal 2, deltas.select(&:modified?).size

    assert_equal 5, hunks.size

    assert_equal((7 + 24 + 1 + 6 + 6), lines.size)
    assert_equal((1), lines.select(&:context?).size)
    assert_equal((24 + 1 + 5 + 5), lines.select(&:addition?).size)
    assert_equal((7 + 1), lines.select(&:deletion?).size)
  end

  def test_diff_merge
    repo = FixtureRepo.from_libgit2("attr")
    a = Rugged::Commit.lookup(repo, "605812a").tree
    b = Rugged::Commit.lookup(repo, "370fe9ec22").tree
    c = Rugged::Commit.lookup(repo, "f5b0af1fb4f5c").tree

    diff = a.diff(b).merge!(c.diff(b))

    deltas = diff.deltas
    patches = diff.patches
    hunks = patches.map(&:hunks).flatten
    lines = hunks.map(&:lines).flatten

    assert_equal 6, diff.size
    assert_equal 6, patches.size
    assert_equal 6, deltas.size

    assert_equal 2, deltas.select(&:added?).size
    assert_equal 1, deltas.select(&:deleted?).size
    assert_equal 3, deltas.select(&:modified?).size

    assert_equal 6, hunks.size

    assert_equal 59, lines.size
    assert_equal 1, lines.select(&:context?).size
    assert_equal 36, lines.select(&:addition?).size
    assert_equal 22, lines.select(&:deletion?).size
  end

  def test_symlink_blob_mode_changed_to_regular_file
    repo = FixtureRepo.from_libgit2("unsymlinked.git")

    a = Rugged::Commit.lookup(repo, "7fccd7").tree
    b = Rugged::Commit.lookup(repo, "806999").tree

    diff = a.diff(b)

    deltas = diff.deltas
    patches = diff.patches

    assert_equal 3, diff.size
    assert_equal 3, patches.size
    assert_equal 3, deltas.size

    assert_equal 1, deltas.select(&:added?).size
    assert_equal 2, deltas.select(&:deleted?).size
    assert_equal 0, deltas.select(&:modified?).size
    assert_equal 0, deltas.select(&:typechange?).size
  end

  def test_symlink_blob_mode_changed_to_regular_file_as_typechange
    repo = FixtureRepo.from_libgit2("unsymlinked.git")

    a = Rugged::Commit.lookup(repo, "7fccd7").tree
    b = Rugged::Commit.lookup(repo, "806999").tree

    diff = a.diff(b, :include_typechange => true)

    deltas = diff.deltas
    patches = diff.patches

    assert_equal 2, diff.size
    assert_equal 2, patches.size
    assert_equal 2, deltas.size

    assert_equal 0, deltas.select(&:added?).size
    assert_equal 1, deltas.select(&:deleted?).size
    assert_equal 0, deltas.select(&:modified?).size
    assert_equal 1, deltas.select(&:typechange?).size
  end

  def test_regular_blob_mode_changed_to_executable_file
    repo = FixtureRepo.from_libgit2("unsymlinked.git")

    a = Rugged::Commit.lookup(repo, "806999").tree
    b = Rugged::Commit.lookup(repo, "a8595c").tree

    diff = a.diff(b)

    deltas = diff.deltas
    patches = diff.patches

    assert_equal 1, diff.size
    assert_equal 1, patches.size
    assert_equal 1, deltas.size

    assert_equal 0, deltas.select(&:added?).size
    assert_equal 0, deltas.select(&:deleted?).size
    assert_equal 1, deltas.select(&:modified?).size
    assert_equal 0, deltas.select(&:typechange?).size
  end

  def test_size
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)
    assert_equal 2, diff.size
  end

  def test_each_delta
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    deltas = []
    diff.each_delta do |delta|
      assert_instance_of Rugged::Diff::Delta, delta
      deltas << delta
    end

    assert_equal 2, deltas.size

    assert_equal "another.txt", deltas[0].old_file[:path]
    assert_equal "another.txt", deltas[0].new_file[:path]

    refute deltas[0].binary?

    assert_equal "readme.txt", deltas[1].old_file[:path]
    assert_equal "readme.txt", deltas[1].new_file[:path]

    refute deltas[1].binary?
  end

  def test_diff_treats_files_bigger_as_max_size_as_binary
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree, :max_size => 10)
    assert_equal 2, diff.patches.size
    assert_equal <<-EOS, diff.patch
diff --git a/another.txt b/another.txt
index 3e5bcba..546c735 100644
Binary files a/another.txt and b/another.txt differ
diff --git a/readme.txt b/readme.txt
index 7b808f7..29ab705 100644
Binary files a/readme.txt and b/readme.txt differ
EOS
  end

  def test_contraining_paths
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree, :paths => ["readme.txt"])
    assert_equal "M\treadme.txt\n", diff.patch(:compact => true)

    diff = a.tree.diff(b.tree, :paths => ["r*.txt"])
    assert_equal "M\treadme.txt\n", diff.patch(:compact => true)

    diff = a.tree.diff(b.tree, :paths => ["*.txt"])
    assert_equal "M\tanother.txt\nM\treadme.txt\n", diff.patch(:compact => true)

    diff = a.tree.diff(b.tree, :paths => ["*.txt"], :disable_pathspec_match => true)
    assert_equal "", diff.patch(:compact => true)

    diff = a.tree.diff(b.tree, :paths => ["readme.txt"], :disable_pathspec_match => true)
    assert_equal "M\treadme.txt\n", diff.patch(:compact => true)
  end

  def test_options
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree, :context_lines => 0)

    assert_equal <<-EOS, diff.patch
diff --git a/another.txt b/another.txt
index 3e5bcba..546c735 100644
--- a/another.txt
+++ b/another.txt
@@ -2 +2 @@ Git is fast. With Git, nearly all operations are performed locally, giving
-it a huge speed advantage on centralized systems that constantly have to
+it an huge speed advantage on centralized systems that constantly have to
@@ -11,4 +10,0 @@ from the start.
-Let's see how common operations stack up against Subversion, a common
-centralized version control system that is similar to CVS or
-Perforce. Smaller is faster.
-
@@ -34,0 +31,4 @@ SVN.
+Let's see how common operations stack up against Subversion, a common
+centralized version control system that is similar to CVS or
+Perforce. Smaller is faster.
+
diff --git a/readme.txt b/readme.txt
index 7b808f7..29ab705 100644
--- a/readme.txt
+++ b/readme.txt
@@ -1 +1 @@
-The Git feature that really makes it stand apart from nearly every other SCM
+The Git feature that r3ally mak3s it stand apart from n3arly 3v3ry other SCM
@@ -10,4 +9,0 @@ This means that you can do things like:
-Frictionless Context Switching. Create a branch to try out an idea, commit a
-few times, switch back to where you branched from, apply a patch, switch
-back to where you are experimenting, and merge it in.
-
@@ -27,3 +22,0 @@ Notably, when you push to a remote repository, you do not have to push all
-of your branches. You can choose to share just one of your branches, a few
-of them, or all of them. This tends to free people to try new ideas without
-worrying about having to plan how and when they are going to merge it in or
@@ -35 +28 @@ incredibly easy and it changes the way most developers work when they learn
-it.
+it.!
\\ No newline at end of file
EOS
  end

  def test_iteration
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    patches = []

    diff.each_patch do |patch|
      assert_instance_of Rugged::Patch, patch
      patches << patch
    end

    assert_equal 2, patches.size

    assert_equal "another.txt", patches[0].delta.old_file[:path]
    assert_equal "another.txt", patches[0].delta.new_file[:path]

    refute patches[0].delta.binary?

    hunks = []
    patches[0].each_hunk do |hunk|
      assert_instance_of Rugged::Diff::Hunk, hunk
      hunks << hunk
    end
    assert_equal 3, hunks.size

    assert hunks[0].header.start_with? "@@ -1,5 +1,5 @@"
    assert_equal 6, hunks[0].line_count
    assert_equal 1, hunks[0].old_start
    assert_equal 5, hunks[0].old_lines
    assert_equal 1, hunks[0].new_start
    assert_equal 5, hunks[0].new_lines

    assert hunks[1].header.start_with? "@@ -8,10 +8,6 @@"
    assert_equal 10, hunks[1].line_count
    assert_equal 8, hunks[1].old_start
    assert_equal 10, hunks[1].old_lines
    assert_equal 8, hunks[1].new_start
    assert_equal 6, hunks[1].new_lines

    assert hunks[2].header.start_with? "@@ -32,6 +28,10 @@"
    assert_equal 10, hunks[2].line_count
    assert_equal 32, hunks[2].old_start
    assert_equal 6, hunks[2].old_lines
    assert_equal 28, hunks[2].new_start
    assert_equal 10, hunks[2].new_lines

    assert_equal "readme.txt", patches[1].delta.old_file[:path]
    assert_equal "readme.txt", patches[1].delta.new_file[:path]

    refute patches[1].delta.binary?

    hunks = []
    patches[1].each_hunk do |hunk|
      assert_instance_of Rugged::Diff::Hunk, hunk
      hunks << hunk
    end
    assert_equal 3, hunks.size

    assert hunks[0].header.start_with? "@@ -1,4 +1,4 @@"
    assert hunks[1].header.start_with? "@@ -7,10 +7,6 @@"
    assert hunks[2].header.start_with? "@@ -24,12 +20,9 @@"

    lines = []
    hunks[0].each_line do |line|
      lines << line
    end
    assert_equal 5, lines.size

    assert_equal :deletion, lines[0].line_origin
    assert_equal "The Git feature that really makes it stand apart from nearly every other SCM\n", lines[0].content
    assert_equal 0, lines[0].content_offset

    assert_equal :addition, lines[1].line_origin
    assert_equal "The Git feature that r3ally mak3s it stand apart from n3arly 3v3ry other SCM\n", lines[1].content
    assert_equal 0, lines[1].content_offset

    assert_equal :context, lines[2].line_origin
    assert_equal "out there is its branching model.\n", lines[2].content
    assert_nil lines[2].content_offset

    assert_equal :context, lines[3].line_origin
    assert_equal "\n", lines[3].content

    assert_equal :context, lines[4].line_origin
    assert_equal "Git allows and encourages you to have multiple local branches that can be\n", lines[4].content

    lines = hunks[2].each_line.to_a

    assert_equal 14, lines.size

    assert_equal :deletion, lines[3].line_origin
    assert_equal "of your branches. You can choose to share just one of your branches, a few\n", lines[3].content
    assert_equal 1248, lines[3].content_offset

    assert_equal :deletion, lines[4].line_origin
    assert_equal "of them, or all of them. This tends to free people to try new ideas without\n", lines[4].content
    assert_equal 1323, lines[4].content_offset

    assert_equal :deletion, lines[11].line_origin
    assert_equal "it.\n", lines[11].content
    assert_equal 1721, lines[11].content_offset

    assert_equal :addition, lines[12].line_origin
    assert_equal "it.!", lines[12].content
    assert_equal 1289, lines[12].content_offset
  end

  def test_each_line_patch
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    lines = diff.each_line.to_a
    assert_equal 63, lines.size

    assert_equal(:file_header, lines[0].line_origin)
    assert_equal("diff --git a/another.txt b/another.txt\nindex 3e5bcba..546c735 100644\n--- a/another.txt\n+++ b/another.txt\n", lines[0].content)
    assert_equal(0, lines[0].content_offset)
    assert_equal(-1, lines[0].old_lineno)
    assert_equal(-1, lines[0].new_lineno)

    assert_equal(:hunk_header, lines[1].line_origin)
    assert_equal("@@ -1,5 +1,5 @@\n", lines[1].content)
    assert_equal(0, lines[1].content_offset)
    assert_equal(-1, lines[1].old_lineno)
    assert_equal(-1, lines[1].new_lineno)

    assert_equal(:context, lines[2].line_origin)
    assert_equal("Git is fast. With Git, nearly all operations are performed locally, giving\n", lines[2].content)
    assert_nil(lines[2].content_offset)
    assert_equal(1, lines[2].old_lineno)
    assert_equal(1, lines[2].new_lineno)

    assert_equal(:deletion, lines[3].line_origin)
    assert_equal("it a huge speed advantage on centralized systems that constantly have to\n", lines[3].content)
    assert_equal(75, lines[3].content_offset)
    assert_equal(2, lines[3].old_lineno)
    assert_equal(-1, lines[3].new_lineno)

    assert_equal(:addition, lines[4].line_origin)
    assert_equal("it an huge speed advantage on centralized systems that constantly have to\n", lines[4].content)
    assert_equal(75, lines[4].content_offset)
    assert_equal(-1, lines[4].old_lineno)
    assert_equal(2, lines[4].new_lineno)
  end

  def test_each_line_patch_header
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    lines = diff.each_line(:patch_header).to_a
    assert_equal 2, lines.size

    assert_equal(:file_header, lines[0].line_origin)
    assert_equal("diff --git a/another.txt b/another.txt\nindex 3e5bcba..546c735 100644\n--- a/another.txt\n+++ b/another.txt\n", lines[0].content)
    assert_equal(0, lines[0].content_offset)
    assert_equal(-1, lines[0].old_lineno)
    assert_equal(-1, lines[0].new_lineno)

    assert_equal(:file_header, lines[1].line_origin)
    assert_equal("diff --git a/readme.txt b/readme.txt\nindex 7b808f7..29ab705 100644\n--- a/readme.txt\n+++ b/readme.txt\n", lines[1].content)
    assert_equal(0, lines[1].content_offset)
    assert_equal(-1, lines[1].old_lineno)
    assert_equal(-1, lines[1].new_lineno)
  end

  def test_each_line_raw
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    lines = diff.each_line(:raw).to_a
    assert_equal 2, lines.size

    assert_equal(:file_header, lines[0].line_origin)
    assert_equal(":100644 100644 3e5bcba... 546c735... M\tanother.txt\n", lines[0].content)
    assert_equal(0, lines[0].content_offset)
    assert_equal(-1, lines[0].old_lineno)
    assert_equal(-1, lines[0].new_lineno)

    assert_equal(:file_header, lines[1].line_origin)
    assert_equal(":100644 100644 7b808f7... 29ab705... M\treadme.txt\n", lines[1].content)
    assert_equal(0, lines[1].content_offset)
    assert_equal(-1, lines[1].old_lineno)
    assert_equal(-1, lines[1].new_lineno)
  end

  def test_each_line_name_only
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    lines = diff.each_line(:name_only).to_a
    assert_equal 2, lines.size

    assert_equal(:file_header, lines[0].line_origin)
    assert_equal("another.txt\n", lines[0].content)
    assert_equal(0, lines[0].content_offset)
    assert_equal(-1, lines[0].old_lineno)
    assert_equal(-1, lines[0].new_lineno)

    assert_equal(:file_header, lines[1].line_origin)
    assert_equal("readme.txt\n", lines[1].content)
    assert_equal(0, lines[1].content_offset)
    assert_equal(-1, lines[1].old_lineno)
    assert_equal(-1, lines[1].new_lineno)
  end

  def test_each_line_name_status
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    lines = diff.each_line(:name_status).to_a
    assert_equal 2, lines.size

    assert_equal(:file_header, lines[0].line_origin)
    assert_equal("M\tanother.txt\n", lines[0].content)
    assert_equal(0, lines[0].content_offset)
    assert_equal(-1, lines[0].old_lineno)
    assert_equal(-1, lines[0].new_lineno)

    assert_equal(:file_header, lines[1].line_origin)
    assert_equal("M\treadme.txt\n", lines[1].content)
    assert_equal(0, lines[1].content_offset)
    assert_equal(-1, lines[1].old_lineno)
    assert_equal(-1, lines[1].new_lineno)
  end

  def test_each_line_unknown_format_raises_error
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    assert_raises ArgumentError do
      diff.each_line(:foobar).to_a
    end
  end

  def test_each_patch_returns_enumerator
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    assert_instance_of Enumerator, diff.each_patch

    patches = []
    diff.each_patch.each do |patch|
      assert_instance_of Rugged::Patch, patch
      patches << patch
    end
    assert_equal 2, patches.size
  end

  def test_each_hunk_returns_enumerator
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    diff.each_patch do |patch|
      assert_instance_of Enumerator, patch.each_hunk
    end
  end

  def test_each_line_returns_enumerator
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    diff.each_patch do |patch|
      patch.each_hunk do |hunk|
        assert_instance_of Enumerator, hunk.each_line
      end
    end
  end

  def test_patch
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    expected = <<-EOS
diff --git a/another.txt b/another.txt
index 3e5bcba..546c735 100644
--- a/another.txt
+++ b/another.txt
@@ -1,5 +1,5 @@
 Git is fast. With Git, nearly all operations are performed locally, giving
-it a huge speed advantage on centralized systems that constantly have to
+it an huge speed advantage on centralized systems that constantly have to
 communicate with a server somewhere.
 
 Git was built to work on the Linux kernel, meaning that it has had to
@@ -8,10 +8,6 @@ reducing the overhead of runtimes associated with higher-level
 languages. Speed and performance has been a primary design goal of the Git
 from the start.
 
-Let's see how common operations stack up against Subversion, a common
-centralized version control system that is similar to CVS or
-Perforce. Smaller is faster.
-
 For testing, large AWS instances were set up in the same availability
 zone. Git and SVN were installed on both machines, the Ruby repository was
 copied to both Git and SVN servers, and common operations were performed on
@@ -32,6 +28,10 @@ Clearly, in many of these common version control operations, Git is one or
 two orders of magnitude faster than SVN, even under ideal conditions for
 SVN.
 
+Let's see how common operations stack up against Subversion, a common
+centralized version control system that is similar to CVS or
+Perforce. Smaller is faster.
+
 One place where Git is slower is in the initial clone operation. Here, Git
 is downloading the entire history rather than only the latest version. As
 seen in the above charts, it's not considerably slower for an operation that
diff --git a/readme.txt b/readme.txt
index 7b808f7..29ab705 100644
--- a/readme.txt
+++ b/readme.txt
@@ -1,4 +1,4 @@
-The Git feature that really makes it stand apart from nearly every other SCM
+The Git feature that r3ally mak3s it stand apart from n3arly 3v3ry other SCM
 out there is its branching model.
 
 Git allows and encourages you to have multiple local branches that can be
@@ -7,10 +7,6 @@ those lines of development takes seconds.
 
 This means that you can do things like:
 
-Frictionless Context Switching. Create a branch to try out an idea, commit a
-few times, switch back to where you branched from, apply a patch, switch
-back to where you are experimenting, and merge it in.
-
 Role-Based Codelines. Have a branch that always contains only what goes to
 production, another that you merge work into for testing, and several
 smaller ones for day to day work.
@@ -24,12 +20,9 @@ not going to work, and just delete it - abandoning the work\xE2\x80\x94with nobody else
 ever seeing it (even if you've pushed other branches in the meantime).
 
 Notably, when you push to a remote repository, you do not have to push all
-of your branches. You can choose to share just one of your branches, a few
-of them, or all of them. This tends to free people to try new ideas without
-worrying about having to plan how and when they are going to merge it in or
 share it with others.
 
 There are ways to accomplish some of this with other systems, but the work
 involved is much more difficult and error-prone. Git makes this process
 incredibly easy and it changes the way most developers work when they learn
-it.
+it.!
\\ No newline at end of file
EOS

    expected.force_encoding('binary') if expected.respond_to?(:force_encoding)

    assert_equal expected, diff.patch
  end

  def test_patch_compact
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    assert_equal <<-EOS, diff.patch(:compact => true)
M\tanother.txt
M\treadme.txt
EOS
  end

  def test_stats
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    b = repo.lookup("7a9e0b02e63179929fed24f0a3e0f19168114d10")

    diff = a.tree.diff(b.tree)

    files, adds, dels = diff.stat
    assert_equal 2, files
    assert_equal 7, adds
    assert_equal 14, dels

    expected_patch_stat = [ [ 5, 5, 26 ], [ 2, 9, 28 ] ]

    diff.each_patch do |patch|
      expected_adds, expected_dels, expected_lines = expected_patch_stat.shift

      actual_adds, actual_dels = patch.stat

      assert_equal expected_adds, actual_adds
      assert_equal expected_dels, actual_dels
      assert_equal expected_adds + expected_dels, patch.changes

      assert_equal expected_lines, patch.lines(exclude_eofnl: true)
    end
  end
end

class TreeDiffRegression < Rugged::TestCase
  def test_nil_repo
    assert_raises TypeError do
      Rugged::Tree.diff nil, "foo"
    end
  end

  def test_self_is_not_tree
    repo = FixtureRepo.from_libgit2("diff")

    ex = assert_raises TypeError do
      Rugged::Tree.diff repo, "foo"
    end
    assert_equal "At least a Rugged::Tree object is required for diffing", ex.message
  end

  def test_self_or_other_must_be_present
    repo = FixtureRepo.from_libgit2("diff")

    ex = assert_raises TypeError do
      Rugged::Tree.diff repo, nil, nil
    end
    assert_equal "Need 'old' or 'new' for diffing", ex.message
  end

  def test_other_is_wrong_type
    repo = FixtureRepo.from_libgit2("diff")

    ex = assert_raises TypeError do
      Rugged::Tree.diff repo, nil, Object.new
    end
    assert_equal "A Rugged::Commit, Rugged::Tree or Rugged::Index instance is required", ex.message
  end

  def test_self_is_nil_other_is_tree_does_not_fail
    repo = FixtureRepo.from_libgit2("diff")

    a = repo.lookup("d70d245ed97ed2aa596dd1af6536e4bfdb047b69")

    diff = Rugged::Tree.diff(repo, nil, a.tree)
    assert_equal 2, diff.size
    assert_equal 2, diff.deltas.size

    delta = diff.deltas[0]
    assert_equal({
      oid: "0000000000000000000000000000000000000000",
      path: "another.txt",
      size: 0,
      flags: 4,
      mode: 0
    }, delta.old_file)

    assert_equal({
      oid: "3e5bcbad2a68e5bc60a53b8388eea53a1a7ab847",
      path: "another.txt",
      size: 0,
      flags: 12,
      mode: 0100644
    }, delta.new_file)

    delta = diff.deltas[1]
    assert_equal({
      oid: "0000000000000000000000000000000000000000",
      path: "readme.txt",
      size: 0,
      flags: 4,
      mode: 0
    }, delta.old_file)

    assert_equal({
      oid: "7b808f723a8ca90df319682c221187235af76693",
      path: "readme.txt",
      size: 0,
      flags: 12,
      mode: 0100644
    }, delta.new_file)
  end

  def test_other_tree_is_an_index_but_tree_is_nil
    repo = FixtureRepo.from_libgit2("diff")

    diff = Rugged::Tree.diff repo, nil, repo.index
    assert_equal 2, diff.size
    assert_equal 2, diff.deltas.size
  end
end
