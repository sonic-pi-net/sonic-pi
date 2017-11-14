require "test_helper"

class WorkdirCherrypickTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2("cherrypick")
  end

  def test_automerge
    signature = {
      name: "Picker",
      email: "picker@example.org"
    }

    @repo.reset("d3d77487660ee3c0194ee01dc5eaf478782b1c7e", :hard)

    @repo.cherrypick("cfc4f0999a8367568e049af4f72e452d40828a15")
    Rugged::Commit.create(@repo, {
      tree: @repo.index.write_tree,
      update_ref: "HEAD",
      parents: [ @repo.last_commit ],
      author: signature,
      committer: signature,
      message: "Cherry picked!"
    })

    assert_equal "38c05a857e831a7e759d83778bfc85d003e21c45", @repo.index["file1.txt"][:oid]
    assert_equal "a661b5dec1004e2c62654ded3762370c27cf266b", @repo.index["file2.txt"][:oid]
    assert_equal "df6b290e0bd6a89b01d69f66687e8abf385283ca", @repo.index["file3.txt"][:oid]

    @repo.cherrypick("964ea3da044d9083181a88ba6701de9e35778bf4")
    Rugged::Commit.create(@repo, {
      tree: @repo.index.write_tree,
      update_ref: "HEAD",
      parents: [ @repo.last_commit ],
      author: signature,
      committer: signature,
      message: "Cherry picked!"
    })

    assert_equal "38c05a857e831a7e759d83778bfc85d003e21c45", @repo.index["file1.txt"][:oid]
    assert_equal "bd8fc3c59fb52d3c8b5907ace7defa5803f82419", @repo.index["file2.txt"][:oid]
    assert_equal "df6b290e0bd6a89b01d69f66687e8abf385283ca", @repo.index["file3.txt"][:oid]

    @repo.cherrypick("a43a050c588d4e92f11a6b139680923e9728477d")
    Rugged::Commit.create(@repo, {
      tree: @repo.index.write_tree,
      update_ref: "HEAD",
      parents: [ @repo.last_commit ],
      author: signature,
      committer: signature,
      message: "Cherry picked!"
    })

    assert_equal "f06427bee380364bc7e0cb26a9245158e4726ce0", @repo.index["file1.txt"][:oid]
    assert_equal "bd8fc3c59fb52d3c8b5907ace7defa5803f82419", @repo.index["file2.txt"][:oid]
    assert_equal "df6b290e0bd6a89b01d69f66687e8abf385283ca", @repo.index["file3.txt"][:oid]
  end

  def test_merge_first_parent
    @repo.reset("cfc4f0999a8367568e049af4f72e452d40828a15", :hard)

    @repo.cherrypick("abe4603bc7cd5b8167a267e0e2418fd2348f8cff", mainline: 1)

    assert_equal "f90f9dcbdac2cce5cc166346160e19cb693ef4e8", @repo.index["file1.txt"][:oid]
    assert_equal "563f6473a3858f99b80e5f93c660512ed38e1e6f", @repo.index["file2.txt"][:oid]
    assert_equal "e233b9ed408a95e9d4b65fec7fc34943a556deb2", @repo.index["file3.txt"][:oid]
  end

  def test_merge_second_parent
    @repo.reset("cfc4f0999a8367568e049af4f72e452d40828a15", :hard)

    @repo.cherrypick("abe4603bc7cd5b8167a267e0e2418fd2348f8cff", mainline: 2)

    assert_equal "487434cace79238a7091e2220611d4f20a765690", @repo.index["file1.txt"][:oid]
    assert_equal "e5183bfd18e3a0a691fadde2f0d5610b73282d31", @repo.index["file2.txt"][:oid]
    assert_equal "409a1bec58bf35348e8b62b72bb9c1f45cf5a587", @repo.index["file3.txt"][:oid]
  end

  def test_cherrypick_commit
    index = @repo.cherrypick_commit("cfc4f0999a8367568e049af4f72e452d40828a15",
                                    "d3d77487660ee3c0194ee01dc5eaf478782b1c7e")

    assert_equal 0, index.conflicts.size
    assert_equal "38c05a857e831a7e759d83778bfc85d003e21c45", index["file1.txt"][:oid]
    assert_equal "a661b5dec1004e2c62654ded3762370c27cf266b", index["file2.txt"][:oid]
    assert_equal "df6b290e0bd6a89b01d69f66687e8abf385283ca", index["file3.txt"][:oid]


    assert_raises(Rugged::CherrypickError) do
      @repo.cherrypick_commit("abe4603bc7cd5b8167a267e0e2418fd2348f8cff",
                              "cfc4f0999a8367568e049af4f72e452d40828a15")
    end

    index = @repo.cherrypick_commit("abe4603bc7cd5b8167a267e0e2418fd2348f8cff",
                                  "cfc4f0999a8367568e049af4f72e452d40828a15", 1)

    assert_equal 0, index.conflicts.size
    assert_equal "f90f9dcbdac2cce5cc166346160e19cb693ef4e8", index["file1.txt"][:oid]
    assert_equal "563f6473a3858f99b80e5f93c660512ed38e1e6f", index["file2.txt"][:oid]
    assert_equal "e233b9ed408a95e9d4b65fec7fc34943a556deb2", index["file3.txt"][:oid]

    index = @repo.cherrypick_commit("abe4603bc7cd5b8167a267e0e2418fd2348f8cff",
                                  "cfc4f0999a8367568e049af4f72e452d40828a15", 2)

    assert_equal "487434cace79238a7091e2220611d4f20a765690", index["file1.txt"][:oid]
    assert_equal "e5183bfd18e3a0a691fadde2f0d5610b73282d31", index["file2.txt"][:oid]
    assert_equal "409a1bec58bf35348e8b62b72bb9c1f45cf5a587", index["file3.txt"][:oid]
  end
end
