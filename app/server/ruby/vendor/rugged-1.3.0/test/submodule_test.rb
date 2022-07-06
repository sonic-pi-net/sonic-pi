require 'test_helper'

class SubmoduleTest < Rugged::TestCase
  def setup
    @repo = FixtureRepo.from_libgit2('submod2').tap do |repo|
      Dir.chdir(repo.workdir) do
        File.rename(
          File.join('not-submodule', '.gitted'),
          File.join('not-submodule', '.git')
        )

        File.rename(
          File.join('not', '.gitted'),
          File.join('not', '.git')
        )
      end
    end
  end

  class TestException < StandardError
  end

  def test_submodule_simple_lookup
    # lookup pending change in .gitmodules that is not in HEAD
    assert @repo.submodules['sm_added_and_uncommited']

    # lookup pending change in .gitmodules that is neither in HEAD nor index
    assert @repo.submodules['sm_gitmodules_only']

    # lookup git repo subdir that is not added as submodule */
    assert_raises Rugged::SubmoduleError do
      assert @repo.submodules['not-submodule']
    end

    # lookup existing directory that is not a submodule
    assert_nil @repo.submodules['just_a_dir']

    # lookup existing file that is not a submodule
    assert_nil @repo.submodules['just_a_file']

    # lookup non-existent item
    assert_nil @repo.submodules['no_such_file']
  end

  def test_submodule_attribute_getters
    # unchanged
    submodule = @repo.submodules['sm_unchanged']
    oid = "480095882d281ed676fe5b863569520e54a7d5c0"

    submodule_repo = submodule.repository
    assert_instance_of Rugged::Repository, submodule_repo

    assert :none, submodule.ignore_rule
    assert submodule.path.end_with?('sm_unchanged')
    #assert submodule.url.end_with?('submod2_target')
    assert_equal 'sm_unchanged', submodule.name

    assert_equal oid, submodule.head_oid
    assert_equal oid, submodule.index_oid
    assert_equal oid, submodule.workdir_oid

    # changed head
    submodule = @repo.submodules['sm_changed_head']

    assert_equal 'sm_changed_head', submodule.name
    assert_equal oid, submodule.head_oid
    assert_equal oid, submodule.index_oid
    assert_equal '3d9386c507f6b093471a3e324085657a3c2b4247', submodule.workdir_oid

    # added and uncommited
    submodule = @repo.submodules['sm_added_and_uncommited']

    assert_equal 'sm_added_and_uncommited', submodule.name
    assert_nil submodule.head_oid
    assert_equal oid, submodule.index_oid
    assert_equal oid, submodule.workdir_oid

    # missing commits
    submodule = @repo.submodules['sm_missing_commits']
    assert_equal 'sm_missing_commits', submodule.name
    assert_equal oid, submodule.head_oid
    assert_equal oid, submodule.index_oid
    assert_equal '5e4963595a9774b90524d35a807169049de8ccad', submodule.workdir_oid

    # status checks
    submodule = @repo.submodules['sm_unchanged']

    expected = [:in_head, :in_index, :in_config, :in_workdir]
    assert_equal expected, submodule.status
    assert submodule.in_head?
    assert submodule.in_index?
    assert submodule.in_config?
    assert submodule.in_workdir?
    assert submodule.unmodified?
    refute submodule.dirty_workdir?
  end

  def test_submodule_each
    assert @repo.submodules.kind_of? Enumerable
    assert_instance_of Enumerator, @repo.submodules.each

    @repo.submodules.each do |submodule|
      assert_equal :none, submodule.ignore_rule
      assert submodule.name
      assert submodule.url
      assert submodule.path
    end

    # test error handling in callback
    assert_raises TestException do
      @repo.submodules.each do |submodule|
        raise TestException
      end
    end
  end

  def test_submodule_status_ignore_none
    submodule = @repo.submodules['sm_changed_index']
    assert_includes submodule.status, :dirty_workdir_index
    assert submodule.dirty_workdir_index?

    submodule = @repo.submodules['sm_changed_head']
    assert_includes submodule.status, :modified_in_workdir
    assert submodule.modified_in_workdir?

    submodule = @repo.submodules['sm_changed_file']
    assert_includes submodule.status, :modified_files_in_workdir
    assert submodule.modified_files_in_workdir?
    assert submodule.dirty_workdir?
    refute submodule.unmodified?

    submodule = @repo.submodules['sm_changed_untracked_file']
    assert_includes submodule.status, :untracked_files_in_workdir
    assert submodule.untracked_files_in_workdir?

    submodule = @repo.submodules['sm_missing_commits']
    assert_includes submodule.status, :modified_in_workdir
    assert submodule.modified_in_workdir?

    submodule = @repo.submodules['sm_added_and_uncommited']
    assert_includes submodule.status, :added_to_index
    assert submodule.added_to_index?

    sm_unchanged_path = File.join(@repo.workdir, 'sm_unchanged')

    # removed sm_unchanged for deleted workdir
    FileUtils.remove_entry_secure(sm_unchanged_path)
    submodule = @repo.submodules['sm_unchanged']
    assert_includes submodule.status, :deleted_from_workdir
    assert submodule.deleted_from_workdir?

    # now mkdir sm_unchanged to test uninitialized
    FileUtils.mkdir(sm_unchanged_path, :mode => 0755)
    submodule = @repo.submodules['sm_unchanged']
    assert_includes submodule.status, :uninitialized
    assert submodule.uninitialized?

    # update sm_changed_head in index
    submodule = @repo.submodules['sm_changed_head']
    submodule.add_to_index(write_index: true)
    assert_includes submodule.status, :modified_in_index
    assert submodule.modified_in_index?

    # remove sm_changed_head from index
    index = @repo.index
    index.remove('sm_changed_head')
    index.write

    submodule = @repo.submodules['sm_changed_head']
    assert_includes submodule.status, :deleted_from_index
    assert submodule.deleted_from_index?
  end

  def test_submodule_update_ignore_rule
    sm_unchanged_path = File.join(@repo.workdir, 'sm_unchanged')
    # removed sm_unchanged for deleted workdir
    FileUtils.remove_entry_secure(sm_unchanged_path)

    # untracked
    @repo.submodules.update('sm_changed_untracked_file', ignore_rule: :untracked)
    submodule = @repo.submodules['sm_changed_untracked_file']

    assert submodule.unmodified?
    refute submodule.untracked_files_in_workdir?

    #dirty
    @repo.submodules.update('sm_changed_file', ignore_rule: :dirty)
    submodule = @repo.submodules['sm_changed_file']

    refute submodule.modified_files_in_workdir?

    #all
    @repo.submodules.update('sm_added_and_uncommited', ignore_rule: :all)
    submodule = @repo.submodules['sm_added_and_uncommited']

    assert submodule.unmodified?
    refute submodule.added_to_index?
  end

  def test_submodule_update
    url = 'https://github.com/libgit2/libgit2.git'
    submodule = @repo.submodules['sm_changed_head']

    refute submodule.fetch_recurse_submodules?

    @repo.submodules.update(submodule, {
      url: url,
      ignore_rule: :untracked,
      fetch_recurse_submodules: true
    })

    submodule.reload

    assert_equal :untracked, submodule.ignore_rule
    assert_equal url, submodule.url
    assert submodule.fetch_recurse_submodules?
  end

  def test_submodule_update_update_rule
    submodule = @repo.submodules['sm_unchanged']
    assert_equal :checkout, submodule.update_rule

    @repo.submodules.update('sm_unchanged', update_rule: :rebase)
    submodule.reload
    assert_equal :rebase, submodule.update_rule

    @repo.submodules.update('sm_unchanged', update_rule: :merge)
    submodule.reload
    assert_equal :merge, submodule.update_rule

    @repo.submodules.update('sm_unchanged', update_rule: :none)
    submodule.reload
    assert_equal :none, submodule.update_rule
  end

  def test_submodule_sync
    submodule = @repo.submodules['sm_unchanged']

    # At this point, the .git/config URLs for the submodules have
    # not be rewritten with the absolute paths (although the
    # .gitmodules have.  Let's confirm that they DO NOT match
    # yet, then we can do a sync to make them match...
    refute_equal submodule.url, @repo.config['submodule.sm_unchanged.url']

    submodule.sync

    assert_equal submodule.url, @repo.config['submodule.sm_unchanged.url']
  end

  def test_submodule_init
    submodule = @repo.submodules['sm_unchanged']

    #erase submodule data from .git/config
    @repo.config.delete('submodule.sm_unchanged.url')

    # confirm no submodule data in config
    assert_nil @repo.config['submodule.sm_unchanged.url']

    # call init and see that settings are copied. Call it twice, just to check
    # if it accepts the overwrite flag and that it's optional
    submodule.init(overwrite: true)
    submodule.init

    submodule.reload

    # confirm submodule data in config
    assert_equal submodule.url, @repo.config['submodule.sm_unchanged.url']
  end

  def test_submodule_setup_add
    url = 'https://github.com/libgit2/libgit2.git'
    submod_path = 'sm_libgit2'
    second_submod_path = 'sm2_libgit2'

    # re-add existing submodule
    assert_raises Rugged::SubmoduleError do
      @repo.submodules.setup_add('whatever', 'sm_unchanged')
    end

    # add a submodule using gitlink by default
    @repo.submodules.setup_add(url, submod_path)

    assert File.file?(File.join(@repo.workdir, submod_path, '.git'))
    assert File.directory?(File.join(@repo.path, 'modules'))
    assert File.directory?(File.join(@repo.path, 'modules', submod_path))
    assert File.file?(File.join(@repo.path, 'modules', submod_path, 'HEAD'))

    assert_equal url, @repo.config["submodule.#{submod_path}.url"]

    @repo.submodules.setup_add(url, second_submod_path, gitlink: false)

    assert File.directory?(File.join(@repo.workdir, second_submod_path, '.git'))
    refute File.exist?(File.join(@repo.path, 'modules', second_submod_path))
    assert_equal url, @repo.config["submodule.#{submod_path}.url"]
  end

  def test_submodule_add
    url = File.join(Rugged::TestCase::TEST_DIR, 'fixtures', 'testrepo.git')
    submod_path = 'sm_test_repo'

    callback_called = false
    update_tips_cb =  proc { callback_called = true }

    submodule = @repo.submodules.add(url, submod_path, update_tips: update_tips_cb)

    # options are passed to remote fetch
    assert callback_called

    # submodule repo is initialized
    assert File.file?(File.join(@repo.workdir, submod_path, '.git'))

    # submodule repo is checked out
    assert File.file?(File.join(@repo.workdir, submod_path, 'README'))

    # sets up master as a remote tracking branch
    assert submodule.repository.branches['master']
    assert_equal 'origin/master', submodule.repository.branches['master'].upstream.name
  end
end
