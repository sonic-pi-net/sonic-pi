require "test_helper"

class SettingsTest < Rugged::TestCase
  def scrub_stack size
    return if size == 0
    scrub_stack size - 1
  end

  def test_used_cache_size
    # Repo objects hold on to the cache, so make sure all unreferenced repo objects
    # get GC'd
    scrub_stack 50
    GC.start

    size = Rugged::Settings.used_cache_size
    repo = FixtureRepo.from_libgit2("attr")
    repo.diff("605812a", "370fe9ec22", :context_lines => 1, :interhunk_lines => 1)

    # cache size should grow
    assert_operator size, :<, Rugged::Settings.used_cache_size
  end

  def test_max_cache_size
    # We don't assert anything about the default max cache size because it is
    # an implementation detail (libgit2 should be allowed to change the
    # default size without breaking these tests).
    assert Rugged::Settings.max_cache_size
  end
end
