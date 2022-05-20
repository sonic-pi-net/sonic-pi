# Differences from Git

In some instances, the functionality of libgit2 deviates slightly from Git. This can be because of technical limitations when developing a library, licensing limitations when converting functionality from Git to libgit2, or various other reasons.

Repository and Workdir Path Reporting
-------------------------------------

When asking Git for the absolute path of a repository via `git rev-parse --absolute-git-dir`, it will output the path to the ".git" folder without a trailing slash. In contrast to that, the call `git_repository_path(repo)` will return the path with a trailing slash:

```
git rev-parse --absolute-git-dir -> /home/user/projects/libgit2/.git
git_repository_path(repo) -> /home/user/projects/libgit2/.git/
```

The same difference exists when listing worktrees:

```
git worktree list -> /home/user/projects/libgit2
git_repository_workdir(repo) -> /home/user/projects/libgit2/
```

Windows Junction Points
-----------------------

In libgit2, junction points are treated like symbolic links. They're handled specially in `git_win32__file_attribute_to_stat` in `src/win/w32_util.h`. This means that libgit2 tracks the directory itself as a link.

In Git for Windows, junction points are treated like regular directories. This means that Git for Windows tracks the contents of the directory.
