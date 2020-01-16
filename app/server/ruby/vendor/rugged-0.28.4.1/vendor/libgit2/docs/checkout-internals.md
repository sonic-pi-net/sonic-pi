Checkout Internals
==================

Checkout has to handle a lot of different cases.  It examines the
differences between the target tree, the baseline tree and the working
directory, plus the contents of the index, and groups files into five
categories:

1. UNMODIFIED - Files that match in all places.
2. SAFE - Files where the working directory and the baseline content
   match that can be safely updated to the target.
3. DIRTY/MISSING - Files where the working directory differs from the
   baseline but there is no conflicting change with the target.  One
   example is a file that doesn't exist in the working directory - no
   data would be lost as a result of writing this file.  Which action
   will be taken with these files depends on the options you use.
4. CONFLICTS - Files where changes in the working directory conflict
   with changes to be applied by the target.  If conflicts are found,
   they prevent any other modifications from being made (although there
   are options to override that and force the update, of course).
5. UNTRACKED/IGNORED - Files in the working directory that are untracked
   or ignored (i.e. only in the working directory, not the other places).

Right now, this classification is done via 3 iterators (for the three
trees), with a final lookup in the index.  At some point, this may move to
a 4 iterator version to incorporate the index better.

The actual checkout is done in five phases (at least right now).

1. The diff between the baseline and the target tree is used as a base
   list of possible updates to be applied.
2. Iterate through the diff and the working directory, building a list of
   actions to be taken (and sending notifications about conflicts and
   dirty files).
3. Remove any files / directories as needed (because alphabetical
   iteration means that an untracked directory will end up sorted *after*
   a blob that should be checked out with the same name).
4. Update all blobs.
5. Update all submodules (after 4 in case a new .gitmodules blob was
   checked out)

Checkout could be driven either off a target-to-workdir diff or a
baseline-to-target diff.  There are pros and cons of each.

Target-to-workdir means the diff includes every file that could be
modified, which simplifies bookkeeping, but the code to constantly refer
back to the baseline gets complicated.

Baseline-to-target has simpler code because the diff defines the action to
take, but needs special handling for untracked and ignored files, if they
need to be removed.

The current checkout implementation is based on a baseline-to-target diff.


Picking Actions
===============

The most interesting aspect of this is phase 2, picking the actions that
should be taken.  There are a lot of corner cases, so it may be easier to
start by looking at the rules for a simple 2-iterator diff:

Key
---
- B1,B2,B3 - blobs with different SHAs,
- Bi       - ignored blob (WD only)
- T1,T2,T3 - trees with different SHAs,
- Ti       - ignored tree (WD only)
- S1,S2    - submodules with different SHAs
- Sd       - dirty submodule (WD only)
- x        - nothing

Diff with 2 non-workdir iterators
---------------------------------

|    | Old | New |                                                            |
|----|-----|-----|------------------------------------------------------------|
|  0 |   x |   x | nothing                                                    |
|  1 |   x |  B1 | added blob                                                 |
|  2 |   x |  T1 | added tree                                                 |
|  3 |  B1 |   x | removed blob                                               |
|  4 |  B1 |  B1 | unmodified blob                                            |
|  5 |  B1 |  B2 | modified blob                                              |
|  6 |  B1 |  T1 | typechange blob -> tree                                    |
|  7 |  T1 |   x | removed tree                                               |
|  8 |  T1 |  B1 | typechange tree -> blob                                    |
|  9 |  T1 |  T1 | unmodified tree                                            |
| 10 |  T1 |  T2 | modified tree (implies modified/added/removed blob inside) |


Now, let's make the "New" iterator into a working directory iterator, so
we replace "added" items with either untracked or ignored, like this:

Diff with non-work & workdir iterators
--------------------------------------

|    | Old | New |                                                            |
|----|-----|-----|------------------------------------------------------------|
|  0 |  x  | x   | nothing                                                    |
|  1 |  x  | B1  | untracked blob                                             |
|  2 |  x  | Bi  | ignored file                                               |
|  3 |  x  | T1  | untracked tree                                             |
|  4 |  x  | Ti  | ignored tree                                               |
|  5 | B1  | x   | removed blob                                               |
|  6 | B1  | B1  | unmodified blob                                            |
|  7 | B1  | B2  | modified blob                                              |
|  8 | B1  | T1  | typechange blob -> tree                                    |
|  9 | B1  | Ti  | removed blob AND ignored tree as separate items            |
| 10 | T1  | x   | removed tree                                               |
| 11 | T1  | B1  | typechange tree -> blob                                    |
| 12 | T1  | Bi  | removed tree AND ignored blob as separate items            |
| 13 | T1  | T1  | unmodified tree                                            |
| 14 | T1  | T2  | modified tree (implies modified/added/removed blob inside) |

Note: if there is a corresponding entry in the old tree, then a working
directory item won't be ignored (i.e. no Bi or Ti for tracked items).


Now, expand this to three iterators: a baseline tree, a target tree, and
an actual working directory tree:

Checkout From 3 Iterators (2 not workdir, 1 workdir)
----------------------------------------------------

(base == old HEAD; target == what to checkout; actual == working dir)

|     |base | target | actual/workdir |                                                                    |
|-----|-----|------- |----------------|--------------------------------------------------------------------|
|  0  |   x |      x |      x         | nothing                                                            |
|  1  |   x |      x | B1/Bi/T1/Ti    | untracked/ignored blob/tree (SAFE)                                 |
|  2+ |   x |     B1 |      x         | add blob (SAFE)                                                    |
|  3  |   x |     B1 |     B1         | independently added blob (FORCEABLE-2)                             |
|  4* |   x |     B1 | B2/Bi/T1/Ti    | add blob with content conflict (FORCEABLE-2)                       |
|  5+ |   x |     T1 |      x         | add tree (SAFE)                                                    |
|  6* |   x |     T1 |  B1/Bi         | add tree with blob conflict (FORCEABLE-2)                          |
|  7  |   x |     T1 |   T1/i         | independently added tree (SAFE+MISSING)                            |
|  8  |  B1 |      x |      x         | independently deleted blob (SAFE+MISSING)                          |
|  9- |  B1 |      x |     B1         | delete blob (SAFE)                                                 |
| 10- |  B1 |      x |     B2         | delete of modified blob (FORCEABLE-1)                              |
| 11  |  B1 |      x |  T1/Ti         | independently deleted blob AND untrack/ign tree (SAFE+MISSING !!!) |
| 12  |  B1 |     B1 |      x         | locally deleted blob (DIRTY || SAFE+CREATE)                        |
| 13+ |  B1 |     B2 |      x         | update to deleted blob (SAFE+MISSING)                              |
| 14  |  B1 |     B1 |     B1         | unmodified file (SAFE)                                             |
| 15  |  B1 |     B1 |     B2         | locally modified file (DIRTY)                                      |
| 16+ |  B1 |     B2 |     B1         | update unmodified blob (SAFE)                                      |
| 17  |  B1 |     B2 |     B2         | independently updated blob (FORCEABLE-1)                           |
| 18+ |  B1 |     B2 |     B3         | update to modified blob (FORCEABLE-1)                              |
| 19  |  B1 |     B1 |  T1/Ti         | locally deleted blob AND untrack/ign tree (DIRTY)                  |
| 20* |  B1 |     B2 |  T1/Ti         | update to deleted blob AND untrack/ign tree (F-1)                  |
| 21+ |  B1 |     T1 |      x         | add tree with locally deleted blob (SAFE+MISSING)                  |
| 22* |  B1 |     T1 |     B1         | add tree AND deleted blob (SAFE)                                   |
| 23* |  B1 |     T1 |     B2         | add tree with delete of modified blob (F-1)                        |
| 24  |  B1 |     T1 |     T1         | add tree with deleted blob (F-1)                                   |
| 25  |  T1 |      x |      x         | independently deleted tree (SAFE+MISSING)                          |
| 26  |  T1 |      x |  B1/Bi         | independently deleted tree AND untrack/ign blob (F-1)              |
| 27- |  T1 |      x |     T1         | deleted tree (MAYBE SAFE)                                          |
| 28+ |  T1 |     B1 |      x         | deleted tree AND added blob (SAFE+MISSING)                         |
| 29  |  T1 |     B1 |     B1         | independently typechanged tree -> blob (F-1)                       |
| 30+ |  T1 |     B1 |     B2         | typechange tree->blob with conflicting blob (F-1)                  |
| 31* |  T1 |     B1 |  T1/T2         | typechange tree->blob (MAYBE SAFE)                                 |
| 32+ |  T1 |     T1 |      x         | restore locally deleted tree (SAFE+MISSING)                        |
| 33  |  T1 |     T1 |  B1/Bi         | locally typechange tree->untrack/ign blob (DIRTY)                  |
| 34  |  T1 |     T1 |  T1/T2         | unmodified tree (MAYBE SAFE)                                       |
| 35+ |  T1 |     T2 |      x         | update locally deleted tree (SAFE+MISSING)                         |
| 36* |  T1 |     T2 |  B1/Bi         | update to tree with typechanged tree->blob conflict (F-1)          |
| 37  |  T1 |     T2 | T1/T2/T3       | update to existing tree (MAYBE SAFE)                               |
| 38+ |   x |     S1 |      x         | add submodule (SAFE)                                               |
| 39  |   x |     S1 |  S1/Sd         | independently added submodule (SUBMODULE)                          |
| 40* |   x |     S1 |     B1         | add submodule with blob confilct (FORCEABLE)                       |
| 41* |   x |     S1 |     T1         | add submodule with tree conflict (FORCEABLE)                       |
| 42  |  S1 |      x |  S1/Sd         | deleted submodule (SUBMODULE)                                      |
| 43  |  S1 |      x |      x         | independently deleted submodule (SUBMODULE)                        |
| 44  |  S1 |      x |     B1         | independently deleted submodule with added blob (SAFE+MISSING)     |
| 45  |  S1 |      x |     T1         | independently deleted submodule with added tree (SAFE+MISSING)     |
| 46  |  S1 |     S1 |      x         | locally deleted submodule (SUBMODULE)                              |
| 47+ |  S1 |     S2 |      x         | update locally deleted submodule (SAFE)                            |
| 48  |  S1 |     S1 |     S2         | locally updated submodule commit (SUBMODULE)                       |
| 49  |  S1 |     S2 |     S1         | updated submodule commit (SUBMODULE)                               |
| 50+ |  S1 |     B1 |      x         | add blob with locally deleted submodule (SAFE+MISSING)             |
| 51* |  S1 |     B1 |     S1         | typechange submodule->blob (SAFE)                                  |
| 52* |  S1 |     B1 |     Sd         | typechange dirty submodule->blob (SAFE!?!?)                        |
| 53+ |  S1 |     T1 |      x         | add tree with locally deleted submodule (SAFE+MISSING)             |
| 54* |  S1 |     T1 |  S1/Sd         | typechange submodule->tree (MAYBE SAFE)                            |
| 55+ |  B1 |     S1 |      x         | add submodule with locally deleted blob (SAFE+MISSING)             |
| 56* |  B1 |     S1 |     B1         | typechange blob->submodule (SAFE)                                  |
| 57+ |  T1 |     S1 |      x         | add submodule with locally deleted tree (SAFE+MISSING)             |
| 58* |  T1 |     S1 |     T1         | typechange tree->submodule (SAFE)                                  |


The number is followed by ' ' if no change is needed or '+' if the case
needs to write to disk or '-' if something must be deleted and '*' if
there should be a delete followed by an write.

There are four tiers of safe cases:

* SAFE         == completely safe to update
* SAFE+MISSING == safe except the workdir is missing the expect content
* MAYBE SAFE   == safe if workdir tree matches (or is missing) baseline
                  content, which is unknown at this point
* FORCEABLE == conflict unless FORCE is given
* DIRTY     == no conflict but change is not applied unless FORCE
* SUBMODULE == no conflict and no change is applied unless a deleted
               submodule dir is empty

Some slightly unusual circumstances:

* 8 - parent dir is only deleted when file is, so parent will be left if
    empty even though it would be deleted if the file were present
* 11 - core git does not consider this a conflict but attempts to delete T1
    and gives "unable to unlink file" error yet does not skip the rest
    of the operation
* 12 - without FORCE file is left deleted (i.e. not restored) so new wd is
    dirty (and warning message "D file" is printed), with FORCE, file is
    restored.
* 24 - This should be considered MAYBE SAFE since effectively it is 7 and 8
    combined, but core git considers this a conflict unless forced.
* 26 - This combines two cases (1 & 25) (and also implied 8 for tree content)
    which are ok on their own, but core git treat this as a conflict.
    If not forced, this is a conflict.  If forced, this actually doesn't
    have to write anything and leaves the new blob as an untracked file.
* 32 - This is the only case where the baseline and target values match
    and yet we will still write to the working directory.  In all other
    cases, if baseline == target, we don't touch the workdir (it is
    either already right or is "dirty").  However, since this case also
    implies that a ?/B1/x case will exist as well, it can be skipped.
* 41 - It's not clear how core git distinguishes this case from 39 (mode?).
* 52 - Core git makes destructive changes without any warning when the
    submodule is dirty and the type changes to a blob.

Cases 3, 17, 24, 26, and 29 are all considered conflicts even though
none of them will require making any updates to the working directory.
