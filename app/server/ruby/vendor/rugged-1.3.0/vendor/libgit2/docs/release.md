# Releasing the library

We have three kinds of releases: "full" releases, maintenance releases and security releases. Full ones release the state of the `main` branch whereas maintenance releases provide bugfixes building on top of the currently released series. Security releases are also for the current series but only contain security fixes on top of the previous release.

## Full release

We aim to release once every six months. We start the process by opening an issue. This is accompanied with a feature freeze. From now until the release, only bug fixes are to be merged. Use the following as a base for the issue

    Release v0.X
    
    Let's release v0.X, codenamed: <something witty>
    
    - [ ] Bump the versions in the headers (`include/git2/version.h`)
    - [ ] Bump the versions in the clib manifest (`package.json`)
    - [ ] Make a release candidate
    - [ ] Plug any final leaks
    - [ ] Fix any last-minute issues
    - [ ] Make sure changelog.md reflects everything worth discussing
    - [ ] Update the version in changelog.md and the header
    - [ ] Produce a release candidate
    - [ ] Tag
    - [ ] Create maint/v0.X
    - [ ] Update any bindings the core team works with

We tag at least one release candidate. This RC must carry the new version in the headers, including the SOVERSION. If there are no significant issues found, we can go straight to the release after a single RC. This is up to the discretion of the release manager. There is no set time to have the candidate out, but we should we should give downstream projects at least a week to give feedback.

Preparing the first release candidate includes updating the version number of libgit2 to the new version number. To do so, a pull request shall be submitted that adjusts the version number in the following places:

- docs/changelog.md
- include/git2/version.h
- package.json

As soon as the pull request is merged, the merge commit shall be tagged with a lightweight tag.

The tagging happens via GitHub's "releases" tab which lets us attach release notes to a particular tag. In the description we include the changes in `docs/changelog.md` between the last full release. Use the following as a base for the release notes

    This is the first release of the v0.X series, <codename>. The changelog follows.

followed by the three sections in the changelog. For release candidates we can avoid copying the full changelog and only include any new entries.

During the freeze, and certainly after the first release candidate, any bindings the core team work with should be updated in order to discover any issues that might come up with the multitude of approaches to memory management, embedding or linking.

Create a branch `maint/v0.X` at the current state of `main` after you've created the tag. This will be used for maintenance releases and lets our dependents track the latest state of the series.

## Maintenance release

Every once in a while, when we feel we've accumulated a significant amount of backportable fixes in the mainline branch, we produce a maintenance release in order to provide fixes or improvements for those who track the releases. This also lets our users and integrators receive updates without having to upgrade to the next full release.

As a rule of thumb, it's a good idea to produce a maintenance release for the current series when we're getting ready for a full release. This gives the (still) current series a last round of fixes without having to upgrade (which with us potentially means adjusting to API changes).

Start by opening an issue. Use the following as a base.

    Release v0.X.Y
    
    Enough fixes have accumulated, let's release v0.X.Y
    
    - [ ] Select the changes we want to backport
    - [ ] Update maint/v0.X
    - [ ] Tag

The list of changes to backport does not need to be comprehensive and we might not backport something if the code in mainline has diverged significantly. These fixes do not include those which require API or ABI changes as we release under the same SOVERSION.

Do not merge into the `maint/v0.X` until we are getting ready to produce a new release. There is always the possibility that we will need to produce a security release and those must only include the relevant security fixes and not arbitrary fixes we were planning on releasing at some point.

Here we do not use release candidates as the changes are supposed to be small and proven.

## Security releases

This is the same as a maintenance release, except that the fix itself will most likely be developed in a private repository and will only be visible to a select group of people until the release.

We have committed to providing security fixes for the latest two released versions.  E.g. if the latest version is v0.28.x, then we will provide security fixes for both v0.28.x and v0.27.y.

## Updating documentation

We use docurium to generate our documentation. It is a tool written in ruby which leverages libclang's documentation parser. Install docurium

    gem install docurium

and run it against our description file with the tip of `main` checked out.

    cm doc api.docurium

It will start up a few proceses and write out the results as a new commit onto the `gh-pages` branch. That can be pushed to GitHub to update what will show up on our documentation reference site.
