# Making Sonic Pi translatable

[![Weblate](https://hosted.weblate.org/widgets/sonic-pi/-/svg-badge.svg)](https://hosted.weblate.org/engage/sonic-pi/)

(This document is meant for contributors to the Sonic Pi codebase. If 
you're a translator who wants to help bring Sonic Pi to your language 
please read the [Translation Guide](TRANSLATION.md).)

## Making your code translatable

Translations for the Qt GUI are located in the Qt Linguist `.ts` files 
in [`app/gui/qt/lang/sonic-pi_<LANG>.ts`](./app/gui/qt/lang/). Do not 
edit these, that's what we use Weblate for.

The translatable message strings are marked in your C++ code using the 
[`tr()`](https://wiki.qt.io/QtInternationalization#What_is_tr.28.29.3F) 
macro.

There is an extensive [I18N 
Tutorial](http://doc.qt.io/qt-5/internationalization.html) for Qt, but 
the condensed version is: _When you mark a message string with `tr()`, 
it can be translated._

And that's it, that's already all you need to know to have your code
ready for i18n in Sonic Pi.

## Integrating Weblate

The rest of this document is mostly meant as a cheatsheet for @samaaron 
as the main developer in charge of the master repository.

[Weblate](https://weblate.org) is an open-source web-based translation 
editor. Their development team runs a hosted version of the tool and 
they have kindly offered us to use the service for free. (Thanks!)

The Weblate server keeps a copy of Sonic Pi's upstream git repository. 
Translators commit to the cloned repository on the Weblate server.

Sonic Pi's upstream git repository [is tracked by 
Weblate](http://weblate.readthedocs.io/en/latest/admin/continuous.html), 
it will pull changes from and push updates to us.

## Setup

This is a one-time setup for the master repository and needs to be done 
by the main developer.

- [Sign up](https://hosted.weblate.org/accounts/register/),
  preferably using your Github account authorization.

- To enable Weblate pulling updates from the master repository, add the
  [Weblate service](https://docs.weblate.org/en/latest/admin/continuous.html#github-setup)
  in the repository settings and use the URL
  `https://hosted.weblate.org` for the Weblate service settings.

- To enable Weblate pushing translation updates back to the master 
  repository, add the [Weblate push user](https://github.com/weblate) 
  to the collaborators in the master branch's repository settings.

After this, translations will be synced automatically between Github 
and Weblate, using the [lazy 
commit](http://weblate.readthedocs.io/en/latest/admin/continuous.html#lazy-commits) 
strategy.

## Workflow

The Qt Linguist `.ts` files are created and updated from the source 
code, using the Qt `lupdate` tool.

The Tutorial `.po` files are created and updated from the tutorial's
Markdown source texts, using the `i18n-tool.rb` script.

Whenever message strings or parts of the tutorial are changed or a
major feature introduces new texts, you need to update these files.

Don't update too often, as we don't want to annoy the volunteer 
translators with many small work chunks. Remember to do an extra update 
some time before a major release, to give everybody a chance to 
complete the translation.

To initiate a translation update:

1. [Lock the Sonic Pi project](https://hosted.weblate.org/projects/sonic-pi/#repository)
   on Weblate, then wait for Weblate to automatically commit and merge
   all outstanding  translation updates and wait for it to push them from
   Weblate to Github.

2. Update your local repo to the current HEAD of the master branch from 
   Github, update the translation files, commit the update and push it
   back to the master branch.
  
   ```
     git pull

     lupdate -pro app/gui/qt/SonicPi.pro -no-obsolete
     git commit app/gui/qt/lang/sonic-pi_*.ts

     app/server/ruby/bin/i18n-tool.rb -x
     # the following will complain about every fuzzy entry
     app/server/ruby/bin/i18n-tool.rb -u
     git commit etc/doc/lang/*.po
     
     git push
   ```

3. Wait for Weblate to sync these changes back from Github, then
   [unlock the Sonic Pi project](https://hosted.weblate.org/projects/sonic-pi/#repository)
   on Weblate.

This will update all translation files and remove obsolete entries.

Weblate will then fetch the changes automatically, translators can 
update them and the finished translations will flow back into the 
master repository.
