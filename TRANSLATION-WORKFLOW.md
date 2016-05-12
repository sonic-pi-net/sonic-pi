# Making Sonic Pi translatable

[![Weblate](https://hosted.weblate.org/widgets/sonic-pi/-/svg-badge.svg)](https://hosted.weblate.org/engage/sonic-pi/)

(This document is meant for contributors to the Sonic Pi codebase. If 
you're a translator who wants to help bring Sonic Pi to your language 
please read the [Translation Guide Workflow](TRANSLATION.md).)

[Weblate](https://weblate.org) is open-source web-based translation 
editor. Their development team runs a hosted version of the tool and 
they have kindly offered us to use the service for free. (Thanks!)

The Weblate server keeps a copy of Sonic Pi's upstream git repository. 
Translators commit to the cloned repository on the Weblate server.

Sonic Pi's upstream git repository is tracked by Weblate, it will 
pull changes from and push updates to us.

## Setup

This is a one-time setup for the master repository and needs to be done 
by @samaaron.

- [Sign up](https://hosted.weblate.org/accounts/register/),
  preferably using your Github account authorization.

- To enable Weblate pulling updates from the master repository, add the
  [Weblate service](https://docs.weblate.org/en/latest/admin/continuous.html#github-setup)
  in the repository settings and use the URL
  `https://hosted.weblate.org` in the Weblate service settings.

- To enable Weblate pushing translation updates back to the master 
  repository, add the [Weblate push user](@weblate) to the 
  collaborators  in the master branch's repository settings.

After this, translations will be synced automatically between Github 
and Weblate.

## Workflow

Translations for the Qt GUI are located in 
[`app/gui/qt/lang/sonic-pi_<LANG>.ts`](./app/gui/qt/lang/). Do not edit 
these, that's what we use Weblate for.

The translatable message strings are marked in your C++ code using the 
[`tr()`](https://wiki.qt.io/QtInternationalization#What_is_tr.28.29.3F) 
macro. There is an extensive [I18N 
Tutorial](http://doc.qt.io/qt-5/internationalization.html) for Qt, but 
the condensed version is: When you mark a message string with `tr()`, 
it can be translated.

Whenever message strings are changed or a major feature introduces new 
ones, you need to update the `.ts` files.

To initiate a translation update, rebase your repo with the current 
HEAD of the master branch and then do

```
  cd app/gui/qt
  lupdate -pro SonicPi.pro -no-obsolete
```

This will update all Qt linguist files in 
`app/gui/qt/lang/sonic-pi_<LANG>.ts` with the new message strings from 
your code and remove obsolete entries.

After that, commit the updated `.ts` files and push them back to the 
master branch.

Weblate will then fetch them automatically, translators will update 
them and the finished translations will flow back into the master 
repository.
