# Translating Sonic Pi

At present, you can translate the tutorial and the Qt GUI, only.

Translations for the tutorial are located in
[`etc/doc/tutorial`](./etc/doc/tutorial/).

Translations for the Qt GUI are located in
[`app/gui/qt/lang/sonic-pi_<LANG>.ts`](./app/gui/qt/lang/).

## Please note

The i18n code is disabled right now. If you want to enable it to test
your translation, set the required constants in `main.cpp`
(`ENABLE_I18N`) and `qt-doc.rb` (`enable_i18n`).

## Translating the tutorial

- Sign up with [github](https://help.github.com/categories/bootcamp/)
- Fork the [Sonic Pi repo](https://github.com/samaaron/sonic-pi)
  to your own github repo, `git clone` it to your computer
- `cd etc/doc/tutorial`
- Want to add a new language to the tutorial? Then copy the `en/`
  folder and add the new files to your repo with `git add <NEWLANG>/*.md`
- When you're happy, `git commit`, `git push` to your github repo.
- [Send a pull request](https://help.github.com/articles/creating-a-pull-request/) to the Sonic Pi repo.

## Translating the Qt GUI

- Sign up with [github](https://help.github.com/categories/bootcamp/)
- Fork the [Sonic Pi repo](https://github.com/samaaron/sonic-pi)
  to your own github repo, `git clone` it to your computer
- [Build Sonic Pi](./INSTALL.md),
  first the server extensions, then the Qt GUI.
- `cd app/gui/qt`
- Want to add a new language to the Qt GUI? Then first add a reference
  to the new language file to `SonicPi.Pro` and `SonicPi.qrc`, then run
  `lupdate -pro SonicPi.pro` to have the new .ts file created for you.
- Edit the translation with Qt Linguist,
  `linguist lang/sonic-pi_<LANG>.ts`.
- Build a new binary, test it.
- When you're happy, `git add` if you added a new language, then
  `git commit`, `git push` to your github repo.
- [Send a pull request](https://help.github.com/articles/creating-a-pull-request/) to the Sonic Pi repo.

## Adding a new translation string to the Qt GUI

Messages you want to have translated need to be marked with `tr()`
in the source.

If you added or changed a translation string during development,
don't forget to run `lupdate -pro SonicPi.pro` afterwards to update
the `.ts` files.

Then push them back to github and ask the translators to pull and
translate them.

(The translation workflow will hopefully become much easier once
Transifex is integrated.)

## To-Do

- Transifex integration
