# Translating Sonic Pi

At present, you can translate the Qt GUI, only.

Translations are located in
[`app/gui/qt/lang/sonic-pi_<LANG>.ts`](https://github.com/samaaron/sonic-pi/tree/master/app/gui/qt/lang).

## Editing an existing translation

If you are unhappy with a translation, checkout the Sonic Pi
repository with git and edit the translation file with Qt Linguist:

`linguist lang/sonic-pi_de.ts`

When you're finished, commit the result and file a pull request.

## Adding a new language

Add a reference to the new language file to
[`app/gui/qt/SonicPi.Pro`](https://github.com/samaaron/sonic-pi/blob/master/app/gui/qt/SonicPi.pro)
and
[`app/gui/qt/SonicPi.qrc`](https://github.com/samaaron/sonic-pi/blob/master/app/gui/qt/SonicPi.qrc).

Then run `lupdate --pro SonicPi.pro` to have the new `.ts` file created
for you.

Finally, edit the translation with Qt Linguist (see above), commit and
have it pulled.

## Adding a new translation string

Messages you want to have translated need to be marked with `tr()`
in the source.

If you added or changed a translation string during development,
don't forget to run `lupdate --pro SonicPi.pro` afterwards to update
the `.ts` files.

Then push them back to github and ask the translators to pull and
translate them.

(The translation workflow will hopefully become much easier once
Transifex is integrated.)

## To-Do

- tutorial translation
- Transifex integration
