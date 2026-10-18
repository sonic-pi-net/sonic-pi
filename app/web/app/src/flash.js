// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
// A run's flashes, as every renderer of them draws them (editor.js flashField, ui/card.js flash), and as native's do
// (app/gui/utils/lineflashes.h, flash_style.h): the runtime says which lines (its flash records, main.js onRecord),
// each lit behind its code, held FLASH_HOLD_MS after its latest flash, then gone. Any number lit at once.

export const FLASH_HOLD_MS = 150;
export const FLASH_BRIGHTNESS = 35;   // Flash Brightness's default, a percent: how strongly the wash lies (theme.js)

/** Where a line's code is in its text, [from, to]: its indent and any space after it left out. Null for none. */
export function codeSpan(text) {
  const from = text.search(/\S/);
  return from < 0 ? null : [from, text.trimEnd().length];
}
