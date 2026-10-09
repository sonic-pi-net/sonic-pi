// SPDX-License-Identifier: AGPL-3.0-or-later
// A button that asks twice, for what cannot be brought back (deleting a set, clearing the Threads record): the first
// press arms it, its second label in the danger colour (style.css .armed), and the second press does it. Left armed it
// settles back after `settle` ms, or as soon as focus leaves it; with no `settle` it stays armed until pressed again.
// The label is the button's .ask-label where it has one (beside an icon), else its whole text.
export function askTwice(button, { label, armedLabel, act, settle = null }) {
  const text = button.querySelector(".ask-label") ?? button;
  let timer = null;
  const disarm = () => {
    clearTimeout(timer);
    timer = null;
    button.classList.remove("armed");
    text.textContent = label;
  };
  button.addEventListener("click", () => {
    if (!button.classList.contains("armed")) {
      button.classList.add("armed");
      text.textContent = armedLabel;
      if (settle != null) timer = setTimeout(disarm, settle);
      return;
    }
    disarm();
    act();
  });
  if (settle != null) button.addEventListener("blur", disarm);
}
