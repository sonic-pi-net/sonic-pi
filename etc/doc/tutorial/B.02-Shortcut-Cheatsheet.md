B.2 Shortcut Cheatsheet

# Shortcut Cheatsheet

The following is a summary of the main shortcuts available within Sonic
Pi. Please see Section B.1 for motivation and background.

## Conventions

In this list, we use the following conventions (where *Meta* is one of *Alt* on
Windows/Linux or *Cmd* on Mac):

* `C-a`   means hold the *Control* key then press the *a* key whilst holding them both at the same time, then releasing.
* `M-r`   means hold the *Meta* key and then press the *r* key whilst holding them both at the same time, then releasing.
* `S-M-z` means hold the *Shift* key, then the *Meta* key, then finally the *z* key all at the same time, then releasing.
* `C-M-f` means hold the *Control* key, then press *Meta* key, finally the *f* key all at the same time, then releasing.

## Main Application Manipulation

* `M-r`     - Run code
* `M-s`     - Stop code
* `M-i`     - Toggle Help System
* `M-p`     - Toggle Preferences
* `M-{`     - Switch buffer to the left
* `M-}`     - Switch buffer to the right
* `S-M-0`   - Switch to buffer 0
* `S-M-1`   - Switch to buffer 1
* ...
* `S-M-9`   - Switch to buffer 9
* `M-+`     - Increase text size of current buffer
* `M--`     - Decrease text size of current buffer


## Selection/Copy/Paste

* `M-a`     - Select all
* `M-c`     - Copy selection to paste buffer
* `M-]`     - Copy selection to paste buffer
* `M-x`     - Cut selection to paste buffer
* `C-]`     - Cut selection to paste buffer
* `C-k`     - Cut to the end of the line
* `M-v`     - Paste from paste buffer to editor
* `C-y`     - Paste from paste buffer to editor
* `C-SPACE` - Set mark. Navigation will now manipulate highlighted region. Use `C-g` to escape.

## Text Manipulation

* `M-m`     - Align all text
* `Tab`     - Align current line or selection (or select autocompletion)
* `C-l`     - Centre editor
* `M-/`     - Comment/Uncomment current line or selection
* `C-t`     - Transpose/swap characters
* `M-u`     - Convert next word (or selection) to upper case.  
* `M-l`     - Convert next word (or selection) to lower case.  

## Navigation

* `C-a`     - Move to beginning of line
* `C-e`     - Move to end of line
* `C-p`     - Move to previous line
* `C-n`     - Move to next line
* `C-f`     - Move forward one character
* `C-b`     - Move backward one character
* `M-f`     - Move forward one word
* `M-b`     - Move backward one word
* `C-M-n`   - Move line or selection down
* `C-M-p`   - Move line or selection up
* `S-M-u`   - Move up 10 lines
* `S-M-d`   - Move down 10 lines
* `M-<`     - Move to beginning of buffer
* `M->`     - Move to end of buffer

## Deletion

* `C-h`     - Delete previous character
* `C-d`     - Delete next character

## Advanced Editor Features

* `C-i`     - Show docs for word under cursor
* `M-z`     - Undo
* `S-M-z`   - Redo
* `C-g`     - Escape
* `S-M-f`   - Toggle fullscreen mode
* `S-M-b`   - Toggle visibility of buttons
* `S-M-l`   - Toggle visibility of log
* `S-M-m`   - Toggle between light/dark modes
* `S-M-s`   - Save contents of buffer to a file
* `S-M-o`   - Load contents of buffer from a file
