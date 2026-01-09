# VIM CHEAT SHEET

- Learn one new command per day rather than trying to memorize everything at once
- Practice the most common operations: navigation, insertion, deletion, and saving
- Use **:help** extensively - Vim's built-in documentation is excellent

usefull links:

- [good basic motions advises](https://github.com/unblevable/quick-scope?tab=readme-ov-file#other-motions)

`h Ex-commands`

## ============================== Modes ==============================

### Mode Switching

| Command              | Description       |
| -------------------- | ----------------- |
| `Esc`                | Normal mode       |
| `v`                  | Visual mode       |
| `V`                  | Visual line mode  |
| `Ctrl+v` or `Ctrl+q` | Visual block mode |

### Entering Insert Mode from Normal Mode

| Command | Description                    |
| ------- | ------------------------------ |
| `i`     | Insert mode                    |
| `I`     | Insert at line start           |
| `a`     | Append after cursor            |
| `A`     | Append at line end             |
| `o`     | New line below                 |
| `O`     | New line above                 |
| `s`     | Delete char and insert         |
| `S`     | Delete line and insert         |
| `gi`    | Insert at last insert position |

### Insert Mode Operations

| Command  | Description                                                                |
| -------- | -------------------------------------------------------------------------- |
| `Ctrl+h` | Delete the character before the cursor during insert mode                  |
| `Ctrl+w` | Delete word before the cursor during insert mode                           |
| `Ctrl+j` | Add a line break at the cursor position during insert mode                 |
| `Ctrl+t` | Indent (move right) line one shiftwidth during insert mode                 |
| `Ctrl+d` | De-indent (move left) line one shiftwidth during insert mode               |
| `Ctrl+n` | Insert (auto-complete) next match before the cursor during insert mode     |
| `Ctrl+p` | Insert (auto-complete) previous match before the cursor during insert mode |
| `Ctrl+o` | Temporarily enter normal mode to issue one normal-mode command             |
| ``       |                                                                            |

### Other Editing

| Command      | Description                              |
| ------------ | ---------------------------------------- |
| `g~`         | switch case up to motion or selected     |
| `gu`         | change to lowercase up to motion         |
| `gU`         | change to uppercase up to motion         |
| `cc`         | change (replace) entire line             |
| `c$` or `C`  | change (replace) to the end of the line  |
| `cw` or `ce` | change (replace) to the end of the word  |
| `xp`         | transpose two letters (delete and paste) |
| ``           |                                          |

---

## ============================== Navigation ==============================

### Basic Cursor Movement

| Command   | Description           |
| --------- | --------------------- |
| `h j k l` | Left, down, up, right |

### Word Cursor Movement

| Command | Description                                                         |
| ------- | ------------------------------------------------------------------- |
| `w`     | Word forward                                                        |
| `b`     | Word backward                                                       |
| `e`     | End of word                                                         |
| `W`     | WORD forward (words can contain punctuation)                        |
| `B`     | WORD backward (words can contain punctuation)                       |
| `E`     | End of WORD (words can contain punctuation)                         |
| `ge`    | Jump backwards to the end of a word                                 |
| `gE`    | Jump backwards to the end of a word (words can contain punctuation) |

### Line Cursor Movement

| Command | Description          |
| ------- | -------------------- |
| `0`     | Line start           |
| `^`     | First non-blank char |
| `$`     | Line end             |
| `g_`    | Last non-blank char  |

### Screen Cursor Movement

| Command | Description      |
| ------- | ---------------- |
| `H`     | Top of screen    |
| `M`     | Middle of screen |
| `L`     | Bottom of screen |

### Other Cursor Movements

| Command        | Description                                                       |
| -------------- | ----------------------------------------------------------------- |
| `}`            | Jump to next paragraph (or function/block, when editing code)     |
| `{`            | Jump to previous paragraph (or function/block, when editing code) |
| `%`            | Nearest/matching {[()]}                                           |
| `[(` `[{` `[<` | Previous ( or {                                                   |
| `])` `]{` `]>` | Next ) or }                                                       |
| ``             |                                                                   |

### File Navigation

| Command  | Description       |
| -------- | ----------------- |
| `gg`     | Go to top         |
| `G`      | Go to bottom      |
| `[N]gg`  | Go to line number |
| `[N]G`   | Go to line number |
| `:N`     | Go to line number |
| `Ctrl+f` | Page down         |
| `Ctrl+b` | Page up           |
| `Ctrl+d` | Half page down    |
| `Ctrl+u` | Half page up      |

### Screen position from Cursor

| Command | Description                             |
| ------- | --------------------------------------- |
| `zz`    | center cursor on screen                 |
| `zt`    | position cursor on top of the screen    |
| `zb`    | position cursor on bottom of the screen |
| ``      |                                         |

### Jump Navigation

| Command  | Description                                  |
| -------- | -------------------------------------------- |
| `Ctrl+o` | Jump back                                    |
| `Ctrl+i` | Jump forward                                 |
| `g;`     | Go to previous change position               |
| `g,`     | Go to next change position                   |
| `` `. `` | Go to last change in current buffer          |
| `` `^ `` | Go to last position of cursor in insert mode |

### Character Motions

| Command   | Description              |
| --------- | ------------------------ |
| `f{char}` | Find character forward   |
| `F{char}` | Find character backward  |
| `t{char}` | To character forward     |
| `T{char}` | To character backward    |
| `;`       | Repeat character motion  |
| `,`       | Reverse character motion |

## ============================== Editing ==============================

### Delete & Change

| Command     | Description               |
| ----------- | ------------------------- |
| `x`         | Delete character          |
| `X`         | Delete char before cursor |
| `dd`        | Delete line               |
| `dw`        | Delete word               |
| `d$`        | Delete to end of line     |
| `d0`        | Delete to start of line   |
| `D`         | Delete to end of line     |
| `cw`        | Change word               |
| `cc` or `S` | Change line               |
| `r`         | Replace character         |

### Copy & Paste

| Command | Description           |
| ------- | --------------------- |
| `yy`    | Copy line             |
| `yw`    | Copy word             |
| `y$`    | Copy to end of line   |
| `y0`    | Copy to start of line |
| `p`     | Paste after           |
| `P`     | Paste before          |

### Other Change Combinations

| Command   | Description                |
| --------- | -------------------------- |
| `d/hello` | Delete until hello         |
| `>j`      | Indent 2 lines             |
| `ggdG`    | Delete a complete document |
| `gg=G`    | Indent a complete document |
| `ggyG`    | Copy a whole document      |
| ``        |                            |

### Undo & Redo

| Command  | Description       |
| -------- | ----------------- |
| `u`      | Undo              |
| `Ctrl+r` | Redo              |
| `U`      | Undo line changes |

## ============================== Indent ==============================

| Command | Description                                       |
| ------- | ------------------------------------------------- |
| `>>`    | indent (move right) line one shiftwidth           |
| `<<`    | de-indent (move left) line one shiftwidth         |
| `>%`    | indent a block with () or {} (cursor on brace)    |
| `<%`    | de-indent a block with () or {} (cursor on brace) |
| `>ib`   | indent inner block with ()                        |
| `>at`   | indent a block with <> tags                       |
| `3==`   | re-indent 3 lines                                 |
| `=%`    | re-indent a block with () or {} (cursor on brace) |
| `=iB`   | re-indent inner block with {}                     |
| `gg=G`  | re-indent entire buffer                           |
| `]p`    | paste and adjust indent to current line           |
| ``      |                                                   |

## ============================== Text Objects ==============================

| Command   | Description           |
| --------- | --------------------- |
| `p`       | Paragraph             |
| `w`       | Word                  |
| `W`       | WORD                  |
| `s`       | Sentence              |
| `[({<`    | A [], (), or {} block |
| `])}>`    | A [], (), or {} block |
| `` '"` `` | A quoted string       |
| `b`       | A block [(            |
| `B`       | A block in [{         |
| `t`       | A HTML tag block      |
| ``        |                       |

### Word & Paragraph

| Command | Description         |
| ------- | ------------------- |
| `iw`    | Inner word          |
| `aw`    | A word (with space) |
| `is`    | Inner sentence      |
| `as`    | A sentence          |
| `ip`    | Inner paragraph     |
| `ap`    | A paragraph         |

### Brackets & Quotes

| Command | Description   |
| ------- | ------------- |
| `i(`    | Inner block   |
| `a(`    | A block       |
| `ib`    | Inner block   |
| `ab`    | A block       |
| `i"`    | Inside quotes |
| `a"`    | Around quotes |

### Useful Combinations

| Command | Description              |
| ------- | ------------------------ |
| `ciw`   | Change inner word        |
| `ci"`   | Change inner quotes      |
| `cit`   | Change inner tags (HTML) |
| `cip`   | Change inner paragraph   |
| `yip`   | Yank inner paragraph     |
| `yap`   | Yank around paragraph    |
| `di"`   | Delete inside quotes     |
| `ya(`   | Yank around block        |
| `>ip`   | Indent paragraph         |
| `=G`    | Auto-indent to end       |
| `diw`   | Delete inner word        |
| `dis`   | Delete inner sentence    |
| `di"`   | Delete in quotes         |
| `da"`   | Delete around quotes     |
| `dip`   | Delete a paragraph       |
| ``      |                          |

## ============================== Registers ==============================

### Named Registers

| Command | Description           |
| ------- | --------------------- |
| `"ay`   | Yank to register a    |
| `"ap`   | Paste from register a |
| `"Ay`   | Append to register a  |

### Special Registers

| Command | Description         |
| ------- | ------------------- |
| `"+`    | System clipboard    |
| `"*`    | Selection clipboard |
| `"0`    | Last yank           |
| `"1`    | Last delete         |
| `"_`    | Black hole register |
| `:reg`  | Show all registers  |

## Search & Replace

### Searching

| Command    | Description                     |
| ---------- | ------------------------------- |
| `/pattern` | Search forward                  |
| `?pattern` | Search backward                 |
| `n`        | Next match                      |
| `N`        | Previous match                  |
| `*`        | Search word under cursor        |
| `#`        | Search word under cursor (back) |

### Replacing / Substitute :[range]s/{pattern}/{str}/[flags] | :[range]s/{pattern}/{str}/[flags]

| Command           | Description                       |
| ----------------- | --------------------------------- |
| `:s/old/new/`     | Replace first on line             |
| `s/old/new/i`     | Ignore case replace first on line |
| `:s/old/new/g`    | Replace all on line               |
| `:s/old/new/gc`   | Replace all on line (Configm)     |
| `:s/\vold/new/g`  | Replace all with regex            |
| `:2,6s/old/new/g` | Replace between lines 2-6         |
| ``                |                                   |
| `:%s/old/new`     | Replace first in file             |
| `:%s/old/new/g`   | Replace all in file               |
| `:%s/old/new/gc`  | Replace all in file (Confirm)     |
| `:%s/old/new/gi`  | Replace all (ignore case)         |
| `:%s/\vold/new/g` | Replace all with regex            |
| ``                |                                   |

### Replacing / Substitute expression

| Command   | Description                    |
| --------- | ------------------------------ |
| `&` `\0`  | Replace with the whole matched |
| `\1...\9` | Replace with the group 0-9     |
| `\u`      | Uppercase next letter          |
| `\U`      | Uppercase following characters |
| `\l`      | Lowercase next letter          |
| `\L`      | Lowercase following characters |
| `\e`      | End of \u, \U, \l and \L       |
| `\E`      | End of \u, \U, \l and \L       |
| ``        |                                |

#### Flags:

- `g` - Replace all occurrences
- `i` - Ignore case
- `I` - Don't ignore case
- `c` - Confirm each substitution

#### Examples:

- `:s/a\|b/xxx\0xxx/g` --- ( Modifies "a b" to "xxxaxxx xxxbxxx" )
- `:s/test/\U& file/` --- ( Modifies "test" to "TEST FILE" )
- `:s/\(test\)/\U\1\e file/` --- ( Modifies "test" to "TEST file" )
- `:s/\v([abc])([efg])/\2\1/g` --- ( Modifies "af fa bg" to "fa fa gb" )
- `:s/\v\w+/\u\0/g` --- ( Modifies "bla bla" to "Bla Bla" )
- `:s/\v([ab])|([cd])/\1x/g` --- ( Modifies "a b c d" to "ax bx x x" )
- `:%s/.*/\L&/` --- ( Modifies "HTML" to "html" )
- `:s/\v<(.)(\w*)/\u\1\L\2/g` --- ( Make every first letter of a word uppercase )
- `:%s/^\(.*\)\n\1/\1/` --- ( Remove duplicate lines )
- `:%s/<\/\=\(\w\+\)\>/\U&/g` --- ( Convert HTML-Tags to uppercase )
- `:g/^pattern/s/$/mytext` --- ( Find and append text to the end )
- `:g/pattern/norm! @i` --- ( Run a macro on matching lines )
- `/^\(.*\)\(\r\?\n\1\)\+$` --- ( View the duplicates lines )
- `/\v^(.*)(\r?\n\1)+$` --- ( View the duplicates lines (very magic) )
- `:v/./,/./-j` --- ( Compress blank lines into a blank line )
- `:g/<p1>/,/<p2>/d` --- ( Delete inclusively from <p1> to <p2> )

### Ranges

| Command | Description       |
| ------- | ----------------- |
| `%`     | Entire file       |
| `'<,'>` | Current selection |
| `5`     | Line 5            |
| `5,10`  | Lines 5 to 10     |
| `$`     | Last line         |
| `2,$`   | Lines 2 to Last   |
| `.`     | Current line      |
| `,3`    | Next 3 lines      |
| `-3,`   | Forward 3 lines   |
| ``      |                   |

## ============================== Global Commands (or it also calling - `G command`) ==============================

### Pattern Matching

| Command          | Description                   |
| ---------------- | ----------------------------- |
| `:g/pattern/cmd` | Execute on matching lines     |
| `:v/pattern/cmd` | Execute on non-matching lines |

### Common Examples

| Command               | Description                                         |
| --------------------- | --------------------------------------------------- |
| `:g/pattern/d`        | Delete lines containing pattern                     |
| `:g!/pattern/d`       | Delete lines NOT containing pattern                 |
| `:g/^$/d`             | Delete empty lines                                  |
| `:g/^\s*$/d`          | Delete blank lines                                  |
| `:g/pattern/t$`       | Copy matching lines to end                          |
| `:g/pattern/m$`       | Move matching lines to end                          |
| `:g/pattern/p`        | Print matching lines                                |
| `:g/pattern/#`        | Print with line numbers                             |
| `:g/pattern/norm ...` | Exucute normal mode command                         |
| ``                    |                                                     |
| `:v/foo/d`            | Delete lines not containing foo (also: `:g!/foo/d`) |

## ============================== File Operations ==============================

| Command        | Description                                                          |
| -------------- | -------------------------------------------------------------------- |
| `:w`           | Save file                                                            |
| `:w filename`  | Save as                                                              |
| `:q`           | Quit                                                                 |
| `:q!` or `ZQ`  | Force quit (wihtout saving)                                          |
| `:wq` or `ZZ`  | Save and quit                                                        |
| `:x`           | Save and quit                                                        |
| `:e filename`  | Open file                                                            |
| `:r filename`  | Insert file contents                                                 |
| `:sp filename` | Horizontal split open                                                |
| `:vs filename` | Vertical split open                                                  |
| ``             |                                                                      |
| `gf`           | Open file under cursor (absolute or relative - check `vim.opt.path`) |
| `gx`           | Open link under curson                                               |
| ``             |                                                                      |

## ============================== Shell Commands ==============================

### Execute Commands

| Command      | Description            |
| ------------ | ---------------------- |
| `:!command`  | Run shell command      |
| `:r!command` | Insert command output  |
| `:%!command` | Filter through command |
| `:shell`     | Open shell             |

### Common Examples

| Command                | Description            |
| ---------------------- | ---------------------- |
| `:r!date`              | Insert date            |
| `:%!sort`              | Sort lines             |
| `:%!jq .`              | Format JSON            |
| `:'<,'>!jq .`          | Format selected JSON   |
| `:!wc %`               | Word count             |
| `:sort \| %!uniq -u`   | Remove duplicate lines |
| ``                     |                        |
| `:%w !pbcopy  `        | Mac OS X               |
| `:%w !xclip -i -sel c` | GNU/Linux              |
| ``                     |                        |

### Suspend & Resume

| Command      | Description                                                                          |
| ------------ | ------------------------------------------------------------------------------------ |
| `Ctrl+Z`     | Suspend Neovim to send it to the background, returning you to your shell             |
| `[shell] fg` | In the shell, type fg (foreground) and press Enter to bring Neovim back to the front |
| ``           |                                                                                      |

## ============================== Visual Mode ==============================

### Visual Selection

| Command  | Description                                                        |
| -------- | ------------------------------------------------------------------ |
| `v`      | Character-wise selection                                           |
| `V`      | Line-wise selection                                                |
| `Ctrl+v` | Block-wise selection                                               |
| `o`      | Go to other end (for example to chanche start of visual selection) |
| `O`      | Go to other corner                                                 |
| `gv`     | Reselect last selection                                            |
| `vi"`    | Select inner quotes                                                |
| `va"`    | Select around quotes                                               |
| `vi[`    | Select inner brackets                                              |
| `va[`    | Select around brackets                                             |
| `viw`    | Select inner word                                                  |
| `vip`    | Select inner paragraph                                             |
| `vipip`  | Select more paragraph                                              |
| ``       |                                                                    |

### Operations on Selection

| Command | Description      |
| ------- | ---------------- |
| `d`     | Delete selection |
| `y`     | Copy selection   |
| `c`     | Change selection |
| `U`     | Uppercase        |
| `u`     | Lowercase        |
| `~`     | Change Case      |
| `>`     | Indent right     |
| `<`     | Indent left      |
| `=`     | Auto-indent      |

### Other on Selection

| Command    | Description          |
| ---------- | -------------------- |
| `vU`       | Uppercase character  |
| `vu`       | Lowercase character  |
| `viw U`    | Uppercase word       |
| `viw u`    | Lowercase word       |
| `viw ~`    | Toggle case word     |
| `VU / gUU` | Uppercase line       |
| `Vu / guu` | Lowercase line       |
| `V~ / g~~` | Toggle case line     |
| `gggUG`    | Uppercase all text   |
| `ggguG`    | Lowercase all text   |
| `ggg~G`    | Toggle case all text |
| ``         |                      |

## ============================== Visual block mode ==============================

- append to end of all (even diff sizes): after block select - `$A you test!<Esc>`

## ============================== Norm Command Examples ==============================

- `'<,'>norm 0wi*`
- `'<,'>norm ea*`
- `'<,'>norm! 0wi*^[ea*`
  `^[` it's esc combination Ctrl+[, to get it in command mode, need to press `Ctrl+v+[` or in my case `Ctrl+q+[`
  as ctrl+v is using for pasting in my terminals and can't be used (see `:help c_CTRL-V`)

## ============================== Folding ==============================

### Fold Levels

| Command           | Description                                      |
| ----------------- | ------------------------------------------------ |
| `zo / zO`         | Open                                             |
| `zc / zC`         | Close                                            |
| `za / zA`         | Toggle                                           |
| `zv`              | Open folds for this line                         |
| `zm / zM`         | Close one level (foldlevel += 1) / Close all     |
| `zr / zR`         | Open one level (foldlevel -= 1) / Open all folds |
| `zx`              | Update folds                                     |
| ``                |                                                  |
| `:set fdm=indent` | Fold on indent                                   |
| ``                |                                                  |

### Create & Toggle

| Command | Description |
| ------- | ----------- |
| `zf`    | Create fold |
| `zd`    | Delete fold |

## ============================== Buffers ==============================

| Command | Description      |
| ------- | ---------------- |
| `:ls`   | List buffers     |
| `:b N`  | Switch to buffer |
| `:bn`   | Next buffer      |
| `:bp`   | Previous buffer  |
| `:bd`   | Delete buffer    |

## ============================== Windows ==============================

### Window Splits

| Command    | Description      |
| ---------- | ---------------- |
| `:split`   | Split horizontal |
| `:vsplit`  | Split vertical   |
| `Ctrl+w s` | Split horizontal |
| `Ctrl+w v` | Split vertical   |

### Window Navigation

| Command    | Description             |
| ---------- | ----------------------- |
| `Ctrl+w w` | Switch windows          |
| `Ctrl+w h` | Move to left window     |
| `Ctrl+w j` | Move to bottom window   |
| `Ctrl+w k` | Move to top window      |
| `Ctrl+w l` | Move to right window    |
| `Ctrl+w c` | Close window            |
| `Ctrl+w o` | Close all other windows |
| `Ctrl+w =` | Equalize window sizes   |

### Window Resizing

| Command    | Description     |
| ---------- | --------------- |
| `Ctrl+w +` | Increase height |
| `Ctrl+w -` | Decrease height |
| `Ctrl+w >` | Increase width  |
| `Ctrl+w <` | Decrease width  |

### Window Other Useful

| Command    | Description           |
| ---------- | --------------------- |
| `Ctrl+w p` | Go to prev window     |
| `Ctrl+w x` | Swap window with next |

## ============================== Advanced ==============================

### Marks

| Command       | Description                    |
| ------------- | ------------------------------ |
| `m[a-z]`      | Set local mark                 |
| `m[A-Z]`      | Set global mark                |
| `'[mark]`     | Go to line of mark             |
| `` `[mark] `` | Go to exact position           |
| ``            |                                |
| `ma`          | Mark this cursor position as a |
| `` `a ``      | Jump to the cursor position a  |
| ``            |                                |
| `:marks`      | List all current marks         |
| `:delm a`     | Delete mark a                  |
| `:delm a-d`   | Delete marks a, b, c, d        |
| `:delm abc`   | Delete marks a, b, c           |
| ``            |                                |

### Macros

| Command                 | Description                |
| ----------------------- | -------------------------- |
| `q[a-z]`                | Record macro               |
| `q`                     | Stop recording             |
| `@[a-z]`                | Execute macro              |
| `7@[a-z]`               | Execute macro 7 times      |
| `g/pattern/norm @[a-z]` | Execute macro on selection |
| `@@`                    | Repeat last macro          |

### Repeat & Help

| Command       | Description         |
| ------------- | ------------------- |
| `.`           | Repeat last command |
| `:set nu`     | Show line numbers   |
| `:help [cmd]` | Get help            |

## ============================== Vimdiff ==============================

`vimdiff file1 file2 [file3]`
`vim -d file1 file2 [file3]`

| Command         | Description                      |
| --------------- | -------------------------------- |
| `do / :diffget` | Obtain (get) difference          |
| `dp / :diffput` | Put difference                   |
| `:diffthis`     | make current window part of diff |
| `:dif`          | Re-scan differences              |
| `:diffo`        | Switch off diff mode             |
| `ZQ`            | Quit without changes             |
| ``              |                                  |

## ============================== Emergency ==============================

| Command  | Description           |
| -------- | --------------------- |
| `Esc`    | Return to normal mode |
| `Ctrl+c` | Return to normal mode |
| `:q!`    | Quit without saving   |
| `u`      | Undo mistake          |
| `:help`  | Get help              |

### Increase \ Decrease

| Command    | Description                                          |
| ---------- | ---------------------------------------------------- |
| `Ctrl+a`   | Increase number                                      |
| `Ctrl+x`   | Decrease number                                      |
| `g Ctrl+a` | Step increase visualy selected column numbers number |
| `g Ctrl+x` | Step decrease visualy selected column numbers number |
| ``         |                                                      |

### Tabs

| Command        | Description               |
| -------------- | ------------------------- |
| `gt`           | Go to next tab            |
| `gT`           | Go to previous tab        |
| `2gt`          | Go to tab number 2        |
| ``             |                           |
| `:tab ba`      | Edit all buffers as tabs  |
| `:tabe [file]` | Edit file in a new tab    |
| `:tabf [file]` | Open if exists in new tab |
| `:tabc`        | Close current tab         |
| `:tabo`        | Close other tabs          |
| `:tabr`        | Go to first tab           |
| `:tabl`        | Go to last tab            |
| `:tabm 0`      | Move to position 0        |
| `:tabn`        | Go to next tab            |
| `:tabp`        | Go to previous tab        |
| ``             |                           |

---

## ============================== Template ==============================

### Template

| Command | Description |
| ------- | ----------- |
| ``      |             |
| ``      |             |
| ``      |             |
| ``      |             |
| ``      |             |
| ``      |             |

## ################## Another Interesting Notes

## ============================== Messages History ==============================

since I'm using `folke/noice.nvim`, `messages` command is useless for me, use `NoiceHistory`
