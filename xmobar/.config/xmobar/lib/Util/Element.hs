module Util.Element where

import Xmobar

import qualified Util.Variable as V

twoAction :: String -> String -> String -> String
twoAction command1 command2 = oneAction command1 . action command2 3

oneAction :: String -> String -> String
oneAction command = action command 1

-- | Pad a string with a leading and trailing space.
pad :: String -> String
pad = wrap " " " "

-- | Use xmobar escape codes to output a string with the font at the given index
font
    :: Int
    -- ^ index: index of the font to use (0: standard font)
    -> String
    -- ^ output string
    -> String
font index = wrap ("<fn=" ++ show index ++ ">") "</fn>"

fgColor :: String -> String -> String
fgColor fg = color fg "#2E3440:0"

-- | Use xmobar escape codes to output a string with given foreground
--   and background colors.
color
    :: String
    -- ^ foreground color: a color name, or #rrggbb format
    -> String
    -- ^ background color
    -> String
    -- ^ output string
    -> String
color fg bg = wrap t "</fc>"
    where
        t = concat ["<fc=", fg, if null bg then "" else "," ++ bg, ">"]

-- | Encapsulate text with an action. The text will be displayed, and the
-- action executed when the displayed text is clicked. Illegal input is not
-- filtered, allowing xmobar to display any parse errors. Uses xmobar's new
-- syntax wherein the command is surrounded by backticks.
action
    :: String
    -- ^ Command. Use of backticks (`) will cause a parse error.
    -> Int
    -- ^ Buttons 1-5, such as "145". Other characters will cause a
    -- parse error.
    -> String
    -- ^ Displayed/wrapped text.
    -> String
action command button = wrap l r
    where
        l = "<action=`" ++ command ++ "` button=" ++ show button ++ ">"
        r = "</action>"

-- | Use xmobar box to add a border to an arbitrary string.
border
    :: String
    -- ^ Border type. Possible values: VBoth, HBoth, Full,
    -- Top, Bottom, Left or Right
    -> String
    -- ^ color: a color name, or #rrggbb format
    -> Int
    -- ^ width in pixels
    -> String
    -- ^ output string
    -> String
border border color width = wrap prefix "</box>"
    where
        prefix =
            "<box type="
                ++ border
                ++ " width="
                ++ show width
                ++ " color="
                ++ color
                ++ ">"

space :: Int -> String
space pixels = "<hspace=" ++ show pixels ++ "/>"

spaceWrap :: Int -> Int -> String -> String
spaceWrap lpixels rpixels = wrap (space lpixels) (space rpixels)

spaceWrapLeft :: Int -> String -> String
spaceWrapLeft lpixels = wrap (space lpixels) ""

spaceWrapRight :: Int -> String -> String
spaceWrapRight rpixels = wrap "" (space rpixels)

-- | Wrap a string in delimiters, unless it is empty.
wrap
    :: String
    -- ^ left delimiter
    -> String
    -- ^ right delimiter
    -> String
    -- ^ output string
    -> String
wrap _ _ "" = ""
wrap l r m = l ++ m ++ r