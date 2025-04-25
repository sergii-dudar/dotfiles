module Module.Layout where

import qualified Module.Variable as V
import XMonad

import XMonad.Layout.Accordion
import XMonad.Layout.Gaps
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
import XMonad.Layout.SubLayouts
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation

import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import qualified XMonad.Layout.Tabbed as Tabbed

gapsConf :: Integer -> Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
gapsConf s w = spacingRaw False (Border s s s s) True (Border w w w w) True

gapsDef :: l a -> ModifiedLayout Spacing l a
gapsDef = gapsConf 4 6

layoutsTabConfig =
    def
        { Tabbed.fontName = V.fontsDefault
        , Tabbed.activeColor = "#232634" -- color15
        , Tabbed.inactiveColor = "#1c1e29"
        , Tabbed.activeTextColor = "#bd93f9"
        , Tabbed.inactiveTextColor = "#7C8377"
        , Tabbed.activeBorderWidth = 0
        , Tabbed.inactiveBorderWidth = 0
        , Tabbed.activeBorderColor = "#005577"
        , Tabbed.inactiveBorderColor = "#005577"
        , Tabbed.decoHeight = 30
        }

layoutsRatioVar = 1 / 2 -- Default proportion of screen occupied by master pane
layoutsDeltaVar = 3 / 100 -- Percent of screen to increment by when resizing panes

-- ############# Main Layout ############

layoutsTall =
    gapsDef $
        Tall 1 layoutsDeltaVar layoutsRatioVar
layoutsFull = gapsDef Full
layoutsTab = noBorders (tabbed shrinkText layoutsTabConfig)

-- ############# Stack Layouts (second vertical screen) ############

layoutsStack = gapsDef $ ResizableTall 0 layoutsDeltaVar layoutsRatioVar []
layoutsStackMaster = gapsDef $ StackTile 1 layoutsDeltaVar layoutsRatioVar