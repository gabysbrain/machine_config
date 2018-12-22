import XMonad
import Data.Monoid
import System.Exit

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.UpdatePointer

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import XMonad.Util.NamedActions
import XMonad.Util.Run(spawnPipe)

import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- Workspaces
wsGen = "Gen"
wsWk1 = "Wk1"
wsWk2 = "Wk2"
wsWk3 = "Wk3"
wsCom = "Com"
wsDoc = "Doc"
wsAV  = "AV"
wsTmp = "Tmp"
wsSys = "Sys"

---
--- derived from https://gist.github.com/Tzbob/7362371
---

myTerminal :: String
myTerminal = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myModMask = mod4Mask

myWorkspaces = [wsGen,wsWk1,wsWk2,wsWk3, wsCom, wsDoc, wsAV, wsTmp, wsSys]
myMainColor = "#333333"
myBgColor = "#FEFEFE"
myTextColor = "#282828"
myLowColor = "#999999"
greenColor = "#75b92d"

myActiveColor = greenColor
myInactiveColor = myTextColor

myBorderWidth = 0
myTopBarHeight = 5
myTabBarHeight = 13
myGap = 10
myFont = "xft:Anonymous Pro:size=10"

myTopBar = def
  { inactiveBorderColor = myInactiveColor
  , inactiveColor = myInactiveColor
  , inactiveTextColor = myInactiveColor
  , activeBorderColor = myActiveColor
  , activeColor = myActiveColor
  , activeTextColor = myActiveColor
  , decoHeight = myTopBarHeight
  }

myTabBar = def
  { fontName = myFont
  , inactiveBorderColor = myInactiveColor
  , inactiveColor = myInactiveColor
  , inactiveTextColor = myBgColor
  , activeBorderColor = myActiveColor
  , activeColor = myActiveColor
  , activeTextColor = myTextColor
  , decoHeight = myTabBarHeight
  }

-- Key bindings. Add, modify or remove key bindings here.
-------------------------------------------------------------------------------
myKeys x = M.union (M.fromList (customKeys x)) (keys def x)

customKeys conf@(XConfig {XMonad.modMask = modm}) = 

  -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

  -- close focused window
  , ((modm .|. shiftMask, xK_c), kill)

   -- Rotate through the available layout algorithms
  , ((modm, xK_space ), sendMessage NextLayout)

  -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)

  -- Shrink the master area
  , ((modm, xK_h), sendMessage Shrink)

  -- Expand the master area
  , ((modm, xK_l), sendMessage Expand)

  -- Shrink a window
  , ((modm, xK_u), sendMessage MirrorShrink)

  -- Expand a window
  , ((modm, xK_i), sendMessage MirrorExpand)

  -- Push window back into tiling
  , ((modm, xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area
  , ((modm .|. shiftMask, xK_h), sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ((modm .|. shiftMask, xK_l), sendMessage (IncMasterN (-1)))

  -- Volume
  , ((0, 0x1008ff11), spawn "amixer -q set Master 5- unmute")
  , ((0, 0x1008ff13), spawn "amixer -q set Master 5+ unmute")
  , ((0, 0x1008ff12), spawn "amixer set Master toggle")

  -- Select display
  , ((modm, xK_F8), spawn "screenselect")

  -- Cover the status bar gap
  , ((modm, xK_y), sendMessage ToggleStruts)

  -- Programs
  , ((modm, xK_b), spawn "firefox")
  , ((modm, xK_k), spawn "urxvt -e khal")
  , ((modm, xK_m), spawn "urxvt -e mutt")
  , ((modm, xK_r), spawn "urxvt -e ranger")

  -- Restart xmonad
  , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
  ]

-- Layouts
------------------------------------------------------------------------
myLayout = smartBorders $ avoidStruts $ tiledL ||| bspL ||| tabbedL 
  where
    tiledL = named "Tiled"
      $ topbar 
      $ spacing myGap 
      $ ResizableTall 1 0.03 0.5 []
    tabbedL = named "Tabbed"
      $ fsSpacing myGap 
      $ tabbed shrinkText myTabBar
    bspL = named "BSP" 
      $ topbar 
      $ spacing myGap 
      $ emptyBSP
    gridL = named "Grid" 
      $ topbar 
      $ spacing myGap 
      $ Grid

    spacing x = spacingRaw False (Border 0 0 0 0) False (Border x x x x) True
    fsSpacing x = spacingRaw False (Border x x x x) True (Border 0 0 0 0) False
    topbar = noFrillsDeco shrinkText myTopBar
    named n = renamed [(XMonad.Layout.Renamed.Replace n)]

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------
myManageHook = manageDocks <+> composeOne
    [ isFullscreen -?> doFullFloat
    , isBrowserDialog -?> forceCenterFloat
    , isRole =? "pop-up" -?> forceCenterFloat

    -- floating windows
    , className =? "Pinentry" -?> doFloat
    , className =? "Qpdfview" -?> doFloat
    ]
  where
  isBrowserDialog = isDialog <&&> (className =? "Chromium-browser" <||> className =? "Firefox")
  isRole = stringProperty "WM_WINDOW_ROLE"

-- Event handling
-------------------------------------------------------------------------------
myEventHook = mconcat
  [ docksEventHook -- this is needed to properly get xmobar struts working
  , handleEventHook def
  ]

-- Status bars and logging
-------------------------------------------------------------------------------
addPad = wrap " " " "

myPP statusPipe = xmobarPP {
    ppOutput = hPutStrLn statusPipe
  , ppCurrent = xmobarColor myMainColor myBgColor . addPad
  , ppHiddenNoWindows = xmobarColor myLowColor "" . addPad
  , ppHidden = xmobarColor myTextColor "" . addPad
  , ppTitle = xmobarColor myTextColor ""
  , ppSep = xmobarColor myMainColor myBgColor "  |  "
}

myLogHook pipe = dynamicLogWithPP (myPP pipe)  -- >> updatePointer (Relative 0.5 0.5)


-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- Startup hook
-------------------------------------------------------------------------------
myStartupHook = setWMName "LG3D"

-- Configuration structure
-------------------------------------------------------------------------------
myConfig statusPipe = def {
  -- simple stuff
  terminal           = myTerminal,
  focusFollowsMouse  = myFocusFollowsMouse,
  borderWidth        = myBorderWidth,
  modMask            = myModMask,
  workspaces         = myWorkspaces,

  -- bindings
  keys               = myKeys,

  -- hooks, layouts
  layoutHook         = myLayout,
  manageHook         = myManageHook,
  handleEventHook    = myEventHook,
  logHook            = myLogHook statusPipe,
  startupHook        = myStartupHook
}

-- Run xmonad with the settings specified. No need to modify this.
-------------------------------------------------------------------------------
main = do
  statusPipe <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad . ewmh $ myConfig statusPipe
    -- $ addDescrKeys ((myModMask, xK_F1), showKeybindings) myKeys

-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2

