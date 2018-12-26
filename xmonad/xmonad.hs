import XMonad
import Data.Monoid
import System.Exit

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.DynamicProjects
import XMonad.Actions.UpdatePointer

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run

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

-- Projects
workProject :: String -> Project
workProject ws = Project { projectName = ws
                         , projectDirectory = "~/"
                         , projectStartHook = Just $ do
                             spawn myBrowser
                             rit' "ranger"
                         }

projects :: [Project]
projects = 
  [ Project { projectName = wsSys
            , projectDirectory = "~/"
            , projectStartHook = Just $ do rit' "glances"
            }
  ] ++ map workProject [ wsWk1, wsWk2, wsWk3 ]

---
--- derived from https://gist.github.com/Tzbob/7362371
---

myTerminal = "urxvt"
myBrowser = "firefox"

myFocusFollowsMouse = True

myWorkspaces = [wsWk1, wsWk2, wsWk3, wsGen, wsCom, wsDoc, wsAV, wsTmp, wsSys]
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
myModMask = mod4Mask

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info --font=terminus"
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

myKeys conf = let
  -- from https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmonad.hs
  subKeys str ks = subtitle str : ks
  in
  subKeys "system"
    [ ((myModMask, xK_q), addName "Restart and recompile xmonad" $ spawn "xmonad --recompile; xmonad --restart")
    , ((myModMask, xK_F8), addName "Select display" $ spawn "screenselect")
    -- Volume
    , ((0, 0x1008ff11), addName "Reduce volume" $ spawn "amixer -q set Master 5- unmute")
    , ((0, 0x1008ff13), addName "Increase volume" $ spawn "amixer -q set Master 5+ unmute")
    , ((0, 0x1008ff12), addName "Mute volume" $ spawn "amixer set Master toggle")
    ] ^++^

  -- Programs
  subKeys "launchers"
    [ ((myModMask .|. shiftMask, xK_Return), addName "Terminal" $ spawn myTerminal)
    , ((myModMask, xK_b), addName "Browser" $ spawn myBrowser)
    , ((myModMask, xK_k), addName "Khal" $ rit' "khal")
    , ((myModMask, xK_m), addName "Mutt" $ rit' "mutt")
    , ((myModMask, xK_r), addName "Ranger" $ rit' "ranger")
    , ((myModMask, xK_backslash), addName "Password" $ spawn "passmenu")
    ] ^++^

  subKeys "layouts"
    [ ((myModMask, xK_space ), addName "Change layout" $ sendMessage NextLayout)
    , ((myModMask, xK_Return), addName "Swap master" $ windows W.swapMaster)
    , ((myModMask, xK_t), addName "Push window pack to tiling" $ withFocused $ windows . W.sink) 
    , ((myModMask .|. shiftMask, xK_c), addName "Close window" $ kill)
    , ((myModMask, xK_y), addName "Hide status bar" $ sendMessage ToggleStruts)
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
  xmonad
    $ dynamicProjects projects
    $ ewmh 
    $ addDescrKeys ((myModMask, xK_F1), showKeybindings) myKeys
    $ myConfig statusPipe

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

rit n c = runInTerm ("-name " ++ n) c
rit' c = rit c c

