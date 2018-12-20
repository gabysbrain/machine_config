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
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import XMonad.Util.Run(spawnPipe)
import System.IO(hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map as M

---
--- derived from https://gist.github.com/Tzbob/7362371
---

myTerminal :: String
myTerminal = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myModMask = mod4Mask

myWorkspaces = ["1 sh","2 ed","3 www","4 mail", "5 doc", "6 ."]
myMainColor = "#333333"
myBgColor = "#FEFEFE"
myTextColor = "#282828"
myLowColor = "#999999"
greenColor = "#75b92d"

myActiveColor = greenColor
myInactiveColor = myTextColor

myBorderWidth = 0
myTopBarHeight = 5
myGap = 10

myTopBar = def
  { inactiveBorderColor = myInactiveColor
  , inactiveColor = myInactiveColor
  , inactiveTextColor = myInactiveColor
  , activeBorderColor = myActiveColor
  , activeColor = myActiveColor
  , activeTextColor = myActiveColor
  , decoHeight = myTopBarHeight
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
myLayout = smartBorders $ avoidStruts $ tiled ||| bsp ||| tabbed 
  where
    tiled = topbar $ spacing myGap $ ResizableTall 1 0.03 0.5 []
    tabbed = fsSpacing myGap $ simpleTabbed
    bsp = topbar $ spacing myGap $ emptyBSP
    grid = topbar $ spacing myGap $ Grid
    spacing x = spacingRaw False (Border 0 0 0 0) False (Border x x x x) True
    fsSpacing x = spacingRaw False (Border x x x x) True (Border 0 0 0 0) False
    topbar = noFrillsDeco shrinkText myTopBar

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------
myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat
    -- special desktops
    , className =? "Chromium-browser" --> doShift "3 www"
    , className =? "Firefox" --> doShift "3 www"
    , className =? "Gvim" --> doShift "2 ed"
    , className =? "okular" --> doShift "5 doc"

    -- floating windows
    , className =? "Pinentry" --> doFloat
    , className =? "Qpdfview" --> doFloat
    ]

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

