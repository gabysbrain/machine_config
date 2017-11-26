import XMonad
import Data.Monoid
import System.Exit

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.UpdatePointer

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

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

myWorkspaces = ["1 sh","2 ed","3 www","4 mail", "5 doc"]
myMainColor = "#333333"
myBgColor = "#FEFEFE"
myTextcolor = "#282828"
myLowColor = "#999999"

myBorderWidth = 1
myNormalBorderColor = myTextcolor
myFocusedBorderColor = "#75b92d"

-- Key bindings. Add, modify or remove key bindings here.
-------------------------------------------------------------------------------
myKeys x = M.union (M.fromList (customKeys x)) (keys defaultConfig x)

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
  , ((0                 , 0x1008ff11), spawn "amixer -q set Master 5- unmute")
  , ((0                 , 0x1008ff13), spawn "amixer -q set Master 5+ unmute")
  , ((0                 , 0x1008ff12), spawn "amixer set Master toggle")

  -- Cover the status bar gap
  , ((modm, xK_b), sendMessage ToggleStruts)

  -- Programs
  , ((modm, xK_w), spawn "chromium-browser")

  -- Restart xmonad
  , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
  ]

-- Layouts
------------------------------------------------------------------------
myLayout = smartBorders $ avoidStruts $ smartSpacing 2 $ tiled ||| Mirror tiled ||| Full
  where
    tiled = ResizableTall 1 0.03 0.5 []

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------
myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Chromium-browser" --> doShift "3 www"
    , className =? "Gvim" --> doShift "2 ed"
    ]

-- Event handling
-------------------------------------------------------------------------------
myEventHook = mconcat
  [ docksEventHook -- this is needed to properly get xmobar struts working
  , handleEventHook defaultConfig
  ]

-- Status bars and logging
-------------------------------------------------------------------------------
addPad = wrap " " " "

myPP statusPipe = xmobarPP {
    ppOutput = hPutStrLn statusPipe
    , ppCurrent = xmobarColor myMainColor myBgColor . addPad
    , ppHiddenNoWindows = xmobarColor myLowColor "" . addPad
    , ppHidden = xmobarColor myTextcolor "" . addPad
    , ppTitle = xmobarColor myTextcolor ""
    , ppSep = xmobarColor myMainColor myBgColor "  |  "
}

myLogHook pipe = dynamicLogWithPP (myPP pipe)  -- >> updatePointer (Relative 0.5 0.5)

-- Startup hook
-------------------------------------------------------------------------------
myStartupHook = setWMName "LG3D"

-- Configuration structure
-------------------------------------------------------------------------------
defaults statusPipe = ewmh defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

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
    xmonad $ defaults statusPipe

