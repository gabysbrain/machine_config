import           Data.Monoid
import           System.Exit
import           XMonad

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.EwmhDesktops

import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.TreeSelect
import           XMonad.Actions.UpdatePointer

import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PerScreen
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns

import           XMonad.Prompt
import           XMonad.Prompt.Input

import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           System.Posix.Env (putEnv)
import           Control.Monad (void)

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import qualified Data.Map                           as M
import qualified XMonad.StackSet                    as W

-- Workspaces
wsWk1 = "Wk1"
wsWk2 = "Wk2"
wsWk3 = "Wk3"
wsWk4 = "Wk4"
wsCom = "Com"
wsDoc = "Doc"
wsAV  = "AV"
wsTmp = "Tmp"
wsSys = "Sys"

launchMail = rit "Email" "alot"
launchCal = rit "Calendar" "ikhal"
--launchChat = spawn "zulip"
launchMon = rit "Monitor" "btop"

-- Projects
workProject :: String -> Project
workProject ws = Project { projectName = ws
                         , projectDirectory = "~/"
                         , projectStartHook = Just $ do
                             rit "lf" "lf ~/Projects"
                             --spawn myBrowser
                         }

projects :: [Project]
projects =
  [ Project { projectName = wsCom
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                launchMail
                launchCal
                --launchChat
            }
  , Project { projectName = wsAV
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                rit "ncmpcpp" "ncmpcpp"
            }
  , Project { projectName = wsSys
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                launchMon
            }
  ] -- ++ map workProject [ wsWk1, wsWk2, wsWk3 ]
  -- Project { projectName = wsSys
            --, projectDirectory = "~/"
            --, projectStartHook = Just $ do rit' "glances -1"
            --}

---
--- derived from https://gist.github.com/Tzbob/7362371
---

myTerminal = "kitty"
myBrowser = "firefox"

myFocusFollowsMouse = True

myWorkspaces = [wsWk1, wsWk2, wsWk3, wsWk4, wsCom, wsDoc, wsAV, wsTmp, wsSys]
myMainColor = "#333333"
myBgColor = "#FEFEFE"
myTextColor = "#282828"
myLowColor = "#999999"
myLowerColor = "#DDDDDD"
greenColor = "#75b92d"

myActiveColor = greenColor
myInactiveColor = myTextColor

myBorderWidth = 0
myTopBarHeight = 5
myTabBarHeight = 13
myGap = 6
myFont = "xft:Anonymice Nerd Font:size=9"

maxNormalScreenWidth = 1920

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
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/blob/master/.xmonad/lib/XMonad/Config/A00001.hs
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
  -- TODO: split this into applications, utilities (like speaker or passwords), system (e.g. volume), and window manager things
  subKeys "system"
    [ ((myModMask, xK_q), addName "Restart and recompile xmonad" $ spawn "xmonad --restart")
    , ((myModMask, xK_F7), addName "Select speaker" $ spawn "speakerselect")
    , ((myModMask, xK_F8), addName "Select display" $ spawn "screenselect")
    , ((myModMask, xK_Print), addName "Take screenshot" $ spawn "screenshot")
    -- Volume
    , ((0, 0x1008ff11), addName "Reduce volume" $ spawn "amixer -q set Master 5- unmute")
    , ((0, 0x1008ff13), addName "Increase volume" $ spawn "amixer -q set Master 5+ unmute")
    , ((0, 0x1008ff12), addName "Mute volume" $ spawn "amixer set Master toggle")
    -- brightness
    , ((0, xF86XK_MonBrightnessUp), addName "Increase screen brightness" $ spawn "xbacklight -inc 10")
    , ((0, xF86XK_MonBrightnessDown), addName "Decrease screen brightness" $ spawn "xbacklight -dec 10")
    -- screen lock
    , ((myModMask .|. controlMask, xK_l), addName "Lock screen" $ spawn "slock")
    ] ^++^

  -- Programs
  subKeys "launchers"
    [ ((myModMask .|. shiftMask, xK_Return), addName "Terminal" $ spawn myTerminal)
    , ((myModMask, xK_b), addName "Browser" $ spawn myBrowser)
    , ((myModMask, xK_k), addName "Calendar" launchCal)
    , ((myModMask, xK_m), addName "Email" launchMail)
    , ((myModMask, xK_s), addName "Spotify" $ namedScratchpadAction scratchpads "spotify")
    , ((myModMask .|. shiftMask, xK_m), addName "Pavucontrol mixer" $ namedScratchpadAction scratchpads "mixer")
    , ((myModMask, xK_n), addName "File browser" $ rit' "lf")
    , ((myModMask .|. shiftMask, xK_n), addName "Wiki" $ spawn "nvim -c 'VimwikiIndex'")
    , ((myModMask .|. shiftMask, xK_t), addName "Work tasks" $ namedScratchpadAction scratchpads "work_tasks")
    , ((myModMask .|. controlMask, xK_p), addName "Password" $ spawn "gopass-dmenu")
    ] ^++^

  subKeys "shortcuts"
    [ --((myModMask, xK_a), addName "Create new appointment" $ apptPrompt def ["work", "home", "talks"])
    ] ^++^

  subKeys "layouts"
    [ ((myModMask, xK_space), addName "Change layout" $ sendMessage NextLayout)
    , ((myModMask, xK_Return), addName "Swap master" $ windows W.swapMaster)
    , ((myModMask, xK_t), addName "Push window back to tiling" $ withFocused $ windows . W.sink)
    , ((myModMask .|. shiftMask, xK_c), addName "Close window" $ kill)
    , ((myModMask, xK_y), addName "Hide status bar" $ sendMessage ToggleStruts)
    , ((myModMask .|. shiftMask, xK_space), addName "reset layout" $ setLayout $ XMonad.layoutHook conf)
    ]

myTreeConf = def { ts_font         = myFont
                 , ts_background   = 0x00000000
                 , ts_node         = (0xffeceff4, 0xff4c566a)
                 , ts_nodealt      = (0xffeceff4, 0xff3b4252)
                 , ts_highlight    = (0xffeceff4, 0xff88c0d0)
                 , ts_extra        = 0xffd0d0d0
                 }

-- Layouts
------------------------------------------------------------------------
myLayout = ifWider maxNormalScreenWidth wideLayout normalLayout

normalLayout = smartBorders $ avoidStruts $ tiledL ||| gridL ||| tabbedL
  where
    tiledL = named "Tiled"
      $ topbar
      $ spacing myGap
      $ ResizableTall 1 0.03 0.5 []
    tabbedL = named "Full"
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

wideLayout = smartBorders $ avoidStruts $ col3L ||| col2L ||| gridL ||| tabbedL
  where
    col3L = named "3Col"
      $ topbar
      $ spacing myGap
      $ ThreeColMid 1 (3/100) (4/9)
    col2L = named "2Col"
      $ topbar
      $ spacing myGap
      $ ResizableTall 1 0.03 0.5 []
    tabbedL = named "Full"
      $ fsSpacing myGap
      $ tabbed shrinkText myTabBar
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
    , isDialog                -?> doFloat
    , className =? "Pinentry" -?> doFloat
    , className =? "Qpdfview" -?> doFloat
    , className =? "Skype"    -?> doFloat
    , className =? "Gksqt"    -?> doFloat
    , className =? "gksqt"    -?> doFloat
    , title =? "Steam - News" -?> forceCenterFloat
    ] <+> namedScratchpadManageHook scratchpads
  where
  isBrowserDialog = isDialog <&&> (className =? "Chromium-browser" <||> className =? "Firefox")
  isRole = stringProperty "WM_WINDOW_ROLE"

-- Scratchpads
-- Name, launch command, how to find the window, float spec
scratchpads =
  --[ NS "mixer" "pavucontrol" (className =? "Pavucontrol")
  [ NS "mixer" "helvum" (className =? "helvum")
       (centerFloat 0.6 0.6)
  , NS "work_tasks" "kitty --title Tasks zsh -c 'tmuxp load Tasks'"
  -- , NS "work_tasks" "termite -t Tasks -r tasks -e tasks"
       (stringProperty "WM_NAME" =? "Tasks") (centerFloat 0.8 0.8)
  ]

-- Event handling
-------------------------------------------------------------------------------
myEventHook = mconcat
  [ docksEventHook -- this is needed to properly get xmobar struts working
  --, dynamicPropertyChange "WM_NAME" (title =? (fst spotifyFloat) --> (snd spotifyFloat))
  , fullscreenEventHook
  ]

-- Status bars and logging
-------------------------------------------------------------------------------
workspaceFormatter dbus = def {
    ppOutput          = dbusOutput dbus
  --, ppTitle           = shorten 80
  --, ppCurrent         = xmobarColor myMainColor myBgColor
  , ppCurrent         = wrap "" ""
  , ppVisible         = wrap "(" ")"
  , ppHidden          = wrap ("%{F" ++ myLowColor ++ "}") "%{F-}"
  , ppHiddenNoWindows = wrap ("%{F" ++ myLowerColor ++ "}") "%{F-}"
  , ppSep             = " | "
  , ppWsSep           = "  "
}

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myLogHook dbus = do
  let pp = workspaceFormatter dbus
  dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP pp
  -- put mouse in focused window
  updatePointer (0.25, 0.25) (0.25, 0.25)

-- Configuration structure
-------------------------------------------------------------------------------
myConfig statusPipe = def {
  -- simple stuff
    terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , borderWidth        = myBorderWidth
  , modMask            = myModMask
  , workspaces         = myWorkspaces

  -- hooks, layouts
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , logHook            = myLogHook statusPipe
  , startupHook        = ewmhDesktopsStartup
}

-- Run xmonad with the settings specified. No need to modify this.
-------------------------------------------------------------------------------
main = do
  -- for java things
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"

  -- create log files to dump window and workspace info for polybar
  --forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]

  -- setup dbus for polybar
  dbus <- D.connectSession
  ---- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad
    $ dynamicProjects projects
    $ ewmh
    $ addDescrKeys ((myModMask, xK_F1), showKeybindings) myKeys
    $ myConfig dbus

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

rit n = runInTerm ("--title \"" ++ n ++"\"")
rit' c = rit c c

-- | Prompt the user for information and add an appointment using \'khal\'
--apptPrompt :: XPConfig -> [String] -> X ()
--apptPrompt c calNames =
  --inputPromptWithCompl c "Cal" (mkComplFunFromList calNames) ?+ \cal ->
  --inputPrompt c "Title" ?+ \ttl ->
  --inputPrompt c "Start" ?+ \start ->
  --inputPrompt c "End" ?+ \end ->
  --void $ safeSpawn "khal" ["new", "-a", cal, start, end, ttl]
    ---- >> return ()

-- stuff to specify floating window sizes
centerFloat w h = customFloating $ W.RationalRect ((1-w)/2) ((1-h)/2) w h

