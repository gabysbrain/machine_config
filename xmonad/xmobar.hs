Config 
  { font = "xft:Anonymous Pro:size=10"
  , bgColor = "#FEFEFE"
  , fgColor = "#999999"
  , position = Static { xpos = 1, ypos = 1, width = 1909, height = 20 }
  , lowerOnStart = False
  , commands = [
      Run Com "/home/tom/.xmonad/xmobar-syncthing-status.sh" [] "st" 30
    , Run Com "/home/tom/.xmonad/wireless.sh" [] "wifi" 30
    , Run Battery ["-t", "<left>"] 100
    , Run MultiCpu ["-t", "<total>"] 30
    , Run Date "%_d %#B %Y <fc=#333333>|</fc> %H:%M" "date" 600
    , Run Alsa "default" "Master" ["-t", "<volume>% <status>"]
    , Run Kbd []
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " %StdinReader% }{<fc=#333333>wifi</fc> <action=`connman-gtk`>%wifi%</action> <fc=#333333>kb</fc> %kbd% <fc=#333333>st</fc> %st% <fc=#333333>cpu</fc> %multicpu% <fc=#333333>vol</fc> %alsa:default:Master% <fc=#333333>bat</fc> %battery% <fc=#333333>|</fc> %date% "
  }

