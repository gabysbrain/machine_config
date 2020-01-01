Config 
  { font = "xft:Anonymice Nerd Font:size=10"
  , bgColor = "#FEFEFE"
  , fgColor = "#999999"
  , position = Static { xpos = 1, ypos = 1, width = 1909, height = 20 }
  , lowerOnStart = False
  , commands = [
      Run Com "/home/tom/.xmonad/xmobar-syncthing-status.sh" [] "st" 30
    , Run Com "/home/tom/.xmonad/net.sh" [] "netstat" 30
    , Run Battery ["-t", "<left><acstatus>", "--", "-O", "+", "-i", "", "-o", " <timeleft>", "-L", "10", "-l", "red"] 60
    , Run MultiCpu ["-t", "<total>"] 30
    , Run CpuFreq ["-t", "<cpu0>GHz"] 30
    , Run Date "%_d %#B %Y <fc=#333333>|</fc> %H:%M" "date" 600
    , Run Alsa "default" "Master" ["-t", "<volume>% <status>"]
    , Run Kbd []
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " %StdinReader% }{<fc=#333333>net</fc> <action=`connman-gtk`>%netstat%</action> <fc=#333333>kb</fc> %kbd% <fc=#333333>st</fc> %st% <fc=#333333>cpu</fc> %multicpu% (%cpufreq%) <fc=#333333>vol</fc> %alsa:default:Master% <fc=#333333>bat</fc> %battery% <fc=#333333>|</fc> %date% "
  }

