#!/bin/bash
#Author: NNoeLL <nnoell3@gmail.com>
#Deps: trayer 1.0-4; dzen2-xft-xpm-xinerama-svn 271-1
#Based on: https://bbs.archlinux.org/viewtopic.php?id=45364

#Apps: /usr/bin/
AP1="GridSelect" #xdotool key super+g
AP2="Logout"     #xdotool key super+shift+q
AP3="Halt"		 #sudo halt
AP4="Reboot"	 #sudo reboot
#App Icons
ICM=~/.icons/subtlexbm/box_in.xbm
AP1I=~/.icons/subtlexbm/tile1.xbm
AP2I=~/.icons/subtlexbm/bail.xbm
AP3I=~/.icons/subtlexbm/off.xbm
AP4I=~/.icons/subtlexbm/clockwise.xbm
#Look and feel
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
FG1=#ffffff #font color
FG2=#ffffff #menu icon color
FG3=#82ca42 #icons color

launcher() {
	(echo "^fg(${FG2})^i(${ICM})"; echo -e "^fg(${FG1})\
	^fg(${FG3})^fg(${FG1})^i(${AP1I}) ${AP1} \
	\n ^fg(${FG3})^fg(${FG1})^i(${AP2I}) ${AP2} \
	\n ^fg(${FG3})^fg(${FG1})^i(${AP3I}) ${AP3} \
	\n ^fg(${FG3})^fg(${FG1})^i(${AP4I}) ${AP4} ")\
	| (dzen2 -x '1260' -y '784' -fn "$FONT" -bg '#60a0c0' -fg '#ffffff' -l '4' -tw '30' -h '16' -w '95' -m -p -e 'button3=togglecollapse;leaveslave=collapse;button1=menuexec')
	return
}

tray() {
	trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 14 --height 16 --transparent true --alpha 0 --tint 0x050505 --margin 20
	return
}

#Launch trayer and launcher
launcher &
tray
