#!/bin/bash
# Author: nnoell <nnoell3@gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn && trayer
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
BAR_H=9
BIGBAR_W=65
WIDTH=500 #220 for trayer
HEIGHT=16
X_POS=1000 #1060 for trayer and -w 939 in xmonad.hs
Y_POS=1034

#Colors and font
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
DZEN_BG="#020202"
DZEN_FG="#eee"
DZEN_FG2="#bbb"
CRIT="#99cc66"
BAR_FG="#3475aa"
BAR_BG="#363636"
COLOR_SEP=$DZEN_FG2

#Options
INTERVAL=5
WIFISIGNAL=0

printDiskInfo() {
	RFSP=$(df -h / | tail -1 | awk '{ print $5 }' | tr -d '%')
	BFSP=$(df -h /boot | tail -1 | awk '{ print $5 }' | tr -d '%')
	G=$(df -h /media/500G | tail -1 | awk '{ print $5 }' | tr -d '%')
	H=$(df -h /home | tail -1 | awk '{ print $5 }' | tr -d '%')
	echo -n "^fg($DZEN_FG2)| ROOT ^fg($BAR_FG)${RFSP}% "
	echo -n "^fg($DZEN_FG2)| BOOT ^fg($BAR_FG)${BFSP}%"
	echo -n "^fg($DZEN_FG2)| 500G ^fg($BAR_FG)${G}%"
	echo -n "^fg($DZEN_FG2)| HOME ^fg($BAR_FG)${H}%"
	return
}

printBattery() {
	BatPresent=$(acpi -b | wc -l)
	ACPresent=$(acpi -a | grep -c on-line)
	if [[ $BatPresent == "0" ]]; then
		echo -n "^fg($DZEN_FG2)AC ^fg($BAR_FG)on ^fg($DZEN_FG2)BAT ^fg($BAR_FG)off"
		return
	else
		RPERC=$(acpi -b | awk '{print $4}' | tr -d "%,")
		echo -n "^fg($DZEN_FG2)BAT "
		if [[ $ACPresent == "1" ]]; then
			echo -n "$(echo $RPERC | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl)"
		else
			echo -n "$(echo $RPERC | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl)"
		fi
		echo -n " ^fg()$RPERC%"
	fi
	return
}

printWifiInfo() {
	WIFIDOWN=$(wicd-cli --wireless -d | wc -l)
	WIFISIGNAL=0
#	[[ $WIFIDOWN -ne "1" ]] && WIFISIGNAL=$(wicd-cli --wireless -d | grep Quality | awk '{print $2}')
	echo -n "^fg($DZEN_FG2)WIFI "
	if [[ $WIFIDOWN -ne "1" ]]; then
		WIFISIGNAL=$(wicd-cli --wireless -d | grep Quality | awk '{print $2}')
		echo -n "$(echo $WIFISIGNAL | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
		echo -n "^fg()$WIFISIGNAL% "
	else
		echo -n "^fg($CRIT)N/A "
	fi
	return
}

printSpace() {
	echo -n " ^fg($COLOR_SEP)|^fg() "
	return
}

printBottomBar() {
	while true; do
		printDiskInfo
		printSpace
		printBattery
		printSpace
		printWifiInfo
		echo
		sleep $INTERVAL
	done
	return
}

tray() {
	trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 16 --transparent true --alpha 0 --tint 0x050505 --margin 215
	return
}

#Print all and pipe into dzen2
printBottomBar | dzen2 -x $X_POS -y $Y_POS -w $WIDTH -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e ''
#tra1034
