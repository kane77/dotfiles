#!/bin/sh

usage="usage: $0 -c {up|down|mute} [-i increment] [-m mixer]"
command=
increment=5
mixer=Master

while getopts i:m:h o
do case "$o" in
    i) increment=$OPTARG;;
    m) mixer=$OPTARG;;
    h) echo "$usage"; exit 0;;
    ?) echo "$usage"; exit 0;;
esac
done

shift $(($OPTIND - 1))
command=$1

if [ "$command" = "" ]; then
    echo "usage: $0 {up|down|mute} [increment]"
    exit 0;
fi

display_volume=0

if [ "$command" = "up" ]; then
    pamixer --increase $increment
    display_volume=$(pamixer --get-volume)
fi

if [ "$command" = "down" ]; then
    pamixer --decrease $increment
    display_volume=$(pamixer --get-volume)
fi

icon_name=""

if [ "$command" = "mute" ]; then
    if $(pamixer --get-mute); then
        pamixer --unmute
        display_volume=$(pamixer --get-volume)
    else
        display_volume=0
        icon_name="notification-audio-volume-muted"
        pamixer --mute
    fi
fi

if [ "$icon_name" = "" ]; then
    if [ "$display_volume" = "0" ]; then
        icon_name="notification-audio-volume-off"
    else
        if [ "$display_volume" -lt "33" ]; then
            icon_name="notification-audio-volume-low"
        else
            if [ "$display_volume" -lt "67" ]; then
                icon_name="notification-audio-volume-medium"
            else
                icon_name="notification-audio-volume-high"
            fi
        fi
    fi
fi
#notify-send "Volume changed " -i $icon_name -h int:value:$display_volume -h string:synchronous:volume
