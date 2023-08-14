#  Owner = MRH
#  status.sh from 20th July 2022 11:41

#!/bin/sh

# while loop for dwm-statusbar
while true; do
	batteryst=$(cat /sys/class/power_supply/BAT0/capacity)
	audiost=$(awk -F"[][]" '/dB/ { print $2 }' <(amixer sget Master))
	datest=$(date '+%a %d %b')
	timest=$(date '+%H:%M')
	kernelst=$(uname -r)
	xsetroot -name " ^c#957fb8^  $batteryst% ^c#c8c093^|^c#7e9cd8^  $audiost ^c#c8c093^|^c#76946a^  $kernelst ^c#c8c093^|^c#c34043^  $datest ^c#c8c093^|^c#c0a36e^  $timest "
	sleep 1s
done
