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
	xsetroot -name " ^c#5a4a78^  $batteryst% ^c#cfc9c2^|^c#0f4b6e^  $audiost ^c#cfc9c2^|^c#33635c^  $kernelst ^c#cfc9c2^|^c#8c4351^  $datest ^c#cfc9c2^|^c#8f5e15^  $timest "
	sleep 1s
done
