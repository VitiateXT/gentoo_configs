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
	xsetroot -name " ^c#c678dd^  $batteryst% ^c#bbc2cf^|^c#51afef^  $audiost ^c#bbc2cf^|^c#98be65^  $kernelst ^c#bbc2cf^|^c#ecbe7b^  $datest ^c#bbc2cf^|^c#ff6c6b^  $timest "
	sleep 1s
done
