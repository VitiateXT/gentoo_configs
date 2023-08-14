#  Owner = MRH
#  autostart.sh from 26th May 2022 18:18
#!/bin/sh

# xhost (maybe remove later)
xhost +local: &

#unmute speaker
amixer set Master 80 unmute &
amixer set Speaker 80 unmute &
amixer set Headphone 80 unmute &

#clipmenu
clipmenud &

# statusbar
source ~/.scripts/status.sh
