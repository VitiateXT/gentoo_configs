#  Owner = MRH
#  autostart.sh from 26th May 2022 18:18
#!/bin/sh

# xhost (maybe remove later)
xhost +local: &

#unmute speaker
amixer set Master 70 unmute &
amixer set Speaker 0 unmute &
amixer set Headphone 70 unmute &

# emacs daemon
/usr/bin/emacs --daemon &

#clipmenu
clipmenud &

# statusbar
source ~/.scripts/status.sh
