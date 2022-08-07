#  Owner = MRH
#  autostart.sh from 26th May 2022 18:18
#!/bin/sh

# xhost (maybe remove later)
xhost +local: &

#unmute speaker
amixer set Master unmute &

# emacs daemon
# /usr/bin/emacs --daemon

#clipmenu
clipmenud &

# statusbar
source ~/.scripts/status.sh
