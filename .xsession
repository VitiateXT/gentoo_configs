#  Owner = MRH
#  xinitrc from 26th May 2022 14:46
#!/bin/sh

# set keymap
setxkbmap de

# merge with Xresources (important for Xft.dpi)
xrdb -merge ~/.Xresources

# picom -> compositor (transparency)
#picom &

# set last wallpaper
#nitrogen --restore &

# execute WM
exec dwm
