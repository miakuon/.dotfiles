#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

#polkit-agent
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# some audio stuff
sudo modprobe -q snd-pcm-oss &
amixer -q sset Master 0% &

systemctl --user start dunst &
dwmblocks & # slstatus &
bgs /home/mia/Pictures/Wallpapers/2160x1440-map-of-copmuter-science.png
dwm

