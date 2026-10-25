#!/bin/sh

prefix="/usr"
exec_prefix="${prefix}"
xrdb="xrdb"
xinitdir="/etc/X11/xinit"
xclock="xclock"
xterm="xterm"
twm="twm"
xmodmap="xmodmap"

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
sysresources="$xinitdir/.Xresources"
sysmodmap="$xinitdir/.Xmodmap"

# merge in defaults and keymaps

if [ -f "$sysresources" ]; then
    if [ -x /usr/bin/cpp ] ; then
        "$xrdb" -merge "$sysresources"
    else
        "$xrdb" -nocpp -merge "$sysresources"
    fi
fi

if [ -f "$sysmodmap" ]; then
    "$xmodmap" "$sysmodmap"
fi

if [ -f "$userresources" ]; then
    if [ -x /usr/bin/cpp ] ; then
        "$xrdb" -merge "$userresources"
    else
        "$xrdb" -nocpp -merge "$userresources"
    fi
fi

if [ -f "$usermodmap" ]; then
    "$xmodmap" "$usermodmap"
fi

if [ -d "$xinitdir"/xinitrc.d ] ; then
	for f in "$xinitdir/xinitrc.d"/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

autostart() {
    xterm &
    AmneziaVPN &
    # for i in $(seq 1 20); do
    #     systemctl --user is-active --quiet emacs.service && break
    #     sleep 1
    # done
    # emacsclient -c -n -a '' &
}

systemctl --user import-environment DISPLAY XAUTHORITY
dbus-update-activation-environment --systemd DISPLAY XAUTHORITY 2>/dev/null

#polkit-agent
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# some audio stuff
sudo modprobe -q snd-pcm-oss &
amixer -q sset Master 50% &

kbdd &                           # per window layout states
systemctl --user start dunst &   # notifications
dwmblocks &                      # slstatus &

bgs /home/mia/Pictures/Wallpapers/2160x1440-map-of-copmuter-science.png &
dwm &
sleep 0.5
autostart
wait

