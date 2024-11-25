#
# ~/.bash_profile
# Starts on user login

[[ -f ~/.bashrc ]] && . ~/.bashrc

echo "Starting udiskie..."
udiskie &
echo "Starting spoof-dpi"
spoof-dpi &
startx
