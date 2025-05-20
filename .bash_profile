#
# ~/.bash_profile
# Starts on user login

[[ -f ~/.bashrc ]] && . ~/.bashrc

echo "Starting udiskie..."
udiskie &
startx
