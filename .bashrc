# bash config

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
#PS1='[\u@\h \W]\$ '
PS1='\[\e[01;35m\]\W\[\e[m\] > '

[ -r /home/mia/.byobu/prompt ] && . /home/mia/.byobu/prompt   #byobu-prompt#

### PATH
if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/Applications" ] ;
  then PATH="$HOME/Applications:$PATH"
fi

if [ -d "/var/lib/flatpak/exports/bin/" ] ;
  then PATH="/var/lib/flatpak/exports/bin/:$PATH"
fi

if [ -d "$HOME/Documents/Code/screeps/node_modules/grunt-cli/bin" ] ;
  then PATH="$HOME/Documents/Code/screeps/node_modules/grunt-cli/bin:$PATH"
fi

### EMACS
if [ -d "$HOME/.config/emacs/bin/" ] ;
  then PATH="$HOME/.config/emacs/bin/:$PATH"
fi

if [ -d "$HOME/.emacs.d/bin" ] ;
  then PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [ -z "$EMACS" ] ; then
  export EMACS="/usr/bin/emacs"
fi

if [ -z "$EMACSDIR" ] ; then
  export EMACSDIR="$HOME/.config/emacs"
fi

if [ -z "$DOOMDIR" ] ; then
  export DOOMDIR="$HOME/.config/doom"
fi

### OTHER ENVIRONMENT VARIABLES
if [ -z "$XDG_CONFIG_HOME" ] ; then
    export XDG_CONFIG_HOME="$HOME/.config"
fi
if [ -z "$XDG_DATA_HOME" ] ; then
    export XDG_DATA_HOME="$HOME/.local/share"
fi
if [ -z "$XDG_CACHE_HOME" ] ; then
    export XDG_CACHE_HOME="$HOME/.cache"
fi
if [ -z "$HISTTIMEFORMAT" ] ; then
    export HISTTIMEFORMAT='%F %T '
fi

#### DT config
### CHANGE TITLE OF TERMINALS
case ${TERM} in
  xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|alacritty|st|konsole*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
        ;;
  screen*)
    PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
    ;;
esac

### SHOPT
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases
shopt -s checkwinsize # checks term size when bash regains control

### ALIASES ###
# navigation
alias cd..='cd ..'
alias cd...='cd ../..'

# eza
alias eza='eza -al --color=always --group-directories-first' # my preferred listing
alias ezaa='eza -a --color=always --group-directories-first'  # all files and dirs
alias ezal='eza -l --color=always --group-directories-first'  # long format

# gpg encryption
# verify signature for isos
alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
# receive the key of a developer
alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"

# git
# alias addup='git add -u'
# alias addall='git add .'
# alias branch='git branch'
# alias checkout='git checkout'
# alias clone='git clone'
# alias commit='git commit -m'
# alias fetch='git fetch'
# alias pull='git pull origin'
# alias push='git push origin'
# alias stat='git status'  # 'status' is protected name so using 'stat' instead
# alias tag='git tag'
# alias newtag='git tag -a'

# ps
alias psa="ps auxf"
alias psgrep="ps aux | grep -v grep | grep -i -e VSZ -e"
alias psmem='ps auxf | sort -nr -k 4'
alias pscpu='ps auxf | sort -nr -k 3'

# pacman and pikaur
alias pacsyu='sudo pacman -Syu'                  # update only standard pkgs
alias pacsyyu='sudo pacman -Syyu'                # Refresh pkglist & update standard pkgs
alias piksua='pikaur -Sua'             # update only AUR pkgs (paru)
alias piksyu='pikaur -Syu' # update standard pkgs and AUR pkgs (paru)
alias unlock='sudo rm /var/lib/pacman/db.lck'    # remove pacman lock
# alias orphan='sudo pacman -Rns $(pacman -Qtdq)' # remove orphaned packages (DANGEROUS!)

# get fastest mirrors
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

# change your default USER shell
alias tobash="sudo chsh $USER -s /bin/bash && echo 'Log out and log back in for change to take effect.'"
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Log out and log back in for change to take effect.'"
alias tofish="sudo chsh $USER -s /bin/fish && echo 'Log out and log back in for change to take effect.'"

# other
alias em="/usr/bin/emacs -nw"  # Terminal version of Emacs
alias df='df -h'               # human-readable sizes
alias free='free -m'           # show sizes in MB
alias grep='grep --color=auto' # colorize output (good for log files)
#alias pip='echo "Are you sure pipx doesn't work?" && pip'               # to avoid breaking package system


### IDK what is that
source /usr/share/nvm/init-nvm.sh
