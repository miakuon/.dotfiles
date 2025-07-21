# Path to your Oh My Zsh installation.
export ZSH="$HOME/.dotfiles/.oh-my-zsh"

## THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="robbyrussell"
ZSH_THEME="mortalscumbag"
# ZSH_THEME="sorin"
# ZSH_THEME="steeef"
# ZSH_THEME="powerlevel10k/powerlevel10k"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="yyyy-mm-dd"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
plugins=(git docker gh node nvm npm python rsync ssh sudo systemd)

source $ZSH/oh-my-zsh.sh

### User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='nvim'
# fi
export EDITOR='vim'

# Compilation flags
# export ARCHFLAGS="-arch $(uname -m)"

# Set personal aliases, overriding those provided by Oh My Zsh libs,
# plugins, and themes. Aliases can be placed here, though Oh My Zsh
# users are encouraged to define aliases within a top-level file in
# the $ZSH_CUSTOM folder, with .zsh extension. Examples:
# - $ZSH_CUSTOM/aliases.zsh
# - $ZSH_CUSTOM/macos.zsh
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

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

if [ -d "$HOME/Documents/Code/tscreeps/node_modules/rollup/dist/bin/" ] ;
  then PATH="$HOME/Documents/Code/tscreeps/node_modules/rollup/dist/bin/:$PATH"
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
  export DOOMDIR="$HOME/.dotfiles/.config/doom"
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

### CHANGE TITLE OF TERMINALS
function precmd() {
  case $TERM in
    xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|alacritty|st|konsole*)
      print -Pn "\e]0;%n@%m:%~\a"
      ;;
    screen*)
      print -Pn "\ek%n@%m:%~\e\\"
      ;;
  esac
}
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
alias pacsyu='sudo pacman -Syu'                            # update only standard pkgs
alias pacsyyu='sudo pacman -Syyu'                          # Refresh pkglist & update standard pkgs
alias piksua='pikaur -Sua --ignore dwm --ignore dmenu-git' # update only AUR pkgs
alias piksyu='pikaur -Syu --ignore dwm --ignore dmenu-git' # update standard pkgs and AUR pkgs
alias pacunlock='sudo rm /var/lib/pacman/db.lck'           # remove pacman lock
# alias orphan='sudo pacman -Rns $(pacman -Qtdq)'          # remove orphaned packages (DANGEROUS!)

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


### NVM
source /usr/share/nvm/init-nvm.sh

### SDKMAN
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
