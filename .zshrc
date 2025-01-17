
#register rsa part

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
	source /etc/profile.d/vte.sh
fi	


# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# ssh default conf
source $ZSH/oh-my-zsh.sh

# User configuration

#configure the style that the suggestion is shown with.
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=4"

# optionally set DEFAULT_USER in ~/.zshrc to your regular username to hide the “user@hostname” info when you’re logged in as yourself on your local machine.
DEFAULT_USER=lorilan

# export MANPATH="/usr/local/man:$MANPATH"

export VISUAL="vim"
# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias viconf="sudo vim /etc/nixos/configuration.nix"
#alias npm-build="npm run build"
#source /usr/share/nvm/init-nvm.sh

fd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

fd2() {
   DIR=`find * -maxdepth 0 -type d -print 2> /dev/null | fzf-tmux` \
              && cd "$DIR"
}


# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# This script was automatically generated by the broot program
# More information can be found in https://github.com/Canop/broot
# This function starts broot and executes the command
# it produces, if any.
# It's needed because some shell commands, like `cd`,
# have no useful effect if executed in a subshell.
function br {
    f=$(mktemp)
    (
	set +e
	broot --outcmd "$f" "$@"
	code=$?
	if [ "$code" != 0 ]; then
	    rm -f "$f"
	    exit "$code"
	fi
    )
    code=$?
    if [ "$code" != 0 ]; then
	return "$code"
    fi
    d=$(<"$f")
    rm -f "$f"
    eval "$d"
}

export PATH=~/.pyenv/shims:~/bin:~/.cargo/bin:~/.local/bin:~/go/bin:/home/lgirault/.local/share/coursier/bin:$PATH
export PATH=~/dev/depot_tools:$PATH
source ~/.rvm/scripts/rvm

export VAGRANT_DEFAULT_PROVIDER=libvirt
export BAZEL_NETRC=~/.netrc
export XDG_CURRENT_DESKTOP=kde #makes icons work in dolphin

export SYSTEMD_EDITOR=vim
#And then sudo visudo and add this line:
# Defaults  env_keep += "SYSTEMD_EDITOR"
alias manfr="LANG=fr_FR.UTF-8 man"

POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true
source /usr/share/nvm/init-nvm.sh

export JAVA_HOME=/usr/lib/jvm/java-17-openjdk
export JAVA_11_HOME=/usr/lib/jvm/java-11-openjdk


# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
