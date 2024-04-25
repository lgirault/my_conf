function register_if_exists {
  local keyname=$1
  if [ -f $HOME/.ssh/${keyname} ]; then
    eval $(keychain --eval --quiet --nogui ${keyname})
  fi
}
register_if_exists github_karas_rsa
register_if_exists github_work_rsa

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

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"
#ZSH_THEME="agnoster-custom"
#ZSH_THEME="agnoster"
# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

plugins=(git shrink-path)

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

#ssh-add ~/.ssh/git_key

#export SBT_OPTS="-Dsbt.override.build.repos=true -Xms512M -Xmx5120M -Xss1M"

export SBT_CREDENTIALS="$HOME/.ivy2/.credentials"

export DEV_HOME="$HOME/dev/mediarithmics"
export TEST_TMPDIR="$DEV_HOME/.bazel_cache"

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


export PAGER="less -FRX"

alias hunt="ps -aux | grep "

#source /usr/share/nvm/init-nvm.sh

#create ssh-agent if does not exist
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)"
fi

#setxkbmap -layout fr -variant bepo


#local scalaMajor=2.12
#local scalaVersion=2.12.8
#local scalametaVersion=4.1.4
# alias metac="coursier launch org.scalameta:metac_${scalaVersion}:${scalametaVersion} -- -cp $(coursier fetch -p org.scala-lang:scala-library:${scalaVersion})"
# alias metacp="coursier launch org.scalameta:metacp_${scalaMajor}:${scalametaVersion} -- --dependency-classpath $(echo $JAVA_HOME/jre/lib/rt.jar):$(coursier fetch org.scala-lang:scala-library:${scalaVersion} -p)"
# alias metap="coursier launch org.scalameta:metap_${scalaMajor}:${scalametaVersion} --"
export GITHUB_TOKEN=`cat ~/.pass/hub_token` 
export GITHUB_PACKAGES_TOKEN=`cat ~/.pass/github_packages_token`
#alias grep=rg
alias real_find=/bin/find
alias zz=7z
#alias find=fzf

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

export MICS_PROD_USER=lgirault
export MICS_SSH_PRIVATE_KEY=/home/lgirault/.ssh/mics_prod_2023-06-21



# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
