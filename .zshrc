autoload -U colors && colors
autoload -Uz compinit && compinit -i
# plugins=(git z extract fzf alias-tips zsh-autosuggestions)

export PS1="
%{$fg[green]%}%~%{$reset_color%}
%{$fg[magenta]%}$%{$reset_color%} "
export TERMINAL="konsole"
. ~/.aliases
export ALTERNATE_EDITOR="amp"
export EDITOR="/usr/bin/emacsclient -n"
export FZF_DEFAULT_COMMAND='ag -g ""'
export LESS=' -R '
export CHROME_BIN=/usr/bin/chromium
export PLUG_EDITOR='/usr/bin/emacsclient -n +__LINE__ __FILE__'
export TERMCMD=konsole
export TERM=xterm-256color

#fh - repeat history
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}


fpath=(~/.zsh/completion $fpath)

## History wrapper
function omz_history {
  local clear list
  zparseopts -E c=clear l=list

  if [[ -n "$clear" ]]; then
    # if -c provided, clobber the history file
    echo -n >| "$HISTFILE"
    echo >&2 History file deleted. Reload the session to see its effects.
  elif [[ -n "$list" ]]; then
    # if -l provided, run as if calling `fc' directly
    builtin fc "$@"
  else
    # unless a number is provided, show all history events (starting from 1)
    [[ ${@[-1]} = *[0-9]* ]] && builtin fc -l "$@" || builtin fc -l "$@" 1
  fi
}

# Timestamp format
case $HIST_STAMPS in
  "mm/dd/yyyy") alias history='omz_history -f' ;;
  "dd.mm.yyyy") alias history='omz_history -E' ;;
  "yyyy-mm-dd") alias history='omz_history -i' ;;
  "") alias history='omz_history' ;;
  *) alias history="omz_history -t '$HIST_STAMPS'" ;;
esac

## History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
HISTSIZE=50000
SAVEHIST=10000

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt inc_append_history     # add commands to HISTFILE in order of execution
setopt share_history          # share command history data


ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=25"
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# opam configuration
test -r /home/farhad/.opam/opam-init/init.zsh && . /home/farhad/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

eval "$(lua ~/bin/z.lua --init zsh enhanced once)"
. $HOME/.nix-profile/etc/profile.d/nix.sh
export PATH=$HOME/.fnm:$HOME/bin/:$HOME/extbin/:$PATH
eval "`fnm env --multi`"
