# Fig pre block. Keep at the top of this file.
export PATH="${PATH}:${HOME}/.local/bin"
eval "$(fig init zsh pre)"

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
fi

if [[ "$TERM" != "dumb" ]]
then
    eval "$(starship init zsh)"
fi


alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
export PATH="$HOME/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(direnv hook zsh)"
export PATH="$PATH:$HOME/projects/environment-script"
source $HOME/.alias.sh
eval "$(zoxide init zsh)"

# Fig post block. Keep at the bottom of this file.
eval "$(fig init zsh post)"
