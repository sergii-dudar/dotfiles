function sesh-sessions() {
    {
        exec </dev/tty
        exec <&1
        ~/dotfiles/zsh/serhii.shell/util/sesh.sessions.fast.fzf.sh
    }
}

zle     -N             sesh-sessions
bindkey -M emacs '\es' sesh-sessions
bindkey -M vicmd '\es' sesh-sessions
bindkey -M viins '\es' sesh-sessions

bindkey '^F' sesh-sessions
bindkey '^F' sesh-sessions
bindkey '^F' sesh-sessions

# kitty/mac
#bindkey "ß" sesh-sessions
