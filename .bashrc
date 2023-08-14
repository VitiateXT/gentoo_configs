#  Owner = MRH
#  bashrc from 08th August 2022 0:40

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# -personal config-

# Doom emacs to path
# export PATH="$HOME/.emacs.d/bin:$PATH"

# vi mode
set -o vi

# Custom prompt
export PS1="\[\e[31m\][\[\e[m\]\[\e[34m\]\u\[\e[m\]\[\e[m\]\[\e[33m\]@\[\e[m\]\[\e[32m\]\h\[\e[m\]\[\e[36m\] \W\[\e[m\]\[\e[31m\]]\[\e[m\] $ "

# alias for emacsclient while daemon ist running
# alias emacs="emacsclient -c -a 'emacs'"

alias vim="nvim"

# clipmenu env
export CM_SELECTIONS="clipboard"
export CM_DEBUG=0
export CM_OUTPUT_CLIP=1
export CM_MAX_CLIPS=25
export CM_IGNORE_WINDOW="KeePassXC"

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -dr
