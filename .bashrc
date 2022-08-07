#  Owner = MRH
#  bashrc from 19th May 2022 2:45

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Free from here

# Doom emacs to path
export PATH="$HOME/.emacs.d/bin:$PATH"

# Custom prompt
export PS1="\[\e[31m\][\[\e[m\]\[\e[34m\]\u\[\e[m\]\[\e[m\]\[\e[33m\]@\[\e[m\]\[\e[32m\]\h\[\e[m\]\[\e[36m\] \W\[\e[m\]\[\e[31m\]]\[\e[m\] $ "

# alias for emacsclient while daemon ist running
alias emacs="emacsclient -c -a 'emacs'"
