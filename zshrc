#
autoload -U zrecompile
export TZ="Asia/Calcutta"
export PATH="/home/y/bin:/usr/local/bin:/sbin:/usr/bin:/bin:/usr/bin/X11:/usr/games:/usr/sbin/:/usr/local/sbin:/home/seco/gauge:/home/renjith/bin:/usr/libexec:/usr/local/libexec:/export/crawlspace/configs/bin:/home/y/bin"


HISTFILE=~/hist/.zsh_history
HISTSIZE=5000
SAVEHIST=5000
setopt inc_append_history extended_history
setopt hist_ignore_dups hist_reduce_blanks hist_find_no_dups


setopt prompt_subst
if [ $UID -eq 0 ]; then
export PS1="[$(print '%{\e[1;31m%}%n%{\e[0m%}') @ %M:%~]#"
else
export PS1="[$(print '%{\e[1;31m%}%n%{\e[0m%}') @ %M]>"
fi
# Display # of background jobs if it is greater than 0
RPS1='%(1v.%{$fg[red]%}(bg %v%) .)%{$fg[cyan]%}${UPTIME} %t:$?%{$fg[white]%}'
# End prompt

(( ${+HOSTNAME} )) || export HOSTNAME=$HOST
export shortname=$(hostname -s)
export coloname=$(hostname|awk -F. '{print $3}')

# Various options
setopt nobeep               # no beeps please
setopt auto_cd              # change to dirs without cd
setopt auto_pushd           # append dirs to push/pop list
setopt pushd_ignore_dups    # and don't duplicate them
#setopt correct              # spelling correction
setopt extended_glob        # funky pattern matching
setopt equals               # perform - filename expansion
setopt magic_equal_subst    # expand ~ in echo foo=~/bar bar=~/foo
setopt interactive_comments # escape commands and save them in the history
setopt complete_in_word     # <tab> in word works
setopt always_to_end        # after completing in the middle move to the end


stty -ixon -ixoff -ixany
ttyctl -f

case $TERM in
    xterm*)
        precmd () {
		print -Pn "\e]0;%n@%m: %~\a"
		print -Pn "\e]30;$shortname|$coloname\a"
	    	if [[ -f .p4/Client ]];then
       	 		P4CLIENT=`cat .p4/Client`
    		else
        		P4CLIENT="`whoami`.$HOSTNAME"
    		fi
    		P4USER=renjith
    		export P4USER P4CLIENT
	}
        ;;
esac

if [ ! -e $HISTFILE ]; then
	touch $HISTFILE
fi
chmod 600 $HISTFILE

alias exotel='/usr/local/exotel/exotel.pl -m'

bindkey -e
    bindkey "^[[2~" yank
    bindkey "^[[3~" delete-char
    bindkey "^[[5~" up-line-or-history ## PageUp
    bindkey "^[[6~" down-line-or-history ## PageDown
    bindkey "^[[7~" beginning-of-line
    bindkey "^[[8~" end-of-line
    bindkey "^[e" expand-cmd-path ## C-e for expanding path of typed command
#    bindkey "^[[A" history-beginning-search-backward-end ## up arrow for back-history-search
#    bindkey "^[[B" history-beginning-search-forward-end # down arrow for fwd-history-search
    bindkey " " magic-space ## do history expansion on spa



hash -d w3=/export/crawlspace/watcher-logs
hash -d eh=/export/home
hash -d ec=/export/crawlspace
hash -d gem=/var/gem
hash -d stc=/home/seco/tools/conf
hash -d js=/usr/local/jumpstart


##########################
# Functions
#########################
