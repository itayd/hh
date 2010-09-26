__hh() {
	local cur pref opts
	COMPREPLY=()
	cur="${COMP_WORDS[COMP_CWORD]}"

    if hh; then
        opts=`cat /tmp/.hh.tmp`
        rm -f /tmp/.hh.tmp
	    COMPREPLY=( $(compgen -W "${opts}" -- ${cur} ) )
        return 0
    else
        return 1
    fi
}

complete -F __hh _

