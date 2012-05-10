h() {
    local rc=0
    local opts=""
    [[ -r ~/.hhopts ]] && opts=`cat ~/.hhopts`
    history 0 -1 | sed -e "s/^ *[0-9]* *//g" > /tmp/$$
    hh --from=/tmp/$$ $opts -- $BUFFER
    if [[ $? -eq 0 ]]; then
        BUFFER=`cat ~/.hh.last | head -n 1`
        rm -f /tmp/.hh.last
    fi
    rm -f /tmp/$$
}

zle -N h

bindkey "^h" h

