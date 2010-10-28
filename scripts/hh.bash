h() {
    local rc=0
    local args=$*
    local opts=""
    [[ -r ~/.hhopts ]] && opts=`cat ~/.hhopts`
    history | sed -e "s/^ *//g" | cut -d\  -f 2- | sed -e "s/^ //g" > /tmp/$$
    if hh -f/tmp/$$ $opts -- $args; then
        local what=`cat ~/.hh.last | head -n 1`
        rm -f /tmp/.hh.last
        echo $what
        eval $what
        rc=$?
    fi
    rm -f /tmp/$$
    return $rc
}

