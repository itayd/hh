h() {
    local rc=0
    local args=$*
    local opts=""
    [[ -r ~/.hhopts ]] && opts=`cat ~/.hhopts`
    history | awk "{print \$2}" > /tmp/$$
    hh -f/tmp/$$ $opts -- $args
    if [[ $? -eq 0 ]]; then
        what=`cat ~/.hh.last | head -n 1`
        rm -f /tmp/.hh.last
        print -z $what
        rc=$?
    fi
    rm -f /tmp/$$
    return $rc
}

