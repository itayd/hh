h() {
    local rc=0
    local args=$*
    local opts=""
    [[ -r ~/.hhopts ]] && opts=`cat ~/.hhopts`
    if hh $opts -- $args; then
        local what=`cat ~/.hh.last | head -n 1`
        rm -f /tmp/.hh.last
        echo $what
        eval $what
        rc=$?
    fi
    return $rc
}

