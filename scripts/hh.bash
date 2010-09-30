h() {
    local rc=0
    local args=$*
    [[ $args == \! ]] && args=""
    if hh $args; then
        local what=`cat ~/.hh.last | head -n 1`
        rm -f /tmp/.hh.tmp
        echo $what
        eval $what
        local rc=$?
    fi
    rm -f /tmp/.hh.tmp
    return $rc
}

