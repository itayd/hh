h() {
    rc=0
    if hh $*; then
        local what=`cat /tmp/.hh.tmp | head -n 1`
        rm -f /tmp/.hh.tmp
        echo $what
        eval $what
        local rc=$?
    fi
    rm -f /tmp/.hh.tmp
    return $rc
}

