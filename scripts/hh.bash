hh_() {
    rc=0
    if /usr/local/bin/hh; then
        local what=`cat /tmp/.hh.tmp | head -n 1`
        echo $what >> ~/.bash_history
        eval $what
        local rc=$?
    fi
    rm -f /tmp/.hh.tmp
    return $rc
}

