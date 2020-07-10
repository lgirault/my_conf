#!/bin/sh

if ! updates_aur=$(checkupdates+aur 2> /dev/null | wc -l); then
    updates_aur=0
fi

updates=$(("$updates_arch" + "$updates_aur"))

if [ "$updates" -gt 0 ]; then
    echo "# $updates"
else
    echo ""
fi
