#!/bin/bash

##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## check
##

function my_readlink() {
    cd $1
    pwd
    cd - > /dev/null
}

if [ $# = 2 ]; then
    DELIVERY_DIR=$(my_readlink "$1")
    REPORTS_DIR=$(my_readlink "$2")
    EXPORT_FILE="$REPORTS_DIR"/coding-style-reports.log
    SCRIPT=./.github/script/lambdananas
    ### delete existing report file
    rm -f "$EXPORT_FILE"

    ### generate reports
    $SCRIPT . > coding-style-reports.log
fi
