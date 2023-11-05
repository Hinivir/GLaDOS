#!/usr/bin/env bash

TEST_NBR=0
TEST_SUCESS=0
TEST_FAIL=0

DIR="$(dirname "$(realpath "$0")")/subject_example/lispatant"
EXEC="$(pwd)/glados"

test() {
    result=$($EXEC < "$DIR$1")
    error_msg=$($EXEC < "$DIR$1" 2>&1)

    (( TEST_NBR+=1 ))
    if [[ $result == "$2" || $error_msg == "$2" ]]; then
        echo -e "$3: [\033[32m✔\033[0m]"
        (( TEST_SUCESS+=1 ))
    else
        echo -e "$3: [\033[31m✘\033[0m]"
        (( TEST_FAIL+=1 ))
    fi

}

test "foo.lip" "42" "foo"
test "error.lip" "" "error var"

echo -e ""

test "call.lip" "5" "call"

echo -e ""

test "function1.lip" "7" "function 1"

echo -e ""

test "if1.lip" "1" "if 1"
test "if2.lip" "2" "if 2"
test "if3.lip" "21" "if 3"

echo -e ""

test "builtins1.lip" "11" "builtins 1"
test "builtins2.lip" "True" "builtins 2"
test "builtins3.lip" "False" "builtins 3"

echo -e ""

test "superior.lip" "True" "superior"
test "factorial.lip" "3628800" "factorial"

echo -e ""

echo -e "----------------------------------"
echo "Tests: $TEST_NBR"
echo -e "Success: [\033[32m$TEST_SUCESS\033[0m]"
echo -e "Fail: [\033[31m$TEST_FAIL\033[0m]"
