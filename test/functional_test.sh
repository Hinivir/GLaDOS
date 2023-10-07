#!/usr/bin/env bash

TEST_NBR=0
TEST_SUCESS=0
TEST_FAIL=0

DIR="$(dirname "$(realpath "$0")")/subject_example/"
EXEC="$(pwd)/glados"

test() {
    result=$($EXEC < "$DIR$1")

    (( TEST_NBR+=1 ))
    if [[ $result == "$2" ]]; then
        echo -e "$3: [\033[32m✔\033[0m]"
        (( TEST_SUCESS+=1 ))
    else
        echo -e "$3: [\033[31m✘\033[0m]"
        (( TEST_FAIL+=1 ))
    fi

}

test "foo.scm" "42" "foo"
test "error.scm" "*** ERROR : variable foo is not bound." "error var"

echo -e ""

test "call.scm" "5" "call"

echo -e ""

test "lambda1.scm" "#<procedure>" "lambda 1"
test "lambda2.scm" "3" "lambda 2"
test "lambda3.scm" "7" "lambda 3"

echo -e ""

test "function1.scm" "7" "function 1"

echo -e ""

test "if1.scm" "1" "if 1"
test "if2.scm" "2" "if 2"
test "if3.scm" "21" "if 3"

echo -e ""

test "builtins1.scm" "11" "builtins 1"
test "builtins2.scm" "#t" "builtins 2"
test "builtins3.scm" "#f" "builtins 3"

echo -e ""

test "superior.scm" "#t" "superior"
test "factorial.scm" "3628800" "factorial"

echo -e ""

echo -e "----------------------------------"
echo "Tests: $TEST_NBR"
echo -e "Success: [\033[32m$TEST_SUCESS\033[0m]"
echo -e "Fail: [\033[31m$TEST_FAIL\033[0m]"
