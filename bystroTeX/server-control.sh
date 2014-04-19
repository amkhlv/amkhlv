#!/usr/bin/env bash

# Copyright 2012,2013 Andrei Mikhailov

# This file is part of bystroTeX.

# bystroTeX is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# bystroTeX is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with bystroTeX.  If not, see <http://www.gnu.org/licenses/>.

export MAIN_PIPE_NAME="bystrotex.fifo"
export  LOG_FILE_NAME="server-log.txt"
export ERROR_FILE_NAME="server-error.txt"

start_server (){
    AMKHLV_CLASSPATH="$AMKHLV_JAVA_PATH:$JLATEXMATH_JAR"
    java -classpath  "$AMKHLV_CLASSPATH" LaTeXServer "$MAIN_PIPE_NAME" "$LOG_FILE_NAME" 2>"$ERROR_FILE_NAME" 1>&- &
} 

wait_and_kill (){ # $1 is secs to sleep, $2 is PID to kill
    sleep $1 & X=$!
    echo $X
    wait $X 2>&- ; SLEPT_WELL=$?
    [ $SLEPT_WELL == 0 ] && kill $2 2>&-
}

# procedure to see if the pipe is responding:
check_server (){
    export WAIT_N_SEC=$1 
    if [ -p "$MAIN_PIPE_NAME" ] ; then
        echo HI > "$MAIN_PIPE_NAME" & export ECHO_PID=$! 
        coproc HITMAN { 
            wait_and_kill  $WAIT_N_SEC  $ECHO_PID
            read  # this is to lock the coproc 
        }
        read SLEEP_PID 0<&${HITMAN[0]}
        wait $ECHO_PID 2>&- ; PIPE_STATUS_WAS=$?
        # at this point either our pipe started responding:
        if [ $PIPE_STATUS_WAS == 0 ] ; then
            kill $SLEEP_PID  2>&-
            echo goaway 1>&${HITMAN[1]} # to release the coproc lock
            wait $HITMAN_PID 2>&-
            cat "$MAIN_PIPE_NAME" & export CAT_PID=$!
            coproc TERMINATOR {
                wait_and_kill 2 $CAT_PID
                read  # this is to lock the coproc 
            }
            read SLEEP_PID 0<&${TERMINATOR[0]}
            wait $CAT_PID   2>&- ; 
            ANSWR_PIPE_STATUS_WAS=$?
            if [ $ANSWR_PIPE_STATUS_WAS != 0 ] ; then
                echo "PIPE_FAILED_TO_RESPOND"
            fi
            kill $SLEEP_PID  2>&-
            echo goaway 1>&${TERMINATOR[1]} # to release the coproc lock
            wait $TERMINATOR_PID 2>&-
        else # or it was killed:
            kill $SLEEP_PID  2>&-
            echo goaway 1>&${HITMAN[1]} # to release the coproc lock
            wait $HITMAN_PID 2>&-
            echo "PIPE_FAILED_TO_RESPOND"
        fi
    else
        mkfifo "$MAIN_PIPE_NAME" 
        echo "THERE_WAS_NO_PIPE--APPEARS_FIRST_RUN"
    fi
}

WAIT_SECONDS=3

while [ $WAIT_SECONDS ] ; do
    CHECK_RESULT="$( check_server $WAIT_SECONDS 2>&- )"
    if [ "$CHECK_RESULT" != LISTENING ] ; then 
    # at this point either our write to the pipe did not go through 
    # or we got wrong response;
    # try to restart our Java server (or perhaps start it for the first time):
        if [ $WAIT_SECONDS -lt 10 ] ; then
            rm "$MAIN_PIPE_NAME" ; mkfifo "$MAIN_PIPE_NAME" ; start_server 
            WAIT_SECONDS=20
            # (we have assumed that Java should be able to start in 20 seconds...)
        else
            WAIT_SECONDS=""
            echo 0
            echo "$CHECK_RESULT" 1>&2 ; exit 1
        fi
    else
        # stdin enters here:
        ( echo XML ; cat ) > "$MAIN_PIPE_NAME" 
        # reading the response from the server (some XML) and writing it on stdout:
        cat "$MAIN_PIPE_NAME"
        # now want to get out of the while loop and finish the program:
        WAIT_SECONDS=""
    fi
done


