#!/bin/bash

TOOLS_DIR=~/stuff/projects/sandboxes/ai-challenge-2011/tools
CUR_DIR=$(pwd)
TIME=1000 # orig
#TIME=5000
TURNS=1000 # orig
#TURNS=100

wait_key ()
{
    echo "Press Enter"
    read foo
}

while /bin/true; do
    opt=$(dialog --menu "ants" 20 50 14 c "compile" \
        4_1 "maze_04p_01 (del log)" \
        2_1 "maps/maze/maze_02p_01.map (del log)" \
        3>&1 1>&2 2>&3 3>&-)
    #echo "opt=$opt"
    #wait_key

    if [ "$opt"_ = "c"_ ]; then
        sbcl --script MyBot.lisp
        no=0
    elif [ "$opt"_ = "4_1"_ ]; then
        map="maps/maze/maze_04p_01.map"
        no=4
    elif [ "$opt"_ = "2_1"_ ]; then
        map="maps/maze/maze_02p_01.map"
        no=2
    else
        exit 0
    fi

    if [ $no -gt 0 ]; then
        rm -f "$CUR_DIR/output.log"
        pushd "$TOOLS_DIR"
        if [ "$no" -eq 2 ]; then
            ./playgame.py -So --player_seed 42 --end_wait=0.25 --verbose --log_dir game_logs --turns $TURNS \
                --turntime=$TIME --map_file "$map" \
                "python sample_bots/python/LeftyBot.py" \
                "$CUR_DIR/MyBot" | java -jar visualizer.jar
        elif [ "$no" -eq 4 ]; then
            ./playgame.py -So --player_seed 42 --end_wait=0.25 --verbose --log_dir game_logs --turns $TURNS \
                --turntime=$TIME --map_file "$map" \
                "python sample_bots/python/LeftyBot.py" "python sample_bots/python/HunterBot.py" \
                "python sample_bots/python/LeftyBot.py" \
                "$CUR_DIR/MyBot" | java -jar visualizer.jar
        else
            echo "Invalid no. of bots."
            popd
            exit 1
        fi
        popd
    fi
    wait_key
done
