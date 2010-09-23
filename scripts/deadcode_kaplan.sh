#!/bin/bash

cd ~/www

file=$1

if [ "[$file]" = "[]" ]; then
  git show --pretty="format:" --name-only | grep ".php*" | sort | uniq | xargs --no-run-if-empty ~/bin/deadcode_kaplan.sh
  exit
fi

while [ "[$file]" != "[]" ]
do
  if [ -f $file ]; then
    grep "function *" $file | grep \( | sed "s/(.*/(/" | sed "s/.* //" >$HOME/.tmp.funcs
    cat $HOME/.tmp.funcs | xargs echo | perl -lane 'print join("\\\\\|", @F)'|xargs -I {} scan '{}'>$HOME/.tmp.func_finds
    cat $HOME/.tmp.funcs | xargs -n 1 -I {} bash -c 'echo "{}" `grep -m 2 "{}" $HOME/.tmp.func_finds | wc -l`'| grep -E ' 1$' | awk '{print $1}' | xargs --no-run-if-empty -n 1 scan
  fi
  shift
  file=$1
done
