#!/bin/bash

function runCmd {
  ssh -p 222 root@arbital.alphasheets.com "$1"
}

# Frontend
runCmd "mkdir /www"
runCmd "rm -rf /www/arbital"
scp -P 222 -r dist/frontend root@arbital.alphasheets.com:/www/arbital
runCmd "nginx -s reload"

# Backend
runCmd "killall tmux"
scp -P 222 dist/backend-exe root@arbital.alphasheets.com:~/
runCmd "tmux new -s backend -d \"./backend-exe\""