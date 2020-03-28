#!/bin/bash
fswatch --recursive . | grep --line-buffered '\.hs' | xargs -n1 -I{} sh -c "cat repl.hs | sed -E $'s/(.*)/:type \\\\1\\\\\\n\\\\1/g' | stack ghci | tail -n+2 | ghead -n -1 | tail -n+${1} | sed 's/*[^>]*> //g' | sed n\;G > out"
