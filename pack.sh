#!/bin/bash
set -e
node_modules/bs-platform/lib/bs_ppx_tools.exe <(./node_modules/.bin/bsrefmt src/Js_deep.re --print binary) >(./node_modules/.bin/bsrefmt --parse binary --print re > publish/src/Js_deep.re)
sleep .1
wc -l publish/src/Js_deep.re
# cp publish/src/Js_deep.re publish-esy/src/
echo Done