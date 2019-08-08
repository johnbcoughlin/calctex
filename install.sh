#!/bin/bash

set -ex

emacs --batch -l org README.org -f org-babel-tangle

chmod +x macos_install.sh
chmod +x latex_install.sh

if [ "$TRAVIS_OS_NAME" == "osx" ]; then
    ./macos_install.sh
fi

./latex_install.sh
