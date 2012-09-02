#!/bin/bash

git submodule init
git submodule update

cd ~/.emacs.d/vendor

wget http://www.northbound-train.com/emacs/dired-single.el

