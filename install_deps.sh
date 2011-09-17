#!/bin/bash

git submodule init
git submodule update

cd ~/.emacs.d/vendor

wget http://www.northbound-train.com/emacs/dired-single.el

cd /tmp
wget http://cx4a.org/pub/auto-complete/auto-complete-1.3.1.tar.bz2

echo "Auto-complete needs to be configured"
echo "Launch Emacs and type M-x load-file"
echo "Select file /tmp/auto-complete-1.3.1/etc/install.el"
echo "Install into ~/.emacs.d/vendor/auto-complete"



