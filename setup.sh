#!/bin/sh
echo "Updating the .emacs"
echo "(load-file \"~/.emacs.d/config.el\")" > ~/.emacs
touch ~/.emacs.d/.mybkmrks
