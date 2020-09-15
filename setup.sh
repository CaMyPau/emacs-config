#!/bin/sh

SCRIPT_DIR="$( cd "$( dirname "$0" )" >/dev/null 2>&1 && pwd )"

ln -sf ${SCRIPT_DIR}/.emacs ${HOME}/

EMACS_DIR='.emacs.d'
rm -rf ${HOME}/${EMACS_DIR}
ln -sf ${SCRIPT_DIR}/${EMACS_DIR} ${HOME}/

sudo apt install \
     ttf-anonymous-pro

emacs --eval "(package-install-selected-packages)"
