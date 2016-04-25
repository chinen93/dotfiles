#!/bin/bash
# _version_: 0.1
# __author_: Pedro Chinen
# ____date_: 2016-04-21

# Description:
# Set configuration for gnome

# gsettings does not exist: abort
which gsettings || exit 1;

# log message
echo 'Setting configuration'
echo 'Show what was before and what is now'

# gnome keybindings
gsettings set org.gnome.desktop.wm.keybindings set begin-move "['<Alt>e']"
gsettings set org.gnome.desktop.wm.keybindings set close "['<Alt>F4']"
gsettings set org.gnome.desktop.wm.keybindings set panal-run-dialog "['<Alt>F2']"

# gnome preferences
gsettings set org.gnome.desktop.wm.preferences set mouse-button-modifier "<Alt>"
gsettings set org.gnome.desktop.wm.preferences set auto-raise true
gsettings set org.gnome.desktop.wm.preferences set auto-raise-delay 250
gsettings set org.gnome.desktop.wm.preferences set focus-mode "mouse"
gsettings set org.gnome.desktop.wm.preferences set resize-with-right-button true
