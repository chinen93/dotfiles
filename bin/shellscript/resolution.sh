#!/bin/bash
#
# Author: pedro
# File: RESOLUTION
# Copyright © 2019, pedro, all rights reserved.
# Created: 31 julho 2019
#
# Link to idea http://ubuntuhandbook.org/index.php/2017/04/custom-screen-resolution-ubuntu-desktop/
#

xrandr
cvt 1600 900
sudo xrandr --newmode "1600x900_60.00"  118.25  1600 1696 1856 2112  900 903 908 934 -hsync +vsync
sudo xrandr --addmode eDP-1 "1600x900_60.00"

# resolution.sh ends here
