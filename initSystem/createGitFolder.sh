#!/bin/bash

DIR=$PWD

if [! -d $HOME/git ]; then
    echo "creating Git Folder"
    mkdir $HOME/git
fi

# go to directory
cd $HOME/git

if[! -d /lab-ic ]; then
    git clone https://github.com/chinen93/lab-ic.git lab-ic
    # Project forked add remote
    cd /lab-ic
    git remote add upstream https://github.com/jwnx/lab-ic.git
    cd ..
fi

if[! -d /mc102-unicamp ]; then
    git clone https://gitlab.com/hiderleichinas/mc102-unicamp.git mc102-unicamp
fi

if[! -d /dotfiles ]; then
    git clone https://gitlab.com/hiderleichinas/dotfiles.git dotfiles
fi

# come back
cd $PWD
   
