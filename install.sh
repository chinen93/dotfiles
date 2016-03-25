#!/bin/bash

# Save the directories in variables
dirAtual=$(pwd)
dirFiles=$(pwd)/files

# Check if 'dotfiles_old/' exist, if not
# create a new one
if [ ! -d $dirAtual/dotfiles_old  ];then
    mkdir $dirAtual/dotfiles_old
fi

# go to where the files in git are stored 
cd $dirFiles

# for each file
for filename in *; do
    file=.$filename
    # put an '\.' in front of the filename
    echo "##################################"
    echo "$file"

    
    if [ ! -d $filename ]; then 
        # if file exist in $HOME
        # move it to 'dotfiles_old/' to make a backup
        if [ -f $HOME/$file ];then
	    echo "Backup de $HOME em $dirAtual/dotfiles_old"
	    mv $HOME/$file $dirAtual/dotfiles_old
        fi

        # if file is a link
        # delete the file
        if [ -L $HOME/$file ];then
	    echo "Removendo link $HOME/$file"
	    rm $HOME/$file
        fi
    fi

    # make a link between the file in $HOME
    # and the one in git
    echo "Linkando de $dirFiles para $HOME"
    ln -s $dirFiles/$filename $HOME/$file
    echo ''
done;
echo ""

# go back to atual directory
cd $dirAtual
