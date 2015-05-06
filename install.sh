#!/bin/bash

dirAtual=$(pwd)
dirFiles=$(pwd)/files

if [ ! -d $dirAtual/dotfiles_old  ];then
    mkdir $dirAtual/dotfiles_old
fi

cd $dirFiles
for filename in *; do
    file=.$filename
    echo "##################################"
    echo "$file"
    if [ -f $HOME/$file ];then
	echo "Backup de $HOME em $dirAtual/dotfiles_old"
	mv $HOME/$file $dirAtual/dotfiles_old
    fi
    if [ -L $HOME/$file ];then
	echo "Removendo link $HOME/$file"
	rm $HOME/$file
    fi
    echo "Linkando de $dirFiles para $HOME"
    ln -s $dirFiles/$filename $HOME/$file
done;
echo ""
cd $dirAtual
