#!/bin/bash
# _version_: 0.1
# __author_: Pedro Chinen
# ____date_: 2015-12-16

# Description:
# Script to export plain text (Markdown) into html using the
# (http://daringfireball.net/projects/markdown/) perl command.

# Check to see if the script has the number of parameters required
if [ $# -ne 1 ]; then
    echo -e "Number of parameters wrong: $# is not equal to 1"
    echo -e "Program has only name of Markdown file as parameter"
    exit 1
fi

markdownProgram="$HOME/git/dotfiles/bin/perl/Markdown.pl"
auxFile="testeMarkdown.html"

# Execute perl script
perl -w  $markdownProgram $1 > $auxFile

# Show result in the terminal
echo "begin of markdown"
echo "================="
cat $auxFile
echo "================="
echo "end of markdown"

# show result in firefox
firefox $auxFile &

# wait some time and destroy aux file
sleep 1
rm $auxFile

exit 0



