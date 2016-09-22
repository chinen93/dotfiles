

ORG_CONFIG="/home/pchinen/.emacsConfig/init-orgmode.el"

emacs -nw -q -load $ORG_CONFIG -eval "(progn (load-theme (quote whiteboard)) (my-week-and-todo-list))"

# emacs -nw -q -load "/home/pchinen/.emacsConfig/init-orgmode.el" -eval "(progn (load-theme (quote whiteboard)) (my-week-and-todo-list))"

