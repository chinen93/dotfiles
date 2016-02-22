## 
## $1 - Arquivo Jar
##
## Requerimentos:
## - gtags
## - java
## - Java decompiler: I'm using https://bitbucket.org/mstrobel/procyon/wiki/Java%20Decompiler
##
## Decompilar todos os arquivos .class do JVM padrão.
## Usar o gtags e htags nesses novos arquivos.
## Poder usar os arquivos criados pelo gtags pelo emacs
## 

FILEJAR="rt.jar"
DIRJAR="/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.65-15.b17.fc23.x86_64/jre/lib/"
DIRPROGRAM="~/git/dotfiles/bin/java/"
DIROUT="~/Documents/java_decompilado/"
PROGRAM="procyon-decompiler-0.5.30.jar"

$(java -jar $DIRPROGRAM$PROGRAM -jar $DIRJAR$FILEJAR -o $DIROUT)
cd $DIROUT
gtags
htags

exit 0


