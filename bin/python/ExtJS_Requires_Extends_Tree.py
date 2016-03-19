# _version_: 0.1
# __author_: Pedro Chinen
# ____date_: 2016-03-19

# Description:
# This script get the requires and extends from ExtJs classes
# name: Converja.charts.answeredvsmissed.AnsweredVsMissed
# file: packages/cvj-charts-answered-vc-missed/src/AnsweredVsMissed.js
#
# name: Converja.charts.pie.Pie
# file: packages/cvj-core/src/Pie.js
#
# Make an unique file from the Initial Class with all the classes that are
# used by this class
#
# -packages/cvj-charts-answered-vc-missed/src/AnsweredVsMissed.js
# --npackages/cvj-core/src/Pie.js
#

# Usage:
# python3 ExtJs_Require_Extends_Tree.py [file class initial]
# $# == 1
# $1 == path to inicial class
# 


#
# IMPORTS
#
import sys
import getopt
import re


#
# CONSTANTS
#  Converja.charts.answeredvsmissed.AnsweredVsMissed
#  Converja.charts.answeredvsmissed.AnsweredVsMissed
#  Converja.misc.chat.Chat
# Converja\.\([A-Za-z]\|.\)+
 
REGEX_CONVERJA = r'Converja\.([A-Za-z]|\.)+'
REGEX_EXT      = r'Ext\.([A-Za-z]|\.)+'
REGEX_SAMPLE   = r'Sample\.([A-Za-z]|\.)+'

#
# CODE
#

def main(pathInitialFile):
    '''
    I create the tree
    
    @param pathInitialFile: Path to file
    @type  pathInitialFile: String

    '''
    converjaCoreFiles = {}
    
    converjaFiles = {}
    extFiles      = {}
    sampleFiles   = {}
    unorderedFiles = {}

    print(pathInitialFile)
    getAllFiles('/home/pchinen/Dropbox/vulcanet.org')
    getAllFiles('/home/pchinen/Dropbox/vulcanet.orgadsdadsa')
# main()


def getAllFiles(pathFile):
    '''
    I get all the files in the file 
    
    @param pathFile: Path to file
    @type  pathFile: String

    @return: Dictionary with all the files in param file
    @rType : dictionary
    '''
    numberOfLines = 15
    try:
        # open file
        searchedFile= open(pathFile, 'r')
        
        # set the regular expression to use
        regexConverja = re.compile(REGEX_CONVERJA)
        regexExt      = re.compile(REGEX_EXT)
        regexSample   = re.compile(REGEX_SAMPLE)
        
        # search in every line for any regex
        # TODO para testar em ~/vulcanet.org as classes estao no final do arquivo
        for i in range(numberOfLines):
            line = searchedFile.readline().strip()
            converja = regexConverja.match(line)
            ext      = regexExt.match(line)
            sample   = regexSample.match(line)

            # see if the reference isn't in the dictionary yet
            if converja is not None and not converjaFiles.has_key(converja):
                converjaFiles[converja] = getPath(converja)

            #TODO terminar de fazer os casos
                
            print(line)
    except IOError:
        print('Error file [' + pathFile + '] do not exist.')
        
    
# getAllFiles()

def getPath(className):
    print('getPath')
# getPath()


if __name__ == '__main__':
    # verify the options that were given to the program before
    # calling the main function
    try:
        opts, args = getopt.getopt(sys.argv[1:], "ho:v", ["help", "output="])
        main(args[0])
    except getopt.GetoptError as err:
        # print help information and exit:
        print(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)
