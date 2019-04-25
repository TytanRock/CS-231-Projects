/**
 * Author: Cory Ness
 * 
 * This program accepts two command-line arguments of files (including stdin on one)
 * And will report differences in the first file and the second file if there are any
 */

#include <stdio.h>
#include <string.h>

#define MAX_WORD_LENGTH 100

struct {
    FILE * userInput;
    FILE * dictionary;
}_commandStates;

/**
 * compareStrings will compare the file1String against all strings in file2
 * If it finds a match, it will return 1
 * If it does not, it will return 0
 */
int compareStrings(char * file1String, char * file2String)
{
    int compareState = strcasecmp(file1String, file2String);
    if(compareState == 0) return 1;
    else if(compareState > 0) 
    {
        char * ret = fgets(file2String, MAX_WORD_LENGTH, _commandStates.dictionary);
        if(ret)
            return compareStrings(file1String, file2String);
    }
    return 0;
}

int main(int argc, char ** args)
{
    if(argc != 3)
    {
        fprintf(stderr, "Please specify 2 files to compare against, or \"-\" and a file\n");
        return -1;
    }

    if(args[1][0] == '-')
    {
        _commandStates.userInput = stdin;
    }
    else
    {
        _commandStates.userInput = fopen(args[1], "r");
    }
    _commandStates.dictionary = fopen(args[2], "r");

    char dictionaryBuf[MAX_WORD_LENGTH];
    fgets(dictionaryBuf, MAX_WORD_LENGTH, _commandStates.dictionary);
    char userBuf[MAX_WORD_LENGTH];
    while(fgets(userBuf, MAX_WORD_LENGTH, _commandStates.userInput))
    {
        int foundCompare = compareStrings(
                            userBuf, 
                            dictionaryBuf);
        if(foundCompare)
        {
            fprintf(stdout, "%s:\tSpelled Correctly\n", strtok(userBuf, "\n"));
        }
        else
        {
            fprintf(stdout, "%s:\tMisspelled\n", strtok(userBuf, "\n"));
        }        
    }
    if(_commandStates.userInput != stdin)
        fclose(_commandStates.userInput);
    fclose(_commandStates.dictionary);
}