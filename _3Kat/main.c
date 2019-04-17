/**
 * Author: Cory Ness
 */

/**
 * This program will mimic the functionality of Cat, including the following flags:
 * - E: Add a $ at the end of every line
 * - n: Write the line number before each line
 * - b: Same as n, but does not add line numbers to empty lines
 * If -b and -n are added, b is used
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define FLAG_b(val) val |= 2
#define FLAG_n(val) val |= 1
#define FLAG_E(val) val |= 1

#define USING_b (_appVariables.lineNumberState & 2)
#define USING_n (_appVariables.lineNumberState & 1)
#define USING_E (_appVariables.endLineState & 1)

struct
{
    int endLineState; // !< Second bit is -b, first bit is -n
    int lineNumberState; // !< First bit is -E
    int filesToKat; // !< Number of files we need to Kat
    char ** fileNames; // !< Array of strings containing Kat files
    int runningLineCount;
}_appVariables;

/**
 * Processes the specified argument, modifying state variables
 * - arg: string of the argument
 */
void ProcessArgument(char *arg)
{
    int size = strlen(arg);
    if(size <= 0) return; /* Return if the size is zero or less */

    if(arg[0] == '-')
    {
        /* This is a flag, we will modify the endline/linenumber state variables */
        for(int i = 1; arg[i]; ++i)
        {
            if(arg[i] == 'b') FLAG_b(_appVariables.lineNumberState);
            if(arg[i] == 'n') FLAG_n(_appVariables.lineNumberState);
            if(arg[i] == 'E') FLAG_E(_appVariables.endLineState);
        }
        if(arg[1] == '\0')
        {
            _appVariables.fileNames[_appVariables.filesToKat] = "\0";
            ++_appVariables.filesToKat;
        }
    }
    else
    {
        /* This is not a flag, so it's actually a filename */
        _appVariables.fileNames[_appVariables.filesToKat] = arg;

        /* Incrememnt files to Kat */
        ++_appVariables.filesToKat;
    }
}

void PrintLine(char * currentLine)
{
        if(USING_b)
        {
            /* Check if this is an empty line */
            if(strlen(currentLine) > 1)
            {
                printf("%6d  ", _appVariables.runningLineCount++);
            }
        }
        else if(USING_n)
        {
            printf("%6d  ", _appVariables.runningLineCount++);
        }
        /* Remove the newline at the end of the line */
        char charToCheck = currentLine[strlen(currentLine)-1];
        if(charToCheck == '\n') currentLine[strlen(currentLine)-1] = '\0'; 
        /* Print the line */
        printf("%s", currentLine);
        /* Add a $ if using E */
        if(USING_E && charToCheck == '\n')
        {
            printf("$");
        }
        /* Add newline if it existed before*/
        if(charToCheck == '\n')
            printf("\n");
}

int PrintFile(char * fileName)
{
    FILE * file;
    if(fileName[0] == '\0')
        file = stdin;
    else
        file = fopen(fileName, "r");

    if(file == NULL) return -1; /* Return error, because file couldn't open */

    char currentLine[101];
    currentLine[100] = '\0';
    while(fgets(currentLine, 100, file))
    {
        PrintLine(currentLine);
    }

    fclose(file);

    return 0;
}

int main(int argc, char ** args)
{
    _appVariables.runningLineCount = 1;
    _appVariables.filesToKat = 0;
    /* Max number of filenames is argc - 1 */
    _appVariables.fileNames = malloc(sizeof(char *) * (argc - 1));

    for(int i = 1; i < argc; ++i)
    {
        /* Process every argument passed down */
        ProcessArgument(args[i]);
    }

    for(int i = 0; i < _appVariables.filesToKat; ++i)
    {
        PrintFile(_appVariables.fileNames[i]);
    }

    if(_appVariables.filesToKat == 0)
    {
        char charBuffer[101];
        charBuffer[100] = '\0';
        while(fgets(charBuffer, 100, stdin))
        {
            PrintLine(charBuffer);
        }
    }
}
