/**
 * Author: Cory Ness
 */

/**
 * This program will mimic the functionality of Cat, including the following flags:
 * - E: Add a $ at the end of every line
 * - n: Write the line number before each line
 * - b: Same as n, but does not add line numbers to empty lines
 * If -b and -n are added, b is used
 * 
 * Program allows multiple file names, and multiple user-inputs designated with '-'
 */

#include <string.h> // !< strlen
#include <stdio.h>  // !< printf, fprintf, FILE, fopen, fclose, fgets
#include <stdlib.h> // !< malloc

/* Macros to set flags a little easier */
#define FLAG_b(val) val |= 2
#define FLAG_n(val) val |= 1
#define FLAG_E(val) val |= 1

/* Macros to read flags a little easier */
#define USING_b (_appVariables.lineNumberState & 2)
#define USING_n (_appVariables.lineNumberState & 1)
#define USING_E (_appVariables.endLineState & 1)

/* Keep a struct containing command state variables */
struct
{
    int endLineState; // !< Second bit is -b, first bit is -n
    int lineNumberState; // !< First bit is -E
    int filesToKat; // !< Number of files we need to Kat
    char ** fileNames; // !< Array of strings containing Kat files
    int runningLineCount;

    int newLined; // !< Bool used to determine if we've passed a newline
}_appVariables; /* Name it _appVariables */

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
        /* Treat it as a flag, we will modify the endline/linenumber state variables */
        for(int i = 1; arg[i]; ++i)
        {
            if(arg[i] == 'b') FLAG_b(_appVariables.lineNumberState);
            if(arg[i] == 'n') FLAG_n(_appVariables.lineNumberState);
            if(arg[i] == 'E') FLAG_E(_appVariables.endLineState);
        }
        /* If flag has no arguments, it's actually user input designator */
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

/**
 * Prints the specified line using command arguments
 * - currentLine: String to print
 */
void PrintLine(char * currentLine)
{
    /* If we've new-line'd, then we can add line numbers */
    if(_appVariables.newLined)
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
        /* Make sure we clear new-line'd, so that we don't write a ton of line numbers */
        _appVariables.newLined = 0;
    }
    /* Remove the newline at the end of the line */
    char charToCheck = currentLine[strlen(currentLine)-1];
    if(charToCheck == '\n') currentLine[strlen(currentLine)-1] = '\0'; 
    /* Print the line */
    printf("%s", currentLine);
    /* Add a $ if using E & we reached end of line */
    if(USING_E && charToCheck == '\n')
    {
        printf("$");
    }
    /* Add newline if it existed before*/
    if(charToCheck == '\n')
    {
        printf("\n");
        _appVariables.newLined = 1;
    }
}

/**
 * Prints the file using the command arguments
 * - fileName: Name of file to open (\0 if prompting user input)
 * returns 0 if opened file succesfully, else -1 if error occured
 */
int PrintFile(char * fileName)
{
    FILE * file;
    /* If first character is null, than use stdin instead of a file */
    if(fileName[0] == '\0')
        file = stdin;
    else
        file = fopen(fileName, "r"); /* Open file in readonly */

    if(file == NULL) return -1; /* Return error, because file couldn't open */

    char currentLine[101];
    currentLine[100] = '\0'; /* Ensure last character is always null-terminated */
    while(fgets(currentLine, 100, file))
    {
        PrintLine(currentLine); /* Print every line in file */
    }

    if(fileName[0] != '\0')
        fclose(file); // !< If we used an open file, we need to close it
    else
        clearerr(stdin); // !< If we used stdin, we need to clear the error

    return 0;
}

/**
 * Main program, pass command-line arguments into here
 */
int main(int argc, char ** args)
{
    /* Initialize command state variables */
    _appVariables.runningLineCount = 1;
    _appVariables.filesToKat = 0;
    _appVariables.newLined = 1;

    /* Max number of filenames is argc - 1 */
    _appVariables.fileNames = malloc(sizeof(char *) * (argc - 1));

    /* Process every argument */
    for(int i = 1; i < argc; ++i)
    {
        ProcessArgument(args[i]);
    }

    /* For every file we need to print, print it */
    for(int i = 0; i < _appVariables.filesToKat; ++i)
    {
        /* If we couldn't open the file, tell user in stderr */
        if(PrintFile(_appVariables.fileNames[i]) == -1)
        {
            fprintf(stderr, "kat: %s: No such file\n", _appVariables.fileNames[i]);
        }
    }

    /* If there are not files to print, than prompt user instead */
    if(_appVariables.filesToKat == 0)
    {
        PrintFile("\0"); // !< Passing "\0" tells PrintFile to prompt user
    }
}
