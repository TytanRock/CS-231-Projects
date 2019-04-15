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

#define FLAG_b(val) val |= 2
#define FLAG_n(val) val |= 1
#define FLAG_E(val) val |= 1

struct 
{
    int endLineState; // !< Second bit is -b, first bit is -n
    int lineNumberState; // !< First bit is -E
    int filesToKat; // !< Number of files we need to Kat
    char ** fileNames; // !< Array of strings containing Kat files
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
    }
    else
    {
        /* This is not a flag, so it's actually a filename */
        _appVariables.fileNames[_appVariables.filesToKat] = arg;

        /* Incrememnt files to Kat */
        ++_appVariables.filesToKat;
    }
    
}

int main(int argc, char ** args)
{
    /* Max number of filenames is argc - 1 */
    _appVariables.fileNames = malloc(sizeof(char *) * (argc - 1));

    for(int i = 1; i < argc; ++i)
    {
        /* Process every argument passed down */
        ProcessArgument(args[i]);
    }

    
}