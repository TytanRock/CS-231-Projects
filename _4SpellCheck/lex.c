/**
 * Author: Cory Ness
 * 
 * This program accepts one command-line argument, a file name, and prints out all
 * words in that file
 */

#define BUF_SIZE 100
#define MAX_WORD_LENGTH 100

#define IS_WHITESPACE(val)  ((val == ' ')  || \
                             (val == '\t') || \
                             (val == '\n'))

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

int main(int argc, char ** args)
{
    /* Check for too few arguments */
    if(argc <= 1)
    {
        fprintf(stderr, "Pass at least one filename into the parameter");
        return -1;
    }

    /* Open the passed command line argument as a file name */
    FILE * file = fopen(args[1], "r");
    if(file == NULL)
    {
        fprintf(stderr, "File open failed");
        return -1;
    }

    /* Create a buffer of words */
    int currentWordCap = 100;
    int currentWord = 0;
    int currentChar = 0;
    char ** words = malloc(sizeof(char *) * currentWordCap);
    words[currentWord] = calloc(MAX_WORD_LENGTH, sizeof(char));

    /* Read the file and break it into words */
    while(fscanf(file, "%s", words[currentWord]) != EOF) 
    {
        /* Check every char, if it's not alphabetic, rescan */
        for(int i = 0; words[currentWord][i]; ++i)
        {
            if(!isalpha(words[currentWord][i]))
                goto endOfLoop;
        }
        /* If we've gone past our currentWordCap, reallocate memory to increase cap */
        ++currentWord;
        if(currentWord >= currentWordCap)
        {
            currentWordCap *= 2;
            realloc(words, sizeof(char *) * currentWordCap);
        }
        /* calloc space for the string */
        words[currentWord] = calloc(MAX_WORD_LENGTH, sizeof(char));

        endOfLoop: ;
    }
    if(file != stdin)
        fclose(file);

    /* Print them to stdout */
    for(int i = 0; words[i][0]; ++i)
    {
        printf("%s\n", words[i]);
    }
}