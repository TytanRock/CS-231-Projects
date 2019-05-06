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

    char buf[MAX_WORD_LENGTH];
    int beginningOfWord = 0;

    /* Read the file and break it into words */
    while(fgets(buf, MAX_WORD_LENGTH, file)) 
    {
        /* Check every char, if it's not alphabetic, split */
        for(int i = 0; buf[i]; ++i)
        {
            if(!isalpha(buf[i]))
            {
                if(isalpha(buf[i-1]))
                {
                    /* Assign what we've encountered so far to the array location */
                    buf[i] = '\0';
                    strcat(words[currentWord], buf + beginningOfWord);

                    /* If we've gone past our currentWordCap, reallocate memory to increase cap */
                    ++currentWord;
                    if(currentWord >= currentWordCap)
                    {
                        currentWordCap *= 2;
                        if(realloc(words, sizeof(char *) * currentWordCap) == NULL)
                        {
                            /* Something wrong happened, tell user and leave */
                            fprintf(stderr, "Realloc failed\n");
                        }
                    }
                    /* calloc space for the string */
                    words[currentWord] = calloc(MAX_WORD_LENGTH, sizeof(char));
                }
                beginningOfWord = i + 1;
            }
        }
        strcpy(words[currentWord], buf + beginningOfWord);
        beginningOfWord = 0;
    }
    if(file != stdin)
        fclose(file);

    /* Print them to stdout */
    for(int i = 0; i < currentWord + 1; ++i)
    {
        printf("%s\n", words[i]);
    }
}