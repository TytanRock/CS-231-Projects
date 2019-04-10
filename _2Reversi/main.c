/*
 * Author: Cory Ness
 * Date: 4/10/2019
 */

/*
 * Input:
 *  Multiple lines of strings
 * Output:
 *  Each line of input preceded by a number (All words reversed)
 *  Line number, # of words, count of each alphabetic character
 *  Total number of words and count of each alphabetic character
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 100
#define CHAR_BUF_SIZE 101 // !< 101 allows for strings of size 100

char ** GetUserInput()
{
    /* Dynamically allocate memory for user input */
    char ** buf = malloc(sizeof(char *) * MAX_LINES);
    /* Set everything in buf to nul */
    for(int i = 0; i < MAX_LINES; ++i)
    {
        buf[i] = calloc(CHAR_BUF_SIZE, sizeof(char));
    }
    /* Create counter to keep track of where in the buf we're at */
    int count = 0;

    printf("Enter up to %d lines of %d maximum characters\n", MAX_LINES, CHAR_BUF_SIZE - 1);
    while(fgets(buf[count], CHAR_BUF_SIZE, stdin) != NULL) /* This will break with ^z in windows, ^d in unix */
    {
        /* Valid string entered, let's increment count */
        ++count;
    }

    /* We've read all the user input, let's return */
    return buf;
}

void ReverseWord(char * word, int len)
{
    /* Just set each letter to the reversed position */
    char * tmpWord = calloc(len, sizeof(char)); // !< DYNAMICALLY ALLOCATED
    for(int i = 0; i < len; ++i)
    {
        tmpWord[i] = word[len - i - 1];
    }
    for(int i = 0; i < len; ++i)
    {
        word[i] = tmpWord[i];
    }
    free(tmpWord);
}

void ProcessLine(char * stringLine, int * numberOfWords)
{
    /* Initialize pointer to 0 */
    *numberOfWords = 0;

    int stringLength = strlen(stringLine);
    int lastWord = 0;
    for(int i = 0; i < stringLength; ++i)
    {
        if(stringLine[i] == ' ' || stringLine[i] == '\n')
        {
            /* We hit a space, let's reverse the word */
            ReverseWord(stringLine + lastWord, i - lastWord);
            lastWord = i + 1;
            ++ (*numberOfWords); // Incrememnt number of words
        }
    }
}

int main(int argc, char **args)
{
    /* Check if there's command line arguments */
    if(argc > 1)
    {
        /* Let user know this does not support command-line arguments and return */
        printf("This does not support command line arguments");
        return 0;
    }
    char ** buf;
    buf = GetUserInput();
    int number;
    int totalNumber = 0;
    for(int i = 0; buf[i][0]; ++i)
    {
        ProcessLine(buf[i], &number);
        printf("%s - There are %d words in the previous line\n", buf[i], number);
        totalNumber += number;
    }
    printf("Total number of words are: %d", totalNumber);
    return 0;
}