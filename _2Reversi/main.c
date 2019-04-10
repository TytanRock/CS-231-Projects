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

/*
 * Function used to get user input
 * Returns an array of c string (char arrays)
 */
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
    /* We've exited, let's add some newlines to clean up the terminal a bit */
    printf("\n\n\n");

    /* We've read all the user input, let's return */
    return buf;
}

/*
 * Used to reverse text up to specified length
 * word - String to reverse
 * len - Length to reverse word
 * word WILL be modified
 */
void ReverseWord(char * word, int len)
{
    char * tmpWord = calloc(len, sizeof(char)); // !< DYNAMICALLY ALLOCATED

    /* Each consecutive character is the last consecutive character */
    for(int i = 0; i < len; ++i)
    {
        tmpWord[i] = word[len - i - 1];
    }
    /* And then we set the original word to the reversed word */
    for(int i = 0; i < len; ++i)
    {
        word[i] = tmpWord[i];
    }
    /* Free memory used by the temporary word */
    free(tmpWord);
}

/*
 * Processes a line at a time according to the program's spec
 * stringLine - String to process
 * numberOfWords - sets the humber of words in this line
 * stringLine WILL be modified
 * numberOfWords WILL be modified
 */
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

/*
 * Main Program
 */
int main(int argc, char **args)
{
    /* Check if there's command line arguments */
    if(argc > 1)
    {
        /* Let user know this does not support command-line arguments and return */
        printf("This does not support command line arguments");
        return 0;
    }

    /* Create variable to keep track of user input */
    char ** buf;
    buf = GetUserInput();

    /* Process the lines */
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