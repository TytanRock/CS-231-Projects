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

#include "helpers.h"

/*
 * Main Program
 */
int main(int argc, char **args)
{
    printf("Author is Cory Ness\n");
    printf("Date authored is 4/10/2019\n");
    /* Check if there's command line arguments */
    if(argc > 1)
    {
        /* Let user know this does not support command-line arguments and return */
        printf("This does not support command line arguments");
        return 0;
    }

    /* Create variable to keep track of user input */
    char ** userInput;
    userInput = GetUserInput();

    /* Process the lines */
    int number;
    int totalNumber = 0;
    int currentCharCount[CHARS_IN_ALPHABET];
    int totalCharCount[CHARS_IN_ALPHABET];
    memset(currentCharCount, 0, sizeof(int) * CHARS_IN_ALPHABET); // !< Sets all values to 0
    memset(totalCharCount, 0, sizeof(int) * CHARS_IN_ALPHABET); // !< Sets all values to 0
    for(int i = 0; i < MAX_LINES && userInput[i][0]; ++i)
    {
        ProcessLine(userInput[i], &number);

        char * charCountString = ProcessChars(userInput[i], currentCharCount);
        for(int i = 0; i < CHARS_IN_ALPHABET; ++i)
        {
            totalCharCount[i] += currentCharCount[i];
            currentCharCount[i] = 0;
        }

        printf("%d: %s - There are %d words\n", i+1, userInput[i], number);
        totalNumber += number;
        printf(" - Character Counts are:%s\n", charCountString);
        free(charCountString);
    }
    char * totalCountString = GetCharCountString(totalCharCount);
    printf("Total number of words are: %d\n", totalNumber);
    printf("Total character count is:%s\n", totalCountString);
    return 0;
}
