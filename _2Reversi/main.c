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
#include <ctype.h>

#define MAX_LINES 100
#define CHAR_BUF_SIZE 101 // !< 101 allows for strings of size 100
#define CHARS_IN_ALPHABET 26

/*
 * Function used to get user input
 * Returns an array of c string (char arrays)
 */
char ** GetUserInput()
{
    /* Dynamically allocate memory for user input */
    char ** userInput = malloc(sizeof(char *) * MAX_LINES);
    /* Set everything in userInput to nul */
    for(int i = 0; i < MAX_LINES; ++i)
    {
        userInput[i] = calloc(CHAR_BUF_SIZE, sizeof(char));
    }
    /* Create counter to keep track of where in the userInput we're at */
    int count = 0;

    printf("Enter up to %d lines of %d maximum characters\n", MAX_LINES, CHAR_BUF_SIZE - 1);
    while(fgets(userInput[count], CHAR_BUF_SIZE, stdin) != NULL) /* This will break with ^z in windows, ^d in unix */
    {
        /* 
         * If the second to last character is not null or a newline,
         * then there are more characters received than available
         * It's time to flush the buffer and set the last chars to
         * what we'd expect, truncating the input
         */ 
        if(userInput[count][CHAR_BUF_SIZE - 2] != '\0' && userInput[count][CHAR_BUF_SIZE - 2] != '\n')
        {
            char tmp[CHAR_BUF_SIZE];
            while(!strstr(tmp, "\n")) fgets(tmp, CHAR_BUF_SIZE, stdin); // Keep getting until we reach the newline

            userInput[count][CHAR_BUF_SIZE - 2] = '\n';
            userInput[count][CHAR_BUF_SIZE - 1] = '\0';
        }
        /* Valid string entered, let's increment count */
        ++count;
        if(count >= MAX_LINES)
        {
            /* We're beyond the maximum number of lines, tell user */
            printf("Reached 100 lines, ending\n");
            getchar();
            break;
        }
    }
    /* We've exited, let's add some newlines to clean up the terminal a bit */
    printf("\n------------INPUT------------\n");
    /* Print what the user entered in to make sure everyone knows what's going on */
    for(int i = 0; i < count; ++i)
    {
        printf("%s", userInput[i]);
    }
    /* Add a newline to make it a bit nicer */
    printf("------------OUTPUT------------\n");
    /* We've read all the user input, let's return */
    return userInput;
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

char * GetCharCountString(int * charCount)
{
    char * returnString = calloc(1000, sizeof(char));

    for(int i = 0; i < CHARS_IN_ALPHABET; ++i)
    {
        /* Only add to string if we have a char */
        if(charCount[i] != 0)
        {
            sprintf(returnString, "%s (%c:%d)", returnString, (char)(i+65), charCount[i]);
        }
    }
    return returnString;
}

/*
 * Process the line to get char count
 * line - string to process
 * Returns string with number of alphabetic chars
 */
char * ProcessChars(char * line, int * charCount)
{
    int stringLen = strlen(line);
    for(int i = 0; i < stringLen; ++i)
    {
        if(isalpha(line[i]))
        {
            char val = line[i];
            if(val > 90)
                val -= 32; // !< ensures we have upper case letters
            int index = (int)(val - 65); // !< Index is char value offset by 65

            charCount[index] += 1;
        }
    }
    return GetCharCountString(charCount);
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
        /* Whitespace is a space, newline, or a tab */
        if(stringLine[i] == ' ' || stringLine[i] == '\n' || stringLine[i] == '\t' || stringLine[i] == 4)
        {
            /* We hit a space, let's reverse the word */
            ReverseWord(stringLine + lastWord, i - lastWord);
            lastWord = i + 1;

            /* Make sure we increment only if this followed a non-whitespace */
            if(i > 0 && stringLine[i-1] != ' ' && stringLine[i-1] != '\n' && stringLine[i-1] != '\t')
            {
                ++ (*numberOfWords); // Incrememnt number of words
            }
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
