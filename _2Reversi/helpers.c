#include "helpers.h"

/*
 * Function used to get user input
 * Returns a DYANMICALLY CREATED array of c string (char arrays) containing user input
 * 
 * CALLER HAS RESPONSIBILITY TO FREE RETURN WHEN FINISHED WITH STRING ARRAY
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
            printf("Reached 100 lines, ending.\nPress any character to Continue.\n");
            getchar(); // !< Pause for user input
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
void ReverseString(char * word, int len)
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
 * Converts int array containing character counts to a string representing it
 *   charCount - Integer array containing count of each character 
 *     MUST BE AT LEAST AS LARGE AS 26 (the number of characters in the alphabet) 
 * Returns integer array in a readable (string) format
 * 
 * RETURN STRING IS DYNAMICALLY CREATED.
 * CALLER HAS RESPONSIBILITY TO FREE STRING WHEN FINISHED
 */
char * GetCharCountString(const int * charCount)
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
 *   line - string to process
 * Returns string with number of alphabetic chars
 * 
 * RETURN IS DYNAMICALLY CREATED, 
 * CALLER HAS RESPONSIBILITY TO FREE MEMORY
 */
char * ProcessChars(const char * line, int * charCount)
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
 * Processes a line at a time according to program spec
 * Reverses each word in stringLine
 * For each word in the line, it will incrememnt numberOfWords
 *   stringLine - String to process
 *   numberOfWords - sets the humber of words in this line
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
            ReverseString(stringLine + lastWord, i - lastWord);
            lastWord = i + 1;

            /* Make sure we increment only if this followed a non-whitespace */
            if(i > 0 && stringLine[i-1] != ' ' && stringLine[i-1] != '\n' && stringLine[i-1] != '\t')
            {
                ++ (*numberOfWords); // Incrememnt number of words
            }
        }
    }
}
