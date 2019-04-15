
#ifndef HELPERS_H
#define HELPERS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINES 100
#define CHAR_BUF_SIZE 101 // !< 101 allows for strings of size 100
#define CHARS_IN_ALPHABET 26

/*
 * Function used to get user input
 * Returns a DYANMICALLY CREATED array of c string (char arrays) containing user input
 * 
 * CALLER HAS RESPONSIBILITY TO FREE RETURN WHEN FINISHED WITH STRING ARRAY
 */
char ** GetUserInput();

/*
 * Process the line to get char count
 *   line - string to process
 * Returns string with number of alphabetic chars
 * 
 * RETURN IS DYNAMICALLY CREATED, 
 * CALLER HAS RESPONSIBILITY TO FREE MEMORY
 */
char * ProcessChars(const char * line, int * charCount);

/*
 * Converts int array containing character counts to a string representing it
 *   charCount - Integer array containing count of each character 
 *     MUST BE AT LEAST AS LARGE AS 26 (the number of characters in the alphabet) 
 * Returns integer array in a readable (string) format
 * 
 * RETURN STRING IS DYNAMICALLY CREATED.
 * CALLER HAS RESPONSIBILITY TO FREE STRING WHEN FINISHED
 */
char * GetCharCountString(const int * charCount);

/*
 * Processes a line at a time according to program spec
 * Reverses each word in stringLine
 * For each word in the line, it will incrememnt numberOfWords
 *   stringLine - String to process
 *   numberOfWords - sets the humber of words in this line
 * stringLine WILL be modified
 * numberOfWords WILL be modified
 */
void ProcessLine(char * stringLine, int * numberOfWords);

#endif