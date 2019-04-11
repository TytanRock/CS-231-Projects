
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
 * Returns an array of c string (char arrays)
 */
char ** GetUserInput();

/*
 * Process the line to get char count
 * line - string to process
 * Returns string with number of alphabetic chars
 */
char * ProcessChars(char * line, int * charCount);

/*
 * Converts int of each character count into a string to read
 * Returns string representation of integer array of character count
 */
char * GetCharCountString(const int * charCount);

/*
 * Processes a line at a time according to the program's spec
 * stringLine - String to process
 * numberOfWords - sets the humber of words in this line
 * stringLine WILL be modified
 * numberOfWords WILL be modified
 */
void ProcessLine(char * stringLine, int * numberOfWords);

#endif