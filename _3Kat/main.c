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

struct 
{
    int endLineState;
    int lineNumberState;
    char * fileName;
}_appVariables;

int main(int argc, char ** args)
{

}