

#include<stdio.h> 
#include<stdlib.h> 
#include<unistd.h>

int main(int argc, char ** args)
{
    /* First call lex to output all the words line by line */
    char *commandArgs[] = {"./lex.exe", "test.txt"};
    execvp(commandArgs[0], commandArgs);
}