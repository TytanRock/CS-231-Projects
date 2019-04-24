
#define MAX_WORD_LENGTH 100

#include <stdio.h> 
#include <string.h>
#include <stdlib.h> 
#include <unistd.h>


int main(int argc, char ** args)
{
    int link[2];

    char fileToCheck[MAX_WORD_LENGTH];
    strcpy(fileToCheck, args[1]);
    char * lexArgs[] = {"./lex.out", fileToCheck, NULL};

    if(pipe(link) == -1)
    {
        return -1;
    }

    int pid = fork();
    if(pid == 0)
    {
        dup2(link[1], STDOUT_FILENO);
        close(link[0]);
        close(link[1]);
        execvp(lexArgs[0], lexArgs);
    }
    else
    {
        close(link[1]);
        char word[MAX_WORD_LENGTH];
        FILE * lexOutput = fdopen(link[0], "r");
        while(fgets(word, MAX_WORD_LENGTH, lexOutput))
            printf("Out: %s", word);
        close(link[0]);
        wait(NULL);
    }
}