
#define MAX_WORD_LENGTH 100

#define DEBUG 0

#include <stdio.h> 
#include <string.h>
#include <stdlib.h> 
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char ** args)
{
    /* Keep track of the pipes between spellcheck and lex */
    int link[2];

    /* Create arguments to be passed into lex.out */
    char * lexArgs[] = {"./lex.out", args[1], NULL};

    /* Don't bother checking returns */
    /* TODO: Bother checking returns for failure */
    pipe(link);
    int pid = fork();
    
    /* If it's 0, this is the child process */
    if(pid == 0)
    {
        /* Duplicate output pipe to stdout */
        dup2(link[1], STDOUT_FILENO);
        /* Close input pipe */
        close(link[0]);
        /* Close output pipe b/c it's duplicated to stdout */
        close(link[1]);
        /* Execute lex.out */
        execvp(lexArgs[0], lexArgs);
        exit(EXIT_SUCCESS);
    }
    /* Otherwise this is the parent */
    else
    {
        /* Close output pipe */
        close(link[1]);
        /* Stack allocate a buffer for reading from lex.out */
        char word[MAX_WORD_LENGTH];

        /* Open a FILE from the pipe in order to use fgets */
        FILE * lexOutput = fdopen(link[0], "r");

#if DEBUG == 1
        /* While we don't have an error from fgets, repeat what was outputted */
        while(fgets(word, MAX_WORD_LENGTH, lexOutput))
            printf("Out: %s", word);
#endif

        /* Done with pipe, close input pipe */
        close(link[0]);

        /* Wait until child dies */
        waitpid(pid, NULL, 0);
    }

    /* Now, fork another process and call sort on it */

    /* Now, fork another process and call uniq on it */

    /* Now, fork another process and call compare on it */
    
}