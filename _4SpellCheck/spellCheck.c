
#define MAX_WORD_LENGTH 100

#define DEBUG_PIPE(val)     FILE * file = fdopen(val, "r"); \
                            char word[MAX_WORD_LENGTH]; \
                            while(fgets(word, MAX_WORD_LENGTH, file)) \
                                fprintf(stderr, "out: %s", word);
                            

#include <stdio.h> 
#include <string.h>
#include <stdlib.h> 
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char ** args)
{
    if(argc != 3)
    {
        fprintf(stderr, "Please specify two files as command line arguments");
    }

    /* Keep track of the pipes between spellcheck and lex */
    int lexLink[2];

    /* Create arguments to be passed into lex.out */
    char * lexArgs[] = {"./lex.out", args[1], NULL};

    /* Don't bother checking returns */
    /* TODO: Bother checking returns for failure */
    pipe(lexLink);
    int lexPid = fork();
    
    /* If it's 0, this is the child process */
    if(lexPid == 0)
    {
        /* Duplicate output pipe to stdout */
        dup2(lexLink[1], STDOUT_FILENO);
        /* Close input pipe */
        close(lexLink[0]);
        /* Close output pipe b/c it's duplicated to stdout */
        close(lexLink[1]);
        /* Execute lex.out */
        execvp(lexArgs[0], lexArgs);
        exit(EXIT_SUCCESS);
    }
    /* Otherwise this is the parent */
    else
    {
        /* Close output pipe */
        close(lexLink[1]);
        /* Wait until child dies */
        waitpid(lexPid, NULL, 0);
    }

    /**
     * lexLink[0] is NOT closed yet
     * It is used in sort, so use it in sort then close it
     */

    /* Now, fork another process and call sort on it */
    int sortLink[2];
    pipe(sortLink);
    int sortPid = fork();

    char * sortArgs[] = {"sort", NULL};

    /* If it's 0, this is the child process */
    if(sortPid == 0)
    {
        /* Close sort's read pipe */
        close(sortLink[0]);
        /* Duplicate lex's output into this stdin */
        dup2(lexLink[0], STDIN_FILENO);
        /* Duplicate sort's write pipe into stdou */ 
        dup2(sortLink[1], STDOUT_FILENO);
        /* Close unused pipes */
        close(lexLink[0]);
        close(sortLink[1]);

        execvp(sortArgs[0], sortArgs);

        exit(EXIT_SUCCESS);
    }
    /* Otherwise this is the parent process */
    else
    {
        /* Close unused pipes */
        close(sortLink[1]);
        close(lexLink[0]);

        /* Wait until sort process is done */
        waitpid(sortPid, NULL, 0);
    }

    /**
     * sortLink[0] is NOT closed yet!, be sure to close sortLink[0]
     */
    /* Now, fork another process and call uniq on it */
    int uniqLink[2];
    pipe(uniqLink);
    int uniqPid = fork();

    char * uniqArgs[] = {"uniq", "-i", NULL}; /* -i flag to ignore case */

    /* If fork ID is 0, this is child process */
    if(uniqPid == 0)
    {
        /* Close unused pipes */
        close(uniqLink[0]); // !< Read pipe
        /* Duplicate used pipes */
        dup2(sortLink[0], STDIN_FILENO); // !< stdin pipe
        dup2(uniqLink[1], STDOUT_FILENO);// !< stdout pipe
        /* Close duplicated pipes */
        close(sortLink[0]);
        close(uniqLink[1]);

        execvp(uniqArgs[0], uniqArgs);
        exit(EXIT_SUCCESS);
    }
    /* Otherwise this is the parent */
    else
    {
        /* Close unused pipes */
        close(uniqLink[1]);
        close(sortLink[0]);

        waitpid(uniqPid, NULL, 0);
    }
    
    /**
     * uniqLink[0] is NOT CLOSED yet, close it
     */
    /* Now, fork another process and call compare on it */
    int compareLink[2];
    pipe(compareLink);
    int comparePid = fork();

    char *compareArgs[] = {"./compare.out", "-", args[2], NULL};

    /* If PID is 0, this is the child proces */
    if(comparePid == 0)
    {
        /* Close any unused pipes */
        close(compareLink[0]); //!< Read pipe
        /* Duplicate any needed pipes */
        dup2(uniqLink[0], STDIN_FILENO);
        dup2(compareLink[1], STDOUT_FILENO);
        /* Close duplicated pipes */
        close(uniqLink[0]);
        close(compareLink[1]);

        execvp(compareArgs[0], compareArgs);

        exit(EXIT_SUCCESS);
    }
    /* Otherwise this is the parent process */
    else
    {
        /* Close unused pipes */
        close(uniqLink[0]);
        close(compareLink[1]);

        waitpid(comparePid, NULL, 0);
    }

    /* Now open pipe as a file and report to user */
    FILE * compareOutput = fdopen(compareLink[0], "r");
    
    char charBuf[MAX_WORD_LENGTH];
    while(fgets(charBuf, MAX_WORD_LENGTH, compareOutput))
    {
        /* Print everything as is, newline and all */
        printf("%s", charBuf);
    }
    
    close(compareLink[0]);
}