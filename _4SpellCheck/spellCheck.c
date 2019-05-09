
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

FILE * logFile;

void ForkProcessWithPipe(int * pipes, int * previousPipe, char * args[])
{
    if(pipe(pipes) < 0)
    {
        /* If pipe return is less than 0, the pipe failed */
        fprintf(stderr, "Pipe Failed!\n");
    }
    int pid = fork();
    /* If pid is less than 0, the fork failed */
    if(pid < 0)
    {
        fprintf(stderr, "Fork failed! Error code: %d\n", pid);
        return;
    }
    /* If it's 0, this is the child process */
    if(pid == 0)
    {
        /* Close this read pipe */
        close(pipes[0]);
        /* Duplicate previous output into this stdin */
        if(previousPipe != NULL)
            dup2(previousPipe[0], STDIN_FILENO);
        /* Duplicate this write pipe into stdout */
        dup2(pipes[1], STDOUT_FILENO);
        /* Close unused pipes */
        if(previousPipe != NULL)
            close(previousPipe[0]);
        close(pipes[1]);

        int execReturn = execvp(args[0], args);
        if(execReturn < 0)
        {
            fprintf(stderr, "Exec failed! Error code is: \n\
            Check there exists \"%s\" in your directory", execReturn, args[0]);
        }

        exit(EXIT_SUCCESS);
    }
    /* Otherwise this is the parent process */
    else
    {
        /* Close unused pipes */
        close(pipes[1]);
        if(previousPipe != NULL)
            close(previousPipe[0]);

        /* Wait until sort process is done */
        waitpid(pid, NULL, 0);

        fprintf(logFile, "Process ID %d and name %s exited\n", pid, args[0]);
    }
}

int main(int argc, char ** args)
{
    if(argc != 3)
    {
        fprintf(stderr, "Please specify two files as command line arguments");
    }

    logFile = fopen("spellcheck.log", "w");

    /* Fork process to call lex on specified file */
    int lexLink[2];
    char * lexArgs[] = {"./lex.out", args[1], NULL};

    ForkProcessWithPipe(lexLink, NULL, lexArgs);

    /* Now, fork another process and call sort on it */
    int sortLink[2];
    char * sortArgs[] = {"sort", NULL};

    ForkProcessWithPipe(sortLink, lexLink, sortArgs);

    /* Now, fork another process and call uniq on it */
    int uniqLink[2];
    char * uniqArgs[] = {"uniq", "-i", NULL}; /* -i flag to ignore case */
    
    ForkProcessWithPipe(uniqLink, sortLink, uniqArgs);
    
    /* Now, fork another process and call compare on it */
    int compareLink[2];
    char *compareArgs[] = {"./compare.out", "-", args[2], NULL};

    ForkProcessWithPipe(compareLink, uniqLink, compareArgs);

    /* Close logger file */
    fclose(logFile);

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
