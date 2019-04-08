#include <stdio.h>
#include "windows.h"

int main (int argc, char ** args, char ** env)
{
    printf("Hello world\n");
    printf("Environment Variables: %s\n", env[0]);
    return 0;
}