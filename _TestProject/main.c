#include <stdio.h>
#include "windows.h"

int main (int argc, char ** args, char ** env)
{
    for(int i = 0;env[i]; ++i)
       printf("Environment Variable %d: %s\n", i, env[i]);
    return 0;
}