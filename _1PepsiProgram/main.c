#include <stdio.h>

/**
 * Author: Cory Ness
 */

/**
 * You will write a C program which will
 * ask for and input the price of one bottle of
 * pepsi (double) and will ask for and input the
 * number of bottles to purchase
 * 
 * The output will be the cost of purchasing
 * these pepsis, and will be well formatted
 * and include the unit cost of and number
 * of bottles
 * 
 * Your name in a comment will be at the top of code.
 * Your name will be printed when program is run.
 */
int main (int argc, char **args)
{
    double priceOfUnitBottle;
    int numberOfBottles;

    printf("Author is Cory Ness\n");
    printf("What is the price of a bottle?\n");
    scanf("%lf", &priceOfUnitBottle);
    printf("How many bottles do you want to purchase?\n");
    scanf("%d", &numberOfBottles);

    printf("Total cost for %d bottles at $%.2Lf is: $%.2lf\n", 
        numberOfBottles, priceOfUnitBottle, priceOfUnitBottle * numberOfBottles);
}