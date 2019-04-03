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
 */
int main (int argc, char **args)
{
    double priceOfUnitBottle;
    int numberOfBottles;

    printf("The price of a bottle: ");
    scanf("%Lf", &priceOfUnitBottle);
    printf("Number of bottles you want to purchase: ");
    scanf("%d", &numberOfBottles);

    printf("Total cost for %d bottles at %.2Lf is: %.2Lf\n", 
        numberOfBottles, priceOfUnitBottle, priceOfUnitBottle * numberOfBottles);
}