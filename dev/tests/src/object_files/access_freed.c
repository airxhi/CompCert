#include <stdlib.h>
#include <stdio.h>

int* getPointer(){
    int *x;
    x = malloc(sizeof(int)*32);
    x[0] = 1;
    //free(x);
    x[0] = 2;
    //printf("%d", x[0]);
    return x;
}
