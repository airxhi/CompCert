#include <stdlib.h>
#include <stdio.h>

int main(){
    int *x;
    x = malloc(sizeof(int)*32);
    x[0] = 1;
    free(x);
    int *y = malloc(32*sizeof(int));
    y[0] = 2;
    printf("%d", x[0]);
    return 0;
}
