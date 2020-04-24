#include <stdlib.h>
#include <stdio.h>

int main(){
    int *x;
    x = malloc(sizeof(int)*32);
    x[0] = 1;
    free(x);
    free(x);
    printf("%d", x[0]);
    return 0;
}
