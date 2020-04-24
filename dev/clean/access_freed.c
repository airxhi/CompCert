#include <stdlib.h>
#include <stdio.h>

int main(){
    int *x;
    x = malloc(sizeof(int)*32);
    if (x == NULL){
        return 0;
    }
    x[40] = 1;
    free(x);
    x[0] = 2;
    printf("%d", x[0]);
    return 0;
}
