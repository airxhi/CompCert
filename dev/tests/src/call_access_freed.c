#include <stdlib.h>
#include <stdio.h>

int *getPointer();

int main(){
    int *x = getPointer();
    printf("%d\n", x[0]);
    return 0;
}
