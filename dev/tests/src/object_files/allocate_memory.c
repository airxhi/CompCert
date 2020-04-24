#include <stdlib.h>

int * getPointer(){
    int *x = (int *) malloc(sizeof(int));
    int *y = (int *) malloc(sizeof(int));
    free(y);
    x[0] = 1;
    return x;
}
