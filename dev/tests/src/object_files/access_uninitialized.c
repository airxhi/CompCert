#include <stdlib.h>
int getValue(){
    int *x = malloc(sizeof(int)*20);
    x[0]=1;
    return x[0];
}