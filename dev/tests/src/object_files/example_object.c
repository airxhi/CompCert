#include <stdlib.h>
int bar(int n){
    int y = 0;
    int *x = malloc(n * sizeof(int));
    x[0] = 1;
    y = x[0];
    free(x);
    return y;
}
int foo(int a, int b){
    return bar(a) + b;
}