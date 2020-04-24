
#include<stdlib.h>

int foo(int a, int b){
    return a * b;
}

int *bar(int n){
    int *x = malloc(n*sizeof(int));
    int i;
    for(i = 0; i < n; i++){
        x[i] = foo(i,n);
    }
    return x;
}