#include <stdlib.h>
#include <stdio.h>

int extr(int a, int b, int c, int d);

int foo(int a, int b){
	return a + b;
}

int bar(int a){
    if (a < 0){
        return 0;
    }
    int c = 0;
    int total = 0;
    while (c < a){
        total += c;
        c++;
    }
    return total;
}

int main(){
	int y[10];
    int x;
    float z;
    int a;
    
    z = 0.4f;
    z += 1;
    
    x = foo(1,2);
    a = extr(5,3,1,2);

    // array of 10 ints
    // malloc(32);

    // array of 10 floats from malloc
    // float *ptr;
    // ptr = malloc(100 * sizeof(float));
    // free(ptr);

    // ptr[4] = 4;
    printf("%d\n", a);
    return a;
}
