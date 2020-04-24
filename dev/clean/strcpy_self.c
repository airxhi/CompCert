#include <stdio.h>
#include <string.h>

int main(){
    char * yeet = "hello";
    strcpy(yeet, yeet);
    printf("%s", yeet);
    return 0;
}
