#include <string.h>
#include <stdlib.h>

int main(){
    char buf[4];
    char str[] = "Hello World!AAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
    strcpy(buf, str);
    return 0;
}