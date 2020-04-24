#include <unistd.h>
int foo()
{
    char str[] = "Hello world\n";

    /* Possible warnings will be encountered here, about implicit declaration
     * of `write` and `strlen`
     */
    write(1, str, 12);
    /* `1` is the standard output file descriptor, a.k.a. `STDOUT_FILENO` */

    return 0;
}