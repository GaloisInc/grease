#include <stdlib.h>
int *foo(void) {
    int *p = malloc(sizeof(int));
    *p = 0;
    return p;
}
