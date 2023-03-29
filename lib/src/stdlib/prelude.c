#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

char* print(char* value) {
    puts(value);

    return value;
}

char print_char(char value) {
    putchar(value);

    return value;
}
