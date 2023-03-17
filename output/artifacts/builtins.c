#include <stdio.h>

char* print(char* value) {
    puts(value);
    return value;
}

char print_char(char value) {
    printf("%c\n", value);
    return value;
}
