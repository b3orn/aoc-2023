/*
 * cc -o main main.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>


#define BUFFER_SIZE 1024


int
get_digit(char *line) {
    if (isdigit(line[0])) {
        return line[0] - 48;
    } else if (!strncmp(line, "one", 3)) {
        return 1;
    } else if (!strncmp(line, "two", 3)) {
        return 2;
    } else if (!strncmp(line, "three", 5)) {
        return 3;
    } else if (!strncmp(line, "four", 4)) {
        return 4;
    } else if (!strncmp(line, "five", 4)) {
        return 5;
    } else if (!strncmp(line, "six", 3)) {
        return 6;
    } else if (!strncmp(line, "seven", 5)) {
        return 7;
    } else if (!strncmp(line, "eight", 5)) {
        return 8;
    } else if (!strncmp(line, "nine", 4)) {
        return 9;
    }

    return -1;
}


uint64_t
process_file(FILE *file) {
    uint64_t result, length, i, n;
    int d;
    char c, line[BUFFER_SIZE];

    result = 0;

    while (fgets(line, BUFFER_SIZE, file)) {
        length = strnlen(line, BUFFER_SIZE);
        n = 0;

        for (i = 0; i < length; ++i) {
            if ((d = get_digit(&line[i])) >= 0) {
                n = d * 10;
                break;
            }
        }

        for (i = length; i > 0; --i) {
            if ((d = get_digit(&line[i - 1])) >= 0) {
                n += d;
                break;
            }
        }

        result += n;
    }

    return result;
}


int
main(int argc, char **argv) {
    FILE *file;
    if (argc < 2) {
        printf("usage: %s filename\n", argc > 0 ? argv[0] : "main");
        return EXIT_FAILURE;
    }

    if (!(file = fopen(argv[1], "r")))
        return EXIT_FAILURE;

    printf("%llu\n", process_file(file));

    fclose(file);

    return EXIT_SUCCESS;
}
