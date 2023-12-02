/*
 * cc -o main main.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>


#define BUFFER_SIZE 1024
#define N_RED 12
#define N_GREEN 13
#define N_BLUE 14


void
process_file(FILE *file) {
    uint64_t result, power_sum, length;
    int game_id, offset, n, m, max_red, max_green, max_blue;
    char line[BUFFER_SIZE], color[16], *data;
    bool invalid_game;

    result = 0;
    power_sum = 0;

    while (fgets(line, BUFFER_SIZE, file)) {
        length = strnlen(line, BUFFER_SIZE);
        max_red = 0;
        max_green = 0;
        max_blue = 0;
        invalid_game = false;

        if (!sscanf(line, "Game %d: %n", &game_id, &offset))
            continue;

        while (offset < length) {
            data = &line[offset];

            sscanf(data, "%d%n", &n, &m);

            offset += m + 1;

            if (!strncmp(&line[offset], "red", 3)) {
                offset += 3;
                if (n > max_red)
                    max_red = n;

                if (n > N_RED)
                    invalid_game = true;

            } else if (!strncmp(&line[offset], "green", 5)) {
                offset += 5;
                if (n > max_green)
                    max_green = n;

                if (n > N_GREEN)
                    invalid_game = true;

            } else if (!strncmp(&line[offset], "blue", 4)) {
                offset += 4;
                if (n > max_blue)
                    max_blue = n;

                if (n > N_BLUE)
                    invalid_game = true;
            }

            offset += 2;
        }

        power_sum += max_red * max_green * max_blue;

        if (!invalid_game)
            result += game_id;
    }

    printf("game id sum: %lld\n", result);
    printf("power sum: %lld\n", power_sum);
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

    process_file(file);

    fclose(file);

    return EXIT_SUCCESS;
}
