#include <stdio.h>
#include <string.h>

static int
find(char blocks[][33], int n, unsigned long used, const char *word)
{
    if (*word < 'a' || *word > 'z') {
        return 1;
    } else {
        for (int i = 0; i < n; i++)
            if (!((used >> i) & 1) && strchr(blocks[i], *word))
                if (find(blocks, n, used | (1ul << i), word + 1))
                    return 1;
    }
    return 0;
}

int
main(int argc, char **argv)
{
    int n = 0;
    static char blocks[32][33];
    while (fgets(blocks[n], sizeof(blocks[n]), stdin))
        n++;

    FILE *words = fopen(argv[argc - 1], "r");
    char line[64];
    while (fgets(line, sizeof(line), words))
        if (!find(blocks, n, 0, line))
            fputs(line, stdout);
    fclose(words);
}
