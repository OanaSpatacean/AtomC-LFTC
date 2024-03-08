#include <stdio.h>
#include <stdlib.h>
#include "utils.c"
#include "lexer.c"

int main(void)
{
    char *inbuf = loadFile("tests/testlex.c"); //1.c
    puts(inbuf);

    Token *tokens = tokenize(inbuf);
    showTokens(tokens);

    free(inbuf);
    return 0;
}