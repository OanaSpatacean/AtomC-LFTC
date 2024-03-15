#include <stdio.h>
#include <stdlib.h>
#include "utils.h"
#include "lexer.h"
#include "parser.h"

int main(void)
{
    char *inbuf = loadFile("tests/testparser.c"); //testlex.c
    //puts(inbuf);

    Token *tokens = tokenize(inbuf);
    showTokens(tokens);

    parse(tokens);

    free(inbuf);
    return 0;
}