#include <stdio.h>
#include <stdlib.h>
#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "vm.h"

int main(void)
{
    char *inbuf = loadFile("tests/testat.c"); 
    //puts(inbuf);

    Token *tokens = tokenize(inbuf);
    //showTokens(tokens);

    pushDomain(); //creaza domeniul global in tabela de simboluri
    vmInit(); // initializare masina virtuala

    parse(tokens); //analiza sintactica

    //showDomain(symTable, "global"); //afisare domeniu global
    
    Instr *testCode=genTestProgram(); // genereaza cod de test pentru masina virtuala
    run(testCode); // executie cod masina virtuala
    dropDomain(); //sterge domeniul global

    free(inbuf);
    return 0;
}