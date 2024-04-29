#include <stdio.h>
#include <stdlib.h>
#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "vm.h"

int main(void)
{
    char *inbuf = loadFile("tests/testgc.c"); 
    //puts(inbuf);

    Token *tokens = tokenize(inbuf);
    //showTokens(tokens);

    pushDomain(); //creaza domeniul global in tabela de simboluri
    vmInit(); // initializare masina virtuala

    parse(tokens); //analiza sintactica

    //showDomain(symTable, "global"); //afisare domeniu global

    //Instr *testCode=genTestProgram(); // genereaza cod de test pentru masina virtuala
    //run(testCode); // executie cod masina virtuala

    Symbol *symMain = findSymbolInDomain(symTable, "main");

    if (!symMain) {
        err("missing main function");
    }

    Instr *entryCode = NULL;
    addInstr(&entryCode, OP_CALL)->arg.instr = symMain->fn.instr;
    addInstr(&entryCode, OP_HALT);
    run(entryCode);

    //dropDomain();

    free(inbuf);
    return 0;
}