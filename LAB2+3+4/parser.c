#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include "parser.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

void tkerr(const char *fmt,...){
	fprintf(stderr,"error in line %d: ",iTk->line);
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}

bool consume(int code){
	if(iTk->code==code){
		consumedTk=iTk;
		iTk=iTk->next;
		return true;
		}
	return false;
	}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef() {
    return false;
}
	
// varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef() {
    return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
//totul sau nimic!!
bool typeBase(){
	if(consume(TYPE_INT)){
		return true;
		}
	if(consume(TYPE_DOUBLE)){
		return true;
		}
	if(consume(TYPE_CHAR)){
		return true;
		}
	if(consume(STRUCT)){
		if(consume(ID)){
			return true;
			}
		}
	//iTk = start; //refacere pozitie initiala
	return false;
	}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl() {
	return false;
}

// fnDef: ( typeBase | VOID ) ID
//        LPAR ( fnParam ( COMMA fnParam )* )? RPAR
//        stmCompound
bool fnDef() {
    return false;
}

// unit: ( structDef | fnDef | varDef )* END
//true: daca regula a fost indeplinita si false: daca nu e indeplinita
bool unit(){
	for(;;){
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break;
		}
	if(consume(END)){
		return true;
		}
	return false;
	}

//// fnParam: typeBase ID arrayDecl?
bool fnParam() {
	return false;
}

// stm: stmCompound
//      | IF LPAR expr RPAR stm ( ELSE stm )?
//      | WHILE LPAR expr RPAR stm
//      | RETURN expr? SEMICOLON
//      | expr? SEMICOLON
bool stm() {
	return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound() {
	return false;
}

// expr: exprAssign
bool expr() {
	return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign() {
	return false;
}

// exprOr: exprOr OR exprAnd | exprAnd
bool exprOr() {
	// 	if(exprOr())
// 	{
// 		if(consume(OR)) 
// 		{
// 			if(exprAnd())
// 			{
// 				return true;
// 			}
// 		}
// 	}
// 	if(exprAnd())
// 	{
// 		return true;
// 	}
	return false;
}

// exprAnd: exprAnd AND exprEq | exprEq
bool exprAnd() {
	return false;
}

// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
bool exprEq() {
	return false;
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
bool exprRel() {
	return false;
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
bool exprAdd() {
	return false;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
bool exprMul() {
	return false;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast() {
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary() {
	return false;
}

// exprPostfix: exprPostfix LBRACKET expr RBRACKET
//              | exprPostfix DOT ID
//              | exprPrimary
bool exprPostfix() {
	return false;
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
//              | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary() {
	return false;
}

void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
	}