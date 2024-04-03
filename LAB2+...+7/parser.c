#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include "parser.h"

bool unit();
bool structDef();
bool varDef();
bool typeBase();
bool arrayDecl();
bool typeName();
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound();
bool expr();
bool exprAssign();
bool exprOr();
bool exprAnd();
bool exprEq();
bool exprRel();
bool exprAdd();
bool exprMul();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPrimary();

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
	else
	{
		printf("current token: %d\n",iTk->code);
		tkerr("syntax error");
	}
	return false;
}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef() { //definitia unei structuri 
	Token* start=iTk;

	if(consume(STRUCT))
	{
		if(consume(ID))
		{
			if(consume(LACC))
			{
				while(1)
				{
					if(varDef())
					{

					}
					else 
					{
						break;
					}
				}
				if(consume(RACC))
				{
					if(consume(SEMICOLON))
					{
						return true;
					}
					else 
					{
						tkerr("Missing \';\'\n"); 
					}
				}
				else 
				{
					tkerr("Missing \'}\'\n");
				}
			}
			iTk=start;
		}
		else 
		{
			tkerr("Missing identifier\n");
		}
	}
	iTk=start;
	return false;
}

// varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef() { //definitia unei variabile
	Token *start=iTk;

	if(typeBase())
	{
		if(consume(ID))
		{
			if(arrayDecl())
			{

			}
			while(1)
			{
				if(consume(COMMA))
				{
					if(consume(ID))
					{
						if(arrayDecl())
						{

						}
					}
					else 
					{
						tkerr("Missing identifier\n");
					}
				}
				else 
				{
					break;
				}
			}
			if(consume(SEMICOLON))
			{
				return true;
			}
			else 
			{
				tkerr("Missing \';\'\n");
			}
		}
		else 
		{
			tkerr("Missing identifier\n");
		}
	}
	iTk=start;
	return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
//totul sau nimic!!
bool typeBase(){ //recunoastere type-urilor folosite, inclusiv cel de struct
	Token *start=iTk;
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
		else
		{
			tkerr("Missing name of struct");
		}
	}
	iTk=start; //refacere pozitie initiala
	return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl() { //declararea unui array
	Token *start=iTk;

	if(consume(LBRACKET))
	{
		if(expr())
		{

		}
		if(consume(RBRACKET))
		{
			return true;
		}
		else 
		{
			tkerr("Missing \']\'\n");
		}
	}
	iTk=start;
	return false;
}

bool typeName(){
	if(typeBase())
	{
		if(arrayDecl())
		{
		
		}
		return true;
	}
	return false;
}

// fnDef: ( typeBase | VOID ) ID
//        LPAR ( fnParam ( COMMA fnParam )* )? RPAR
//        stmCompound
bool fnDef() { //definirea unei functii (...)
	Token* start=iTk;

	if(typeBase())
	{
		if(consume(MUL))
		{

		}
		if(consume(ID))
		{
			if(consume(LPAR))
			{
				if(fnParam())
				{
					while(1)
					{
						if(consume(COMMA))
						{
							if(fnParam())
							{

							}
							else 
							{
								tkerr("Missing argument after \',\'\n");
							}
						}
						else 
						{
							break;	
						}
					}
				}
				if(consume(RPAR))
				{
					if(stmCompound())
					{
						return true;
					}
					else 
					{
						tkerr("Missing body of function\n");
					}
				}
				else 
				{
					tkerr("Missing \')\'\n");
				}
			
			}
			else 
			{
				iTk=start;
			}
		}
		else tkerr("Missing function name\n");
	}
	else if(consume(VOID))
	{
		if(consume(ID))
		{
			if(consume(LPAR))
			{
				if(fnParam())
				{
					while(1)
					{
						if(consume(COMMA))
						{
							if(fnParam())
							{

							}
							else 
							{
								tkerr("Missing argument after \',\'\n");
							}
						}
						else 
						{
							break;	
						}
					}
				}
				if(consume(RPAR))
				{
					if(stmCompound())
					{
						return true;
					}
					else 
					{
						tkerr("Missing body of function\n");
					}
				}
				else 
				{
					tkerr("Missing \')\'\n");
				}
			}
			else 
			{
				tkerr("Missing \'(\'\n");
			}
		}
		else 
		{
			tkerr("Missing function name\n");
		}
	}
	iTk=start;
	return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam() { //parametrii unei functii, intre ()
	if(typeBase())
	{
		if(consume(ID))
		{
			if(arrayDecl())
			{

			}
			return true;
		}
		else 
		{
			tkerr("Missing variable name\n");
		}
	}
	return false;
}

// stm: stmCompound
//      | IF LPAR expr RPAR stm ( ELSE stm )?
//      | WHILE LPAR expr RPAR stm
//      | RETURN expr? SEMICOLON
//      | expr? SEMICOLON
bool stm() { //definirea conditiilor if, else, while, return
	if(stmCompound())
	{
		return true;
	}
	if(consume(IF))
	{
		if(consume(LPAR))
		{
			if(expr())
			{
				if(consume(RPAR))
				{
					if(stm())
					{
						if(consume(ELSE))
						{
							if(stm()){}
							else tkerr("Missing \'else\'\n");
						}
						return true;
					}
					else 
					{
						tkerr("Missing expression\n");
					}
				}
				else 
				{
					tkerr("Missing \')\'\n");
				}
			}
			else 
			{
				tkerr("Missing \'if\' condition\n");
			}
		}
		else 
		{
			tkerr("Missing \'(\'\n");
		}
	}
	if(consume(WHILE))
	{
		if(consume(LPAR))
		{
			if(expr()){
				if(consume(RPAR))
				{
					if(stm())
					{
						return true;
					}
					else 
					{
						tkerr("Missing \'while\' body\n");
					}
				}
				else 
				{
					tkerr("Missing \')\'\n");
				}
			}
			else 
			{
				tkerr("Missing \'while\' condition\n");
			}
		}
		else 
		{
			tkerr("Missing \'(\'\n");
		}
	}
	if(consume(RETURN))
	{
		if(expr())
		{

		}
		if(consume(SEMICOLON))
		{
			return true;
		}
		else 
		{
			tkerr("Missing \';\'\n");
		}
	}
	if(expr())
	{
		if(consume(SEMICOLON))
		{
			return true;
		}
		else 
		{
			tkerr("Missing \';\'\n");
		}
	}
	if(consume(SEMICOLON))
	{
		return true;
	}
	return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound() { //interiorul unei functii intre {} unde se pot declara variabile si utiliza instructiuni precum if, else, while, return
	if(consume(LACC))
	{
		while(1)
		{
			if(varDef())
			{
			}
			else if(stm())
			{
			}
			else 
			{
				break;
			}
		}
		if(consume(RACC))
		{
			return true;
		}
		else 
		{
			tkerr("Missing \'}\'\n");
		}
	}	
	return false;
}

// expr: exprAssign
bool expr() {
	if(exprAssign())
	{
		return true;
	}
	return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign() { //tratarea cazului de assign =, sa fie ambele parti prezente, si dreapta, si stanga
	Token* start=iTk;

	if(exprUnary())
	{
		if(consume(ASSIGN))
		{
			if(exprAssign())
			{
				return true;
			}
			else
			{
				tkerr("Missing right part of \'=\'\n");
			}

		}
		iTk=start;
	}

	iTk=start;

	if(exprOr())
	{
		return true;
	}

	iTk=start;
	return false;
}

bool exprOr1(){
	Token* start=iTk;

	if(consume(OR))
	{
		if(exprAnd())
		{
			if(exprOr1())
			{
				return true;
			}
		}
	}
	iTk=start;
	return true;
}

// exprOr: exprOr OR exprAnd | exprAnd
// =>
//exprOr: exprAnd exprOr1
//exprOr1: OR exprAnd exprOr1|epsilon
bool exprOr() { //a || b || c
	Token* start=iTk;

	if(exprAnd())
	{
		if(exprOr1())
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

bool exprAnd1()
{
	Token* start=iTk;

	if(consume(AND))
	{
		if(exprEq())
		{
			if(exprAnd1())
			{
				return true;
			}
		}
	}
	iTk=start;
	return true;
}

// exprAnd: exprAnd AND exprEq | exprEq
// =>
//exprAnd: exprEq exprAnd1
//exprAnd1: AND exprEq exprAnd1 | epsilon
bool exprAnd() { //a && b && c
	Token* start=iTk;

	if(exprEq())
	{
		if(exprAnd1())
		{
			return true;
		}
	}
	iTk=start;
	return false;
}

bool exprEq1(){
	Token* start=iTk;

	if(consume(EQUAL) || consume(NOTEQ))
	{
		if(exprRel())
		{
			if(exprEq1())
			{
				return true;
			}
		}
	}
	
	iTk=start;
	return true;
}

// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
// =>
//exprEq: exprRel exprEq1
//exprEq1: (EQUAL|NOTEQ) exprRel exprEq1 | epsilon
bool exprEq() { //a = b != c
	Token* start=iTk;

	if(exprRel())
	{
		if(exprEq1())
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

bool exprRel1(){
	Token* start=iTk;

	if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ))
	{
		if(exprAdd())
		{
			if(exprRel1())
			{
				return true;
			}
		}
	}

	iTk=start;
	return true;
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
// =>
//exprRel: exprAdd exprRel1
//exprRel1: (LESS|LESSEQ|GREATER|GREATEREQ) exprAdd exprRel1 | epsilon
bool exprRel() { //a < b >= c
	Token* start=iTk;

	if(exprAdd())
	{
		if(exprRel1())
		{
			return true;
		}
	}
	iTk=start;
	return false;
}

bool exprAdd1(){
	Token* start=iTk;

	if(consume(SUB) || consume(ADD))
	{
		if(exprMul())
		{
			if(exprAdd1())
			{
				return true;
			}
		}
	}

	iTk=start;
	return true;
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
// =>
//exprAdd: exprMul exprAdd1
//exprAdd1: (SUB|MUL) exprMul exprAdd1 | epsilon
bool exprAdd() { //a + b - c
	Token* start=iTk;

	if(exprMul())
	{
		if(exprAdd1())
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

int exprMul1(){
	Token* start=iTk;

	if(consume(MUL) || consume(DIV))
	{
		if(exprCast())
		{
			if(exprMul1())
			{
				return true;
			}
		}
	}

	iTk=start;
	return true;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
// =>
//exprMul: exprCast exprMul1
//exprMul1: (MUL|DIV) exprCast exprMul1 | epsilon
bool exprMul() { //a * b / c
	Token* start=iTk;

	if(exprCast())
	{
		if(exprMul1())
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast() { //(int)a

	Token* start=iTk;

	if(consume(LPAR))
	{
		if(typeName())
		{
			if(consume(RPAR))
			{
				if(exprCast())
				{
					return true;
				}
			}
		}
		iTk=start;
	}
	if(exprUnary())
	{
		return true;
	}

	iTk=start;
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary() { //--a
	Token* start=iTk;

	if(consume(SUB) || consume(NOT))
	{
		if(exprUnary())
		{
			return true;
		}
		iTk=start;
	}
	if(exprPostfix())
	{
		return true;
	}

	iTk=start;
	return false;
}

int exprPostfix1(){
	Token* start=iTk;

	if(consume(LBRACKET))
	{
		if(expr())
		{
			if(consume(RBRACKET))
			{
				if(exprPostfix1())
				{
					return true;
				}
			}
		}
		iTk=start;
	}
	if(consume(DOT))
	{
		if(consume(ID))
		{
			if(exprPostfix1())
			{
				return true;
			}
		}
		iTk=start;
	}
	return true;
}

// exprPostfix: exprPostfix LBRACKET expr RBRACKET
//              | exprPostfix DOT ID
//              | exprPrimary
bool exprPostfix() { //a[0].b
	Token* start=iTk;

	if(exprPrimary())
	{
		if(exprPostfix1())
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
//              | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary() { //myFunction(1, "hello", 3.14)

	Token* start=iTk;

	if(consume(ID))
	{
		if(consume(LPAR))
		{
			if(expr())
			{
				while(1)
				{
					if(consume(COMMA))
					{
						if(expr())
						{

						}
						else 
						{
							tkerr("Missing expression after \',\'\n");
						}
					}
					else 
					{
						break;
					}
				}
			}
			if(consume(RPAR))
			{
				return true;
			}
		}
		return true;
	}
	if(consume(INT))
	{
		return true;
	}
	if(consume(DOUBLE))
	{
		return true;
	}
	if(consume(CHAR))
	{
		return true;
	}
	if(consume(STRING))
	{
		return true;
	}
	if(consume(LPAR))
	{
		if(expr())
		{
			if(consume(RPAR))
			{
				return true;
			}
		}
	}
	
	iTk=start;
	return false;
}

void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
	}