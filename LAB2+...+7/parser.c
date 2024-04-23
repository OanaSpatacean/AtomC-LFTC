#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include "parser.h"
#include "utils.h"
#include "ad.h"
#include <string.h>

bool unit();
bool structDef();
bool varDef();
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool typeName();
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
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
Symbol *owner;

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
			Token *tkName = consumedTk; //dacă atomul ID a fost consumat, name va conține tot acel atom

			if(consume(LACC))
			{
				Symbol *s=findSymbolInDomain(symTable,tkName->text);

				if(s)
					tkerr("symbol redefinition: %s",tkName->text); //numele structurii trebuie sa fie unic in domeniu

				s=addSymbolToDomain(symTable,newSymbol(tkName->text,SK_STRUCT));
				s->type.tb=TB_STRUCT;
				s->type.s=s;
				s->type.n=-1; //in interiorul structurii nu pot exista doua variabile cu acelasi nume

				pushDomain(); //crează și adaugă un domeniu în vârful stivei de domenii
				owner=s;

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
						owner=NULL;
						dropDomain(); //șterge domeniul din vârful stivei de domenii

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
	Type t;

	if(typeBase(&t))
	{
		if(consume(ID))
		{
			Token *tkName = consumedTk;

			if(arrayDecl(&t))
			{
				if (t.n == 0) //dimension of array
				{
                    tkerr("A vector variable must have a specified dimension");
                }
			}
			while(1)
			{
				if(consume(COMMA))
				{
					if(consume(ID))
					{
						if(arrayDecl(&t))
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
				Symbol *var=findSymbolInDomain(symTable,tkName->text); //caută un simbol cu numele tkName->text în domeniul symTable

				if(var)
					tkerr("symbol redefinition: %s",tkName->text); //numele variabilei trebuie sa fie unic in domeniu

				var=newSymbol(tkName->text,SK_VAR);
				var->type=t;
				var->owner=owner;
				addSymbolToDomain(symTable,var); //adaugă simbolul var în domeniul symTable

				if(owner)
				{
					switch(owner->kind)
					{
						case SK_FN:
							var->varIdx=symbolsLen(owner->fn.locals); //variabilele de tip vector trebuie sa aiba dimensiunea data (nu se accepta: int v[])
							addSymbolToList(&owner->fn.locals,dupSymbol(var));
							break;
						case SK_STRUCT:
							var->varIdx=typeSize(&owner->type);
							addSymbolToList(&owner->structMembers,dupSymbol(var));
							break;
					}
				}
				else
				{
					var->varMem=safeAlloc(typeSize(&t));
				}

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
bool typeBase(Type *t){ //recunoastere type-urilor folosite, inclusiv cel de struct
	Token *start=iTk;
	t->n=-1;
	if(consume(TYPE_INT)){
		t->tb=TB_INT;
		return true;
		}
	if(consume(TYPE_DOUBLE)){
		t->tb = TB_DOUBLE;
		return true;
		}
	if(consume(TYPE_CHAR)){
		t->tb = TB_CHAR;
		return true;
		}
	if(consume(STRUCT)){
		if(consume(ID)){
				Token *tkName = consumedTk;

				t->tb = TB_STRUCT;
				t->s = findSymbol(tkName->text); //daca tipul de baza este o structura, ea trebuie sa fie deja definita anterior
				if (!t->s) {
					tkerr("Structure is not defined: %s", tkName->text);
				}
				
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
bool arrayDecl(Type *t) { //declararea unui array
	Token *start=iTk;

	if(consume(LBRACKET))
	{
		if (consume(INT)) 
		{
            Token *tkSize = consumedTk;
            t->n = tkSize->i;
        } 
		else 
		{
            t->n = 0;
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
	Type t;
	if(typeBase(&t))
	{
		if(arrayDecl(&t))
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
	Type t;

	if(typeBase(&t))
	{
		if(consume(MUL))
		{

		}
		if(consume(ID))
		{
			Token *tkName = consumedTk;

			if(consume(LPAR))
			{
				Symbol *fn=findSymbolInDomain(symTable,tkName->text);

				if(fn)
					tkerr("symbol redefinition: %s",tkName->text); //numele functiei trebuie sa fie unic in domeniu
					
				fn=newSymbol(tkName->text,SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable,fn);
				owner=fn;
				pushDomain(); //domeniul local functiei incepe imediat dupa LPAR

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
					addInstr(&fn->fn.instr, OP_ENTER);

					if (stmCompound(false)) 
					{
                        dropDomain(); // corpul functiei {...} nu defineste un nou subdomeniu in domeniul local functiei
                        owner = NULL;

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
		t.tb = TB_VOID;

		if(consume(ID))
		{
			Token *tkName = consumedTk;

			if(consume(LPAR))
			{
				Symbol *fn = findSymbolInDomain(symTable, tkName->text);

                if (fn) 
				{
                    tkerr("Symbol is already defined: %s", tkName->text);
                }

                fn = newSymbol(tkName->text, SK_FN);
                fn->type = t;
                addSymbolToDomain(symTable, fn);
                owner = fn;
                pushDomain();

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
					if(stmCompound(false))
					{
						dropDomain(); 
                        owner = NULL;
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
	Type t;
	if(typeBase(&t))
	{
		if(consume(ID))
		{
			Token *tkName = consumedTk;

			if(arrayDecl(&t))
			{
				t.n = 0;
			}

			Symbol *param = findSymbolInDomain(symTable, tkName->text);

            if (param) 
			{
                tkerr("Symbol is already defined: %s", tkName->text); //numele parametrului trebuie sa fie unic in domeniu
            }

            param = newSymbol(tkName->text, SK_PARAM);
            param->type = t;
            param->owner = owner;
            param->paramIdx = symbolsLen(owner->fn.params); //parametrii pot fi vectori cu dimensiune data, dar in acest caz li se sterge dimensiunea ( int v[10] -> int v[] )
            addSymbolToDomain(symTable, param); //parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
            addSymbolToList(&owner->fn.params, dupSymbol(param));

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
	if(stmCompound(true)) // corpul compus {...} al instructiunilor defineste un nou domeniu
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
bool stmCompound(bool newDomain) { //interiorul unei functii intre {} unde se pot declara variabile si utiliza instructiuni precum if, else, while, return
	if(consume(LACC))
	{
		if(newDomain) //se defineste un nou domeniu doar la cerere
			pushDomain();

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
			if (newDomain) 
			{
                dropDomain();
            }
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