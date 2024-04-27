#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include "parser.h"
#include "utils.h"
#include "ad.h"
#include "at.h"
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
bool expr(Ret *r);
bool exprAssign(Ret *r);
bool exprOr(Ret *r);
bool exprAnd(Ret *r);
bool exprEq(Ret *r);
bool exprRel(Ret *r);
bool exprAdd(Ret *r);
bool exprMul(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPrimary(Ret *r);

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
	Ret rCond, rExpr;

	if(stmCompound(true)) // corpul compus {...} al instructiunilor defineste un nou domeniu
	{
		return true;
	}
	if(consume(IF))
	{
		if(consume(LPAR))
		{
			if(expr(&rCond))
			{
				if (!canBeScalar(&rCond)) 
				{
                    tkerr("The if condition must be a scalar value");
                }

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
			if(expr(&rCond))
			{
				if(!canBeScalar(&rCond))
					tkerr("the while condition must be a scalar value");

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
		if (expr(&rExpr)) 
		{
			if (owner->type.tb == TB_VOID) 
			{
				tkerr("a void function cannot return a value");
			}

			if (!canBeScalar(&rExpr)) 
			{
				tkerr("the return value must be a scalar value");
			}

			if (!convTo(&rExpr.type, &owner->type)) 
			{
				tkerr("cannot convert the return expression type to the function return type");
			}
		} 
		else 
		{
			if(owner->type.tb != TB_VOID) {
				tkerr("a non-void function must return a value");
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
	if(expr(&rExpr))
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
bool expr(Ret *r) {
	if(exprAssign(r))
	{
		return true;
	}
	return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(Ret *r) { //tratarea cazului de assign =, sa fie ambele parti prezente, si dreapta, si stanga
	Token* start=iTk;
	Ret rDst;

	if(exprUnary(&rDst))
	{
		if(consume(ASSIGN))
		{
			if(exprAssign(r))
			{
				if (!rDst.lval) 
				{
					tkerr("the assign destination must be a left-value");
				}

				if (rDst.ct) 
				{
					tkerr("the assign destination cannot be constant");
				}

				if (!canBeScalar(&rDst)) 
				{
					tkerr("the assign destination must be scalar");
				}

				if (!canBeScalar(r)) 
				{
					tkerr("the assign source must be scalar");
				}

				if (!convTo(&r->type,&rDst.type)) 
				{
					tkerr("the assign source cannot be converted to destination");
				}

				r->lval = false;
				r->ct = true;

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

	if(exprOr(r))
	{
		return true;
	}

	iTk=start;
	return false;
}

bool exprOr1(Ret *r){
	Token* start=iTk;
	Ret right;

	if(consume(OR))
	{
		if(exprAnd(&right))
		{
			Type tDst;

			if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for ||");
			}

			*r = (Ret){{TB_INT,NULL,-1},false,true};

			if(exprOr1(r))
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
bool exprOr(Ret *r) { //a || b || c
	Token* start=iTk;

	if(exprAnd(r))
	{
		if(exprOr1(r))
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

bool exprAnd1(Ret *r)
{
	Token* start=iTk;
	Ret right;

	if(consume(AND))
	{
		if(exprEq(&right))
		{
			Type tDst;

			if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for &&"); 
			}

			*r = (Ret){{TB_INT,NULL,-1},false,true};

			if(exprAnd1(r))
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
bool exprAnd(Ret *r) { //a && b && c
	Token* start=iTk;

	if(exprEq(r))
	{
		if(exprAnd1(r))
		{
			return true;
		}
	}
	iTk=start;
	return false;
}

bool exprEq1(Ret *r){
	Token* start=iTk;

	if(consume(EQUAL) || consume(NOTEQ))
	{
		Ret right;

		if(exprRel(&right))
		{
			Type tDst;

            if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for == or !="); 
			}
			*r = (Ret){{TB_INT,NULL,-1},false,true};

			if(exprEq1(r))
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
bool exprEq(Ret *r) { //a = b != c
	Token* start=iTk;

	if(exprRel(r))
	{
		if(exprEq1(r))
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

bool exprRel1(Ret *r){
	Token* start=iTk;

	if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ))
	{
		Ret right;

		if(exprAdd(&right))
		{
			Type tDst;

			if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for <, <=, >, >="); 
			}

			*r = (Ret){{TB_INT,NULL,-1},false,true};

			if(exprRel1(r))
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
bool exprRel(Ret *r) { //a < b >= c
	Token* start=iTk;

	if(exprAdd(r))
	{
		if(exprRel1(r))
		{
			return true;
		}
	}
	iTk=start;
	return false;
}

bool exprAdd1(Ret *r){
	Token* start=iTk;

	if(consume(SUB) || consume(ADD))
	{
		Ret right;

		if(exprMul(&right))
		{
			Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for + or -"); 
			}
			*r = (Ret){tDst,false,true};

			if(exprAdd1(r))
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
bool exprAdd(Ret *r) { //a + b - c
	Token* start=iTk;

	if(exprMul(r))
	{
		if(exprAdd1(r))
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

int exprMul1(Ret *r){
	Token* start=iTk;

	if(consume(MUL) || consume(DIV))
	{
		Ret right;

		if(exprCast(&right))
		{
			Type tDst;

            if (!arithTypeTo(&r->type, &right.type, &tDst)) 
			{
				tkerr("invalid operand type for * or /"); 
			}

			*r = (Ret){tDst,false,true};

			if(exprMul1(r))
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
bool exprMul(Ret *r) { //a * b / c
	Token* start=iTk;

	if(exprCast(r))
	{
		if(exprMul1(r))
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(Ret *r) { //(int)a

	Token* start=iTk;

	if(consume(LPAR))
	{
		Type t;
        Ret op;

		if(typeName())
		{
			if(consume(RPAR))
			{
				if(exprCast(&op))
				{	
					if (t.tb == TB_STRUCT) 
					{
						tkerr("cannot convert to a struct type"); 
					}

					if (op.type.tb == TB_STRUCT) 
					{
						tkerr("cannot convert a struct");
					} 

					if (op.type.n >= 0 && t.n < 0) 
					{
						tkerr("an array can be converted only to another array"); 
					}

					if (op.type.n < 0 && t.n >= 0) 
					{
						tkerr("a scalar can be converted only to another scalar"); 
					}
					*r = (Ret){t,false,true};

					return true;
				}
			}
		}
		iTk=start;
	}
	if(exprUnary(r))
	{
		return true;
	}

	iTk=start;
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary(Ret *r) { //--a
	Token* start=iTk;

	if(consume(SUB) || consume(NOT))
	{
		if(exprUnary(r))
		{
			if (!canBeScalar(r)) 
			{
				tkerr("unary - or ! must have a scalar operand");
			}
			r->lval = false;
			r->ct = true;

			return true;
		}
		iTk=start;
	}
	if(exprPostfix(r))
	{
		return true;
	}

	iTk=start;
	return false;
}

int exprPostfix1(Ret *r){
	Token* start=iTk;

	if(consume(LBRACKET))
	{
		Ret idx;

		if(expr(&idx))
		{
			if(consume(RBRACKET))
			{
				if (r->type.n < 0) 
				{
					tkerr("only an array can be indexed");
				}

				Type tInt = {TB_INT,NULL,-1};

                if (!convTo(&idx.type, &tInt)) 
				{
					tkerr("the index is not convertible to int");
				}

				r->type.n = -1;
                r->lval = true;
                r->ct = false;

				if(exprPostfix1(r))
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
			Token *tkName = consumedTk;

			 if (r->type.tb != TB_STRUCT) 
			 {
				tkerr("a field can only be selected from a struct");
			}

			Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);

            if (!s) 
			{
				tkerr("the structure %s does not have a field %s",r->type.s->name,tkName->text);
			}

			*r = (Ret){s->type,true,s->type.n>=0};

			if(exprPostfix1(r))
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
bool exprPostfix(Ret *r) { //a[0].b
	Token* start=iTk;

	if(exprPrimary(r))
	{
		if(exprPostfix1(r))
		{
			return true;
		}
	}

	iTk=start;
	return false;
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
//              | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(Ret *r) { //myFunction(1, "hello", 3.14)
	Token* start=iTk;

	if(consume(ID))
	{
		Token *tkName = consumedTk;
		Symbol *s = findSymbol(tkName->text);

		if (!s) 
		{
			tkerr("undefined id: %s", tkName->text);
		}

		if(consume(LPAR))
		{
			if(s->kind!=SK_FN)
				tkerr("only a function can be called");

			Ret rArg;
			Symbol *param=s->fn.params;

			if(expr(&rArg))
			{
				if (!param) 
				{
					tkerr("too many arguments in function call");
				}

				if (!convTo(&rArg.type, &param->type)) 
				{
					tkerr("in call, cannot convert the argument type to the parameter type");
				}

				param=param->next;

				while(1)
				{
					if(consume(COMMA))
					{
						if(expr(&rArg))
						{
							if (!param) 
							{
								tkerr("too many arguments in function call");
							}

							if (!convTo(&rArg.type, &param->type)) 
							{
								tkerr("in call, cannot convert the argument type to the parameter type");
							}
							
							param=param->next;
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
				if (param) 
				{
					tkerr("too few arguments in function call");
				}

				*r = (Ret){s->type,false,true};			
			}
			else
			{
				if (s->kind == SK_FN) 
				{
					tkerr("a function can only be called");
				}

				*r = (Ret){s->type,true,s->type.n>=0};
			}
		}
		else 
		{
			if(s->kind==SK_FN) 
			{
				tkerr("a function can only be called"); 
			}
			*r = (Ret){s->type,true,s->type.n>=0};
		}

		return true;
	}
	if(consume(INT))
	{
		*r = (Ret){{TB_INT,NULL,-1},false,true};
		return true;
	}
	if(consume(DOUBLE))
	{
		*r = (Ret){{TB_DOUBLE,NULL,-1},false,true};
		return true;
	}
	if(consume(CHAR))
	{
		*r = (Ret){{TB_CHAR,NULL,-1},false,true};
		return true;
	}
	if(consume(STRING))
	{
		*r = (Ret){{TB_CHAR,NULL,0},false,true};
		return true;
	}
	if(consume(LPAR))
	{
		if(expr(r))
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