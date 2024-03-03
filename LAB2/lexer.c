#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){
	Token *tk=safeAlloc(sizeof(Token));
	tk->code=code;
	tk->line=line;
	tk->next=NULL;
	if(lastTk){
		lastTk->next=tk;
		}else{
		tokens=tk;
		}
	lastTk=tk;
	return tk;
	}

char *extract(const char *begin,const char *end){
    int length = end - begin; //length of the substring that will be extracted
    char *extractedSubstring = safeAlloc(length + 1); // add 1 for NULL termination
    
    strncpy(extractedSubstring, begin, length); //copy the begin substring (original) into the extractedSubstring
    extractedSubstring[length] = '\0'; 

    return extractedSubstring;
	}

Token *tokenize(const char *pch){
	const char *start;
	Token *tk;
	for(;;){
		switch(*pch){
			case ' ':
			case '\t':
				pch++;
				break;
			case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
				if(pch[1]=='\n')
					pch++;
				// fallthrough to \n
			case '\n':
				line++;
				pch++;
				break;
			case '\0':
				addTk(END);
				return tokens;
			case ',':
				addTk(COMMA);
				pch++;
				break;
			case '=':
				if(pch[1]=='=')
				{
					addTk(EQUAL);
					pch+=2;
				}
				else
				{
					addTk(ASSIGN);
					pch++;
				}
				break; //already in the given code
			//here starts my code
			case ';':
				addTk(SEMICOLON);
				pch++;
				break;
			case '(': 
				addTk(LPAR);
				pch++;
				break;
			case ')':
				addTk(RPAR);
				pch++;
				break;
			case '[':
				addTk(LBRACKET);
				pch++;
				break;
			case ']':
				addTk(RBRACKET);
				pch++;
				break;
			case '{':
				addTk(LACC);
				pch++;
				break;
			case '}':
				addTk(RACC);
				pch++;
				break;
			case '+':
				addTk(ADD);
				pch++;
				break;
			case '*':
				addTk(MUL);
				pch++;
				break;
			case '/':
				if (pch[1] == '/')
				{
					while (*pch != '\0' && *pch != '\n')
					{
						pch++;
					}
				}
				else
				{
					addTk(DIV);
					pch++;
				}
				break;
			case '.':
				addTk(DOT); 
				pch++; 
				break;
			case '&':
				if(pch[1] == '&')
				{
					addTk(AND);
					pch += 2;
				}
				else
				{
					err("invalid char: %c (%d)", *pch, *pch);
				}
				break;
			case '|':
				if(pch[1] == '|')
				{
					addTk(OR);
					pch += 2;
				}
				else
				{
					err("invalid char: %c (%d)", *pch, *pch);
				}
				break;
			case '!':
				if(pch[1]=='=')
				{
					addTk(NOTEQ);
					pch+=2;
				}
				else
				{
					addTk(NOT);
					pch++;
				}
				break;
			case '<':
				if(pch[1] == '=')
				{
					addTk(LESSEQ);
					pch += 2;
				}
				else
				{
					addTk(LESS);
					pch++;
				}
				break;
			case '>':
				if(pch[1] == '=')
				{
					addTk(GREATEREQ);
					pch += 2;
				}
				else
				{
					addTk(GREATER);
					pch++;
				}
				break;
			default:
				if(isalpha(*pch)||*pch=='_')
				{
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++) //check if the current character is an alphabetic character or an underscore
					{} //find the complete word

					char *text=extract(start,pch); //extract the word resulted from the loop

					if(strcmp(text,"char")==0) //add the corresponding tokens
						addTk(TYPE_CHAR);//already in the given code
					//here starts my code
					else if(strcmp(text,"double")==0) 
						addTk(TYPE_DOUBLE);
					else if(strcmp(text,"else")==0) 
						addTk(ELSE);
					else if(strcmp(text,"if")==0) 
						addTk(IF);
					else if(strcmp(text,"int")==0) 
						addTk(TYPE_INT);
					else if(strcmp(text,"return")==0) 
						addTk(RETURN);
					else if(strcmp(text,"struct")==0) 
						addTk(STRUCT);
					else if(strcmp(text,"void")==0) 
						addTk(VOID);
					else if(strcmp(text,"while")==0) 
						addTk(WHILE);
					else //already in the given code
					{
						tk=addTk(ID); //if no token matches, add an ID 
						tk->text=text; //store the text in the token structure
					}
				}
				//here starts my code
				else if (isalpha(*pch) || *pch == '"' || *pch == '\'') //check if the current character is an alphabetic character, double quote, or single quote
				{
					int SingleQuoteFlag = 0; //process characters or strings

					pch++;

					for (start = pch++; isalnum(*pch) || *pch == '"' || *pch == '\''; pch++)
					{
						if (*pch == '\'')
						{
							SingleQuoteFlag = 1;  //differentiate between character literals and string literals
						}
					}

					if (SingleQuoteFlag == 1)
					{
						char *character = extract(start, pch - 1); //extract the content between the quotes or apostrophes 
						tk = addTk(CHAR); //add a token accordingly CHAR/STRING
						tk->c = *character;
					}
					else
					{
						char *text = extract(start, pch - 1);
						tk = addTk(STRING);
						tk->text = text;
					}
				}
				else if (isdigit(*pch)) //check if the current character is a digit
				{
					int DotFlag = 0; //process digits
					int ENotationFlag = 0;
					char *EndPointer;

					for (start = pch++; isalnum(*pch) || *pch == '_' || *pch == '.' || *pch == 'e' || *pch == '-' || *pch == '+'; pch++)
					{ //check for the presence of a dot or the letter 'e'/'E' to determine if it's a floating-point number
						if (*pch == '.')
						{
							DotFlag = 1;
						}
						if (*pch == 'e' || *pch == 'E')
						{
							ENotationFlag = 1; //scientific notation
						}
					}

					char *text = extract(start, pch); ////extract the number resulted from the loop

					if (DotFlag == 1 || ENotationFlag == 1)
					{
						tk = addTk(DOUBLE); 
						tk->d = strtod(text, &EndPointer); //convert the extracted numeric text into a double using strtod
					}
					else
					{
						tk = addTk(INT);
						tk->i = atoi(text); //convert the extracted numeric text into an integer using atoi
					}
				}
			else //already in the given code
				err("invalid char: %c (%d)",*pch,*pch);
		}
	}
}

void showTokens(const Token *tokens){
	FILE *fout = fopen("resultAlex.txt", "w"); 
    
	if(fout == NULL) 
	{
        printf("Error when opening the output file");
        exit(-1);
    }

    for(const Token *tk = tokens; tk; tk = tk->next) 
	{
        fprintf(fout, "%d\t", tk->line); //line number in the output file

        switch(tk->code) 
		{
            case ID: 
				fprintf(fout, "ID:%s\n", tk->text); 
				break;
            case TYPE_CHAR: 
				fprintf(fout, "TYPE_CHAR\n"); 
				break;
			case TYPE_DOUBLE: 
				fprintf(fout, "TYPE_DOUBLE\n"); 
				break;
			case ELSE: 
				fprintf(fout, "ELSE\n"); 
				break;
			case IF: 
				fprintf(fout, "IF\n"); 
				break;
			case TYPE_INT: 
				fprintf(fout, "TYPE_INT\n"); 
				break;
			case RETURN: 
				fprintf(fout, "RETURN\n"); 
				break;
			case STRUCT: 
				fprintf(fout, "STRUCT\n"); 
				break;
			case VOID: 
				fprintf(fout, "VOID\n"); 
				break;
			case WHILE: 
				fprintf(fout, "WHILE\n"); 
				break;
			case INT: 
				fprintf(fout, "INT:%d\n", tk->i); 
				break;
			case DOUBLE: 
				fprintf(fout, "DOUBLE:%g\n", tk->d); 
				break;
			case CHAR: 
				fprintf(fout, "CHAR:%c\n", tk->c); 
				break;
			case STRING: 
				fprintf(fout, "STRING:%s\n", tk->text); 
				break;
            case COMMA: 
				fprintf(fout, "COMMA\n"); 
				break;
			case SEMICOLON: 
				fprintf(fout, "SEMICOLON\n"); 
				break;
			case LPAR: 
				fprintf(fout, "LPAR\n"); 
				break;
            case RPAR: 
				fprintf(fout, "RPAR\n"); 
				break;
            case LBRACKET: 
				fprintf(fout, "LBRACKET\n"); 
				break;
            case RBRACKET: 
				fprintf(fout, "RBRACKET\n"); 
				break;
            case LACC: 
				fprintf(fout, "LACC\n"); 
				break;
            case RACC: 
				fprintf(fout, "RACC\n"); 
				break;
            case END: 
				fprintf(fout, "END\n"); 
				break;
			case ADD: 
				fprintf(fout, "ADD\n"); 
				break;
            case SUB: 
				fprintf(fout, "SUB\n"); 
				break;
            case MUL: 
				fprintf(fout, "MUL\n"); 
				break;
            case DIV: 
				fprintf(fout, "DIV\n"); 
				break;
            case DOT: 
				fprintf(fout, "DOT\n"); 
				break;
            case AND: 
				fprintf(fout, "AND\n"); 
				break;
            case OR: 
				fprintf(fout, "OR\n"); 
				break;
            case NOT: 
				fprintf(fout, "NOT\n"); 
				break;
			case ASSIGN: 
				fprintf(fout, "ASSIGN\n"); 
				break;
            case EQUAL: 
				fprintf(fout, "EQUAL\n"); 
				break;
			case NOTEQ: 
				fprintf(fout, "NOTEQ\n"); 
				break;
            case LESS: 
				fprintf(fout, "LESS\n"); 
				break;
			case LESSEQ: 
				fprintf(fout, "LESSEQ\n"); 
				break;
            case GREATER: 
				fprintf(fout, "GREATER\n"); 
				break;         
            case GREATEREQ: 
				fprintf(fout, "GREATEREQ\n"); 
				break;    
            default: 
				fprintf(fout, "Token unknown\n");
				break;
        }
    }
    fclose(fout); 
}
