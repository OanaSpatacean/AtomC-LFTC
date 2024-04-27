#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
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
	tk->line=line; //set the line in the input file where the token was found in cases for '\r' and '\n'
	tk->next=NULL;
	if(lastTk){ //input characters are consumed until the end of that token
		lastTk->next=tk; 
		}else{
		tokens=tk;
		}
	lastTk=tk; //add a new token with the given code to the list of tokens
	return tk; //returns a pointer to the added token, allowing for the possibility of setting additional fields if needed
	}

char *extract(const char *begin,const char *end){ //the beginning of the substring to be extracted and the address of the first character after the substring
    int length = end - begin; //length of the substring that will be extracted
    char *extractedSubstring = safeAlloc(length + 1); // add 1 for NULL termination
    
    strncpy(extractedSubstring, begin, length); //copy the begin substring (original) into the extractedSubstring
    extractedSubstring[length] = '\0'; 

    return extractedSubstring;
	}

Token *tokenize(const char *pch) 
{
    const char *start;
    Token *tk;
    for (;;) 
	{
        switch (*pch) 
		{
            case ' ':
            case '\t':
                pch++;
                break;
            case '\r': // handles different kinds of newlines (Windows: \r\n, Linux: \n,
                // MacOS, OS X: \r or \n)
                if (pch[1] == '\n') 
				{
                    pch++;
                }
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
            case '=':
                if (pch[1] == '=') 
				{
                    addTk(EQUAL);
                    pch += 2;
                } 
				else 
				{
                    addTk(ASSIGN);
                    pch++;
                }
                break;
            case '+':
                addTk(ADD);
                pch++;
                break;
            case '-':
                addTk(SUB);
                pch++;
                break;
            case '*':
                addTk(MUL);
                pch++;
                break;
            case '/':
                if (pch[1] == '/') 
				{
                    pch += 2;
                    while (*pch != '\n' && *pch != '\0' && *pch != '\r') 
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
                if (pch[1] == '&') 
				{
                    addTk(AND);
                    pch += 2;
                } 
				else 
				{
                    err("invalid character");
                }
                break;
            case '|':
                if (pch[1] == '|') 
				{
                    addTk(OR);
                    pch += 2;
                } 
				else 
				{
                    err("invalid character");
                }
                break;
            case '!':
                if (pch[1] == '=') 
				{
                    addTk(NOTEQ);
                    pch += 2;
                } 
				else 
				{
                    addTk(NOT);
                    pch++;
                }
                break;
            case '<':
                if (pch[1] == '=') 
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
                if (pch[1] == '=') 
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
                if (isalpha(*pch) || *pch == '_') 
				{
                    for (start = pch; isalnum(*pch) || *pch == '_'; pch++) 
					{
                    }
                    char *text = extract(start, pch);
                    if (strcmp(text, "char") == 0) 
					{
                        addTk(TYPE_CHAR);
                    } 
					else if (strcmp(text, "int") == 0) 
					{
                        addTk(TYPE_INT);
                    } 
					else if (strcmp(text, "double") == 0) 
					{
                        addTk(TYPE_DOUBLE);
                    } 
					else if (strcmp(text, "if") == 0) 
					{
                        addTk(IF);
                    } 
					else if (strcmp(text, "else") == 0) 
					{
                        addTk(ELSE);
                    } 
					else if (strcmp(text, "return") == 0) 
					{
                        addTk(RETURN);
                    } 
					else if (strcmp(text, "struct") == 0) 
					{
                        addTk(STRUCT);
                    } 
					else if (strcmp(text, "void") == 0) 
					{
                        addTk(VOID);
                    } 
					else if (strcmp(text, "while") == 0) 
					{
                        addTk(WHILE);
                    } 
					else 
					{
                        tk = addTk(ID);
                        tk->text = text;
                    }
                } else if (isdigit(*pch)) 
				{
                    // check for int and for double
                    for (start = pch++; isdigit(*pch); pch++) 
					{
                    }
                    if (*pch == '.' || *pch == 'e' || *pch == 'E') 
					{
                        if (*pch == '.') 
						{
                            pch++;
                            for (; isdigit(*pch); pch++) 
							{
                            }
                        }
                        if (*pch == 'e' || *pch == 'E') 
						{
                            pch++;
                            if (*pch == '-' || *pch == '+') 
							{
                                pch++;
                            }
                            for (; isdigit(*pch); pch++) 
							{
                            }
                        }

                        char *text = extract(start, pch);
                        tk = addTk(DOUBLE);
                        tk->d = atof(text);
                    } 
					else 
					{
                        char *text = extract(start, pch);
                        tk = addTk(INT);
                        tk->i = atoi(text);
                    }
                } 
				else if (*pch == '\'') 
				{
                    int char_counter = 0;

                    for (start = ++pch; *pch != '\''; pch++) 
					{
                        char_counter++;
                        if (char_counter > 1) 
						{
                            err("invalid sequence. expected char between ''");
                        }
                    }

                    char text = extract(start, pch)[0];
                    tk = addTk(CHAR);
                    tk->c = text;
                    pch++;
                } 
				else if (*pch == '\"') 
				{
                    for (start = pch++; *pch != '\"'; pch++) 
					{
                    }

                    char *text = extract(start + 1, pch);
                    tk = addTk(STRING);
                    tk->text = text;
                    pch++;
                } 
				else 
				{
                    err("invalid char: %c (%d)", *pch, *pch);
                }
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
