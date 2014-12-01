%{
#include <stdio.h>
#include <string.h>
#include "smallC.tab.h"
char *leafLable; //store the lexeme of ID/INT. Since it's extern declared in yacc file, we can use it creating the leaf node.
char *tp; //store the lexeme of other tokens. I used to think that it could be used in yacc file as well, but not so.
int state;  //judge UMINUS and MINUS
int overFlag = 0;
%}

digit [0-9]
digitNz [1-9]
octDigit [0-7]
octDigitNz [1-7]
hexDigit [0-9a-eA-E]
hexDigitNz [1-9a-eA-E]
character [a-zA-Z]
charNumUs [a-zA-Z0-9_]
charUs [a-zA-Z_]
%%

[ \t]+ {}
\n {printf("\n");}

int    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"TYPE: ");strcat(tp,yytext); printf("%s    ",tp); return TYPE;}
struct    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"STRUCT: ");strcat(tp,yytext); printf("%s    ",tp); return STRUCT;}
return    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"RETURN: ");strcat(tp,yytext); printf("%s    ",tp); return RETURN;}
if    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"IF: ");strcat(tp,yytext); printf("%s    ",tp); return IF;}
else    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"ELSE: ");strcat(tp,yytext); printf("%s    ",tp); return ELSE;}
break    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"BREAK: ");strcat(tp,yytext); printf("%s    ",tp); return BREAK;}
continue    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"CONT: ");strcat(tp,yytext); printf("%s    ",tp); return CONT;}
for    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"FOR: ");strcat(tp,yytext); printf("%s    ",tp); return FOR;}
write    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"WRITE: ");strcat(tp,yytext); printf("%s    ",tp); return WRITE;}
read    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"READ: ");strcat(tp,yytext); printf("%s    ",tp); return READ;}

0x0|00|0    {state = 1; strcpy(leafLable,"INT: "); strcat(leafLable,yytext); printf("%s    ",leafLable); return INT;}
{digitNz}{digit}*    {state = 1; strcpy(leafLable,"INT: ");    strcat(leafLable,yytext);    if (yyleng<10) {}    else if (yyleng==10) {if (strcmp("2147483648",yytext)<0) overFlag = 1;}    else overFlag = 1;    printf("%s",leafLable);    if(overFlag){printf("(overflow!!!)"); overFlag = 0;}    printf("    ");    return INT;}
-{digitNz}{digit}*    {state = 1; strcpy(leafLable,"NEGINT: ");    strcat(leafLable,yytext);    if (yyleng<11) {}    else if (yyleng==11) {if (strcmp("-2147483648",yytext)<0) overFlag = 1;}    else overFlag = 1;    printf("%s",leafLable);    if(overFlag){printf("(overflow!!!)"); overFlag = 0;}    printf("    ");    return NEGINT;}
0x{hexDigitNz}{hexDigit}*     {state = 1; strcpy(leafLable,"INT: ");    strcat(leafLable,yytext);    if (yyleng<10) {}    else if (yyleng==10) {if (strcmp("0x80000000",yytext)<0) overFlag = 1;}    else overFlag = 1;    printf("%s",leafLable);    if(overFlag){printf("(overflow!!!)"); overFlag = 0;}    printf("    ");    return INT;}
-0x{hexDigitNz}{hexDigit}*   {state = 1; strcpy(leafLable,"NEGINT: ");    strcat(leafLable,yytext);    if (yyleng<11) {}    else if (yyleng==11) {if (strcmp("-0x80000000",yytext)<0) overFlag = 1;}    else overFlag = 1;    printf("%s",leafLable);    if(overFlag){printf("(overflow!!!)"); overFlag = 0;}    printf("    ");    return NEGINT;}
0X{hexDigitNz}{hexDigit}*     {state = 1; strcpy(leafLable,"INT: ");    strcat(leafLable,yytext);    if (yyleng<10) {}    else if (yyleng==10) {if (strcmp("0X80000000",yytext)<0) overFlag = 1;}    else overFlag = 1;    printf("%s",leafLable);    if(overFlag){printf("(overflow!!!)"); overFlag = 0;}    printf("    ");    return INT;}
-0X{hexDigitNz}{hexDigit}*   {state = 1; strcpy(leafLable,"NEGINT: ");    strcat(leafLable,yytext);    if (yyleng<11) {}    else if (yyleng==11) {if (strcmp("-0X80000000",yytext)<0) overFlag = 1;}    else overFlag = 1;    printf("%s",leafLable);    if(overFlag){printf("(overflow!!!)"); overFlag = 0;}    printf("    ");    return NEGINT;}
0{octDigitNz}{octDigit}*    {state = 1; strcpy(leafLable,"INT: ");    strcat(leafLable,yytext);    if (yyleng<12) {}    else if (yyleng==12) {if (strcmp("020000000000",yytext)<0) overFlag = 1;}    else overFlag = 1;    printf("%s",leafLable);    if(overFlag){printf("(overflow!!!)"); overFlag = 0;}    printf("    ");    return INT;}
-0{octDigitNz}{octDigit}*    {state = 1; strcpy(leafLable,"NEGINT: ");    strcat(leafLable,yytext);    if (yyleng<13) {}    else if (yyleng==13) {if (strcmp("-020000000000",yytext)<0) overFlag = 1;}    else overFlag = 1;    printf("%s",leafLable);    if(overFlag){printf("(overflow!!!)"); overFlag = 0;}    printf("    ");    return NEGINT;}

{charUs}{charNumUs}*    {leafLable=(char*)malloc(sizeof(char)*60);state = 1; strcpy(leafLable,"ID: ");strcat(leafLable,yytext);printf("%s    ",leafLable);return ID;}

";"    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"SEMI: ");strcat(tp,yytext); printf("%s    ",tp); return SEMI;}
","    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"COMMA: ");strcat(tp,yytext); printf("%s    ",tp); return COMMA;}
"."    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"DOT: ");strcat(tp,yytext); printf("%s    ",tp); return DOT;}
"="    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"ASSIGNOP: ");strcat(tp,yytext); printf("%s    ",tp); return ASSIGNOP;}
"("    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"LP: ");strcat(tp,yytext); printf("%s    ",tp); return LP;}
")"    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"RP: ");strcat(tp,yytext); printf("%s    ",tp); return RP;}
"["    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"LB: ");strcat(tp,yytext); printf("%s    ",tp); return LB;}
"]"    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"RB: ");strcat(tp,yytext); printf("%s    ",tp); return RB;}
"{"    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"LC: ");strcat(tp,yytext); printf("%s    ",tp); return LC;}
"}"    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"RC: ");strcat(tp,yytext); printf("%s    ",tp); return RC;}

"-"    {tp=(char*)malloc(sizeof(char)*60); if(state==0) strcpy(tp,"UMINUS: "); else strcpy(tp,"MINUS: "); strcat(tp,yytext); printf("%s    ",tp); if (state==0)return UMINUS; else return MINUS;}
"!"	   {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"LOGICN: ");strcat(tp,yytext); printf("%s    ",tp); return LOGICN;}
"++"    {state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"INCRE: ");strcat(tp,yytext); printf("%s    ",tp); return INCRE;}
"--"	{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"DECRE: ");strcat(tp,yytext); printf("%s    ",tp); return DECRE;}
"~"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"BITNOT: ");strcat(tp,yytext); printf("%s    ",tp); return BITNOT;}

"*"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"MULT: ");strcat(tp,yytext); printf("%s    ",tp); return MULT;}
"/"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"DIV: ");strcat(tp,yytext); printf("%s    ",tp); return DIV;}
"%"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"MOD: ");strcat(tp,yytext); printf("%s    ",tp); return MOD;}

"+"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"ADD: ");strcat(tp,yytext); printf("%s    ",tp); return ADD;}

"<<"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"SHIFTL: ");strcat(tp,yytext); printf("%s    ",tp); return SHIFTL;}
">>"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"SHIFTR: ");strcat(tp,yytext); printf("%s    ",tp); return SHIFTR;}

">"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"GREATER: ");strcat(tp,yytext); printf("%s    ",tp); return GREATER;}
">="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"NLESS: ");strcat(tp,yytext); printf("%s    ",tp); return NLESS;}
"<"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"LESS: ");strcat(tp,yytext); printf("%s    ",tp); return LESS;}
"<="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"NGREATER: ");strcat(tp,yytext); printf("%s    ",tp); return NGREATER;}

"=="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"EQUAL: ");strcat(tp,yytext); printf("%s    ",tp); return EQUAL;}
"!="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"NEQUAL: ");strcat(tp,yytext); printf("%s    ",tp); return NEQUAL;}

"&"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"BITAND: ");strcat(tp,yytext); printf("%s    ",tp); return BITAND;}

"^"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"BITXOR: ");strcat(tp,yytext); printf("%s    ",tp); return BITXOR;}

"|"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"BITOR: ");strcat(tp,yytext); printf("%s    ",tp); return BITOR;}

"&&"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"LOGICAND: ");strcat(tp,yytext); printf("%s    ",tp); return LOGICAND;}
"||"		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"LOGICOR: ");strcat(tp,yytext); printf("%s    ",tp); return LOGICOR;}

"+="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"ADDA: ");strcat(tp,yytext); printf("%s    ",tp); return ADDA;}
"-="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"MINUSA: ");strcat(tp,yytext); printf("%s    ",tp); return MINUSA;}
"*="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"MULTA: ");strcat(tp,yytext); printf("%s    ",tp); return MULTA;}
"/="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"DIVA: ");strcat(tp,yytext); printf("%s    ",tp); return DIVA;}
"&="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"BITANDA: ");strcat(tp,yytext); printf("%s    ",tp); return BITANDA;}
"^="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"BITXORA: ");strcat(tp,yytext); printf("%s    ",tp); return BITXORA;}
"|="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"BITORA: ");strcat(tp,yytext); printf("%s    ",tp); return BITORA;}
"<<="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"SHIFTLA: ");strcat(tp,yytext); printf("%s    ",tp); return SHIFTLA;}
">>="		{state = 0;  tp=(char*)malloc(sizeof(char)*60);strcpy(tp,"SHIFTRA: ");strcat(tp,yytext); printf("%s    ",tp); return SHIFTRA;}

.    {printf("error");}

%%
int yywrap()
{
  return 1;
}
