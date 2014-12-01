%{
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#define YYSTYPE double

#define YYDEBUG 0

extern char *yytext;
extern char *leafLable;

struct treeNode
{
    char* lable;
	struct treeNode* child[10];
	int code;
};
// For INT/ID, lable = token+lexeme. For others, lable = token.
// A pointer array of 10 is sufficient
// code is something crucial when drawing the syntax tree map

struct symbol
{
    char* word;
    char type;
    char* arrSize;
    char* structName;
    int structMem;
};
struct symbol* symTable[27][20]; //symbol Table

int rNum, callNum, ifNum, forNum, arridxNum; //for register allocation
int paraFlag = 0; //paremetres flag
int paraPoint = 0; //parameters point
char* paraArr[10]; //parametres array
int entryDepth = 0; //depth of stmtblocks
int loadFlag = 1; //load or not?
char* arrName; //array Name
char* arrSize; //array size
char* strName; //struct name
int structMemNum; //Number of struct members

//The followings are variables for graph DIY
char* nodeShape;
char filled;
char* nodeColor;
int nodePeri;
char* edgeStyle;
char* edgeColor;
char* arrowhead;
char diy;
char rounded;
void graphPrint();
void graphConfig();

struct treeNode* nTerBuild(char* l);  //the node has one or more non-terminal children
struct treeNode* terBuild(char* pl,char* sl);  //the node only has one terminal child
void nTerInsert(struct treeNode* root, struct treeNode* sp);  //insert a non-terminal child to its parent
void terInsert(struct treeNode* root, char* l);  //insert a terminal one
void preOrder(struct treeNode* root, int depth);  //preOrder is used to print the syntax tree of indentation form
void lablePostOrder(struct treeNode* root);  //the first part of syntax tree pic txt, declares the node and its lable
void childPostOrder(struct treeNode* root);  //the second part, declares the relationships among the nodes.

//The followings are functions for LLVM IR generation
void _Program(struct treeNode* root);
void _Extdefs(struct treeNode* t);
void _Extdef(struct treeNode* t);
void _ExtvarsType(struct treeNode* t);
void _DecExt(struct treeNode* t);
void _Func(struct treeNode* t);
void _Paras(struct treeNode* t);
void _Para(struct treeNode* t);
void _Stmtblock(struct treeNode* t);
void _Defs(struct treeNode* t);
void _Def(struct treeNode* t);
void _Decs(struct treeNode* t);
void _DecInner(struct treeNode* t);
void _Stmts(struct treeNode* t);
void _Stmt(struct treeNode* t);
char* _Exp(struct treeNode* t);
void _ArgsExt(struct treeNode* t);
void _ArgsInner(struct treeNode* t);
void _ArgsFunc(struct treeNode* t);

//Well, I added functions of struct after DS test
void _ExtdefStruct(struct treeNode* t);
void _DecStrId(struct treeNode* t);
void _ExtdefStrId(struct treeNode* t);
void _ExtvarsStrId(struct treeNode* t);
void _ExtdefStrOp(struct treeNode* t);
void _DefsStrOp(struct treeNode* t);
void _DefStrOp(struct treeNode* t);


struct treeNode *root;  //always the root
struct treeNode *stack[5000];  //the node stack
int top = 0;
int curCode = 0;  //current code
%}

%token	TYPE STRUCT RETURN IF ELSE BREAK CONT FOR INT NEGINT ID SEMI COMMA LC RC READ WRITE

%right 	ASSIGNOP ADDA MINUSA MULTA DIVA BITANDA BITXORA BITORA SHIFTLA SHIFTRA
%left	LOGICOR
%left 	LOGICAND
%left	BITOR
%left	BITXOR
%left	BITAND
%left	EQUAL NEQUAL
%left	GREATER LESS NGREATER NLESS
%left	SHIFTL SHIFTR
%left  	ADD MINUS
%left 	MULT DIV MOD
%right	UMINUS LOGICN BITNOT INCRE DECRE
%right 	LB LP
%left   RB RP DOT

%%

PROGRAM	: EXTDEFS	{root = nTerBuild("PROGRAM"); nTerInsert(root,stack[top]); --top;}
		;

EXTDEFS	: EXTDEF EXTDEFS	{root=nTerBuild("EXTDEFS"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| {stack[++top]=terBuild("EXTDEFS","NULL");}
		;

EXTDEF	: SPEC EXTVARS SEMI		{ root=nTerBuild("EXTDEF"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 2; terInsert(root,"SEMI"); stack[++top]=root;}
		| SPEC FUNC STMTBLOCK	{ root=nTerBuild("EXTDEF"); nTerInsert(root,stack[top-2]); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 3; stack[++top]=root;}
		;

EXTVARS	: DEC	{ root=nTerBuild("EXTVARS"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| DEC COMMA EXTVARS		{ root=nTerBuild("EXTVARS"); nTerInsert(root,stack[top-1]); terInsert(root,"COMMA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| {stack[++top]=terBuild("EXTVARS","NULL");}
		;

SPEC	: TYPE	{stack[++top]=terBuild("SPEC","TYPE");}
		| STSPEC	{ root=nTerBuild("SPEC"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		;

STSPEC	: STRUCT OPTTAG LC DEFS RC	{ root=nTerBuild("STSPEC"); terInsert(root,"STRUCT"); nTerInsert(root,stack[top-1]); terInsert(root,"LC"); nTerInsert(root,stack[top]); top -= 2; terInsert(root,"RC"); stack[++top]=root;}
		| STRUCT THEID		{ root=nTerBuild("STSPEC"); terInsert(root,"STRUCT"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		;

OPTTAG	: THEID{root=nTerBuild("OPTTAG"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| {stack[++top]=terBuild("OPTTAG","NULL");}
		;

VAR		: THEID	{root=nTerBuild("VAR"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| VAR LB INT RB	{ root=nTerBuild("VAR"); nTerInsert(root,stack[top]); --top; terInsert(root,"LB"); terInsert(root,leafLable); terInsert(root,"RB"); stack[++top]=root;}
		;

FUNC	: THEID LP PARAS RP	{ root=nTerBuild("FUNC"); nTerInsert(root,stack[top-1]); terInsert(root,"LP"); nTerInsert(root,stack[top]); top-=2; terInsert(root,"RP"); stack[++top]=root;}
		;

PARAS	: PARA COMMA PARAS	{ root=nTerBuild("PARAS"); nTerInsert(root,stack[top-1]); terInsert(root,"COMMA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| PARA	{ root=nTerBuild("PARAS"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| {stack[++top]=terBuild("PARAS","NULL");}
		;

PARA	: SPEC VAR	{ root=nTerBuild("PARA"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		;

STMTBLOCK: LC DEFS STMTS RC	{ root=nTerBuild("STMTBLOCK"); terInsert(root,"LC"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 2; terInsert(root,"RC"); stack[++top]=root;}
		;

STMTS	: STMT STMTS	{ root=nTerBuild("STMTS"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| {stack[++top]=terBuild("STMTS","NULL");}
		;

STMT	: EXP SEMI	{ root=nTerBuild("STMT"); nTerInsert(root,stack[top]); --top; terInsert(root,"SEMI"); stack[++top]=root;}
		| STMTBLOCK	{ root=nTerBuild("STMT"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| RETURN EXP SEMI	{ root=nTerBuild("STMT"); terInsert(root,"RETURN"); nTerInsert(root,stack[top]); --top; terInsert(root,"SEMI"); stack[++top]=root;}
		| IF LP EXP RP STMT ESTMT	{ root=nTerBuild("STMT"); terInsert(root,"IF"); terInsert(root,"LP"); nTerInsert(root,stack[top-2]); terInsert(root,"RP"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 3; stack[++top]=root;}
		| FOR LP EXP SEMI EXP SEMI EXP RP STMT	{ root=nTerBuild("STMT"); terInsert(root,"FOR"); terInsert(root,"LP"); nTerInsert(root,stack[top-3]); terInsert(root,"SEMI"); nTerInsert(root,stack[top-2]); terInsert(root,"SEMI"); nTerInsert(root,stack[top-1]); terInsert(root,"RP"); nTerInsert(root,stack[top]); top -= 4; stack[++top]=root;}
		| CONT SEMI	{ root=nTerBuild("STMT"); terInsert(root,"CONT"); terInsert(root,"SEMI"); stack[++top]=root;}
		| BREAK SEMI	{ root=nTerBuild("STMT"); terInsert(root,"BREAK"); terInsert(root,"SEMI"); stack[++top]=root;}
		;

ESTMT	: ELSE STMT	{ root=nTerBuild("ESTMT"); terInsert(root,"ELSE"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| {stack[++top]=terBuild("ESTMT","NULL");}
		;

DEFS	: DEF DEFS	{ root=nTerBuild("DEFS"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| {stack[++top]=terBuild("DEFS","NULL");}
		;

DEF		: SPEC DECS SEMI	{ root=nTerBuild("DEF"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top -= 2; terInsert(root,"SEMI"); stack[++top]=root;}
		;

DECS	: DEC COMMA DECS	{ root=nTerBuild("DECS"); nTerInsert(root,stack[top-1]); terInsert(root,"COMMA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| DEC	{ root=nTerBuild("DECS"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		;

DEC		: VAR	{ root=nTerBuild("DEC"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| VAR ASSIGNOP INIT	{ root=nTerBuild("DEC"); nTerInsert(root,stack[top-1]); terInsert(root,"ASSIGNOP"); nTerInsert(root,stack[top]); top-=2; stack[++top]=root;}
		;

INIT	: EXP	{ root=nTerBuild("INIT"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| LC ARGS RC	{ root=nTerBuild("INIT"); terInsert(root,"LC"); nTerInsert(root,stack[top]); --top; terInsert(root,"RC"); stack[++top]=root;}
		;

EXP		: EXP DOT EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"DOT"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP ASSIGNOP EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"ASSIGNOP"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP ADD EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"ADD"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP MINUS EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"MINUS"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP MULT EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"MULT"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP DIV EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"DIV"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP MOD EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"MOD"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP SHIFTLA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"SHIFTLA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP SHIFTRA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"SHIFTRA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP SHIFTL EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"SHIFTL"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP SHIFTR EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"SHIFTR"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP LESS EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"LESS"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP GREATER EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"GREATER"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP NGREATER EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"NGREATER"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP NLESS EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"NLESS"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP EQUAL EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"EQUAL"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP NEQUAL EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"NEQUAL"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP LOGICAND EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"LOGICAND"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP LOGICOR EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"LOGICOR"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP BITAND EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"BITAND"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP BITXOR EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"BITXOR"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP BITOR EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"BITOR"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP ADDA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"ADDA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP MINUSA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"MINUSA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP MULTA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"MULTA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP DIVA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"DIVA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP BITANDA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"BITAND"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP BITXORA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"BITXOR"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP BITORA EXP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"BITOR"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| INCRE EXP	{ root=nTerBuild("EXP"); terInsert(root,"INCRE"); nTerInsert(root,stack[top]); --top;  stack[++top]=root;}
		| DECRE EXP	{ root=nTerBuild("EXP"); terInsert(root,"DECRE"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| LOGICN EXP	{ root=nTerBuild("EXP"); terInsert(root,"LOGICN"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| BITNOT EXP	{ root=nTerBuild("EXP"); terInsert(root,"BITNOT"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| UMINUS EXP	{ root=nTerBuild("EXP"); terInsert(root,"UMINUS"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		| LP EXP RP	{ root=nTerBuild("EXP"); terInsert(root,"LP"); nTerInsert(root,stack[top]); --top; terInsert(root,"RP"); stack[++top]=root;}
		| THEID LP ARGS RP	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"LP"); nTerInsert(root,stack[top]); top-=2; terInsert(root,"RP"); stack[++top]=root;}
		| THEID ARRS	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); nTerInsert(root,stack[top]); top-=2; stack[++top]=root;}
		| EXP DOT THEID	{ root=nTerBuild("EXP"); nTerInsert(root,stack[top-1]); terInsert(root,"DOT"); nTerInsert(root,stack[top]); top-=2; stack[++top]=root;}
		| EXP NEGINT	{root=nTerBuild("EXP"); nTerInsert(root,stack[top]); --top; terInsert(root,leafLable); stack[++top]=root;}
		| NEGINT	{stack[++top]=terBuild("EXP",leafLable);}
		| INT	{stack[++top]=terBuild("EXP",leafLable);}
		| {stack[++top]=terBuild("EXP","NULL");}
		| WR	{root=nTerBuild("EXP"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		;

THEID	: 	ID	{stack[++top]=terBuild("THEID",leafLable);}
		;

WR	: WRITE LP EXP RP {root=nTerBuild("WR"); terInsert(root,"WRITE"); terInsert(root,"LP"); nTerInsert(root,stack[top]); --top; terInsert(root,"RP"); stack[++top]=root;}
		| READ LP EXP RP {root=nTerBuild("WR"); terInsert(root,"READ"); terInsert(root,"LP"); nTerInsert(root,stack[top]); --top; terInsert(root,"RP"); stack[++top]=root;}
		;

ARRS	: LB EXP RB ARRS	{ root=nTerBuild("ARRS"); terInsert(root,"LB"); nTerInsert(root,stack[top-1]); terInsert(root,"RB"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| {stack[++top]=terBuild("ARRS","NULL");}
		;

ARGS	: EXP COMMA ARGS	{ root=nTerBuild("ARGS"); nTerInsert(root,stack[top-1]); terInsert(root,"COMMA"); nTerInsert(root,stack[top]); top -= 2; stack[++top]=root;}
		| EXP	{ root=nTerBuild("ARGS"); nTerInsert(root,stack[top]); --top; stack[++top]=root;}
		;

%%
int main(int argc, char *argv[])
{
	//yydebug = 1;
	//graphConfig();

	extern char *yytext;
	extern FILE* yyin,*yyout;

	freopen(argv[1], "r", stdin);
	freopen(argv[2], "w+", stdout); // i/o redirection

	yyin=stdin;
	yyout=stdout;

	//printf("== Step 1: Lexical Analysis ==\n");
	yyparse(); //lexemes are printed, and the tree is built
	//printf("\n\n== Step 2: Syntax Analysis ==\n");
	//preOrder(root,0);

    /*
	fclose(stdout);
	freopen(argv[3], "w+", stdout);
	yyout=stdout;

	printf("digraph L0\n");
	printf("{\n");
	printf("\tordering=out;\n");
	graphPrint();
	lablePostOrder(root);
	childPostOrder(root);
	printf("}");

	fclose(stdout);
	freopen(argv[4], "w+", stdout);
	yyout=stdout;
	*/

	_Program(root);

	return 0;
}

void graphConfig() //do graph configration jobs
{
    printf("\nWelcome to the world of Small-C!\n");
    printf("First, let's do some preparing work.\n");
    printf("Would you like to DIY your syntax tree visualization(y/n)? ");

    scanf("%c",&diy);
    printf("\n");
    if (diy=='y')
    {
        printf("OK. Let's move on!\n");
        printf("Totally there are 8 steps: \n");
        printf("(1/8) Enter the shape of node: ");
        nodeShape = (char*)malloc(sizeof(char)*60);
        scanf("%s",nodeShape);
        getchar();

        printf("(2/8) Would you like to round your node(y/n)? ");
        scanf("%c",&rounded);

        printf("(3/8) Enter the color of node: ");
        nodeColor = (char*)malloc(sizeof(char)*60);
        scanf("%s",nodeColor);
        getchar();

        printf("(4/8) Would you like to fill your node(y/n)? ");
        scanf("%c",&filled);

        printf("(5/8) Enter the periphery of node: ");
        scanf("%d",&nodePeri);

        printf("(6/8) Enter the style of edge: ");
        edgeStyle = (char*)malloc(sizeof(char)*60);
        scanf("%s",edgeStyle);

        printf("(7/8) Enter the color of edge: ");
        edgeColor = (char*)malloc(sizeof(char)*60);
        scanf("%s",edgeColor);

        printf("(8/8) Enter the arrowhead of edge: ");
        arrowhead = (char*)malloc(sizeof(char)*60);
        scanf("%s",arrowhead);
        printf("\nEnjoy your own figure now!\n");
    }
    else printf("Well, at least you don't have to key in a lot of words...\nEnjoy the figure now!\n");
}

void graphPrint() //print the graph in DOT language
{
    if (diy=='n')
    {
        printf("\tnode [shape=box];\n");
    }
    else
    {
        printf("\tnode [shape=");
        if (rounded=='y') printf("M");
        printf("%s,",nodeShape);

        if (filled=='y') printf("style=filled,");

        printf("color=%s,",nodeColor);
        printf("peripheries=%d];\n",nodePeri);

        printf("\tedge [color=%s,style=%s,arrowhead = %s];\n",edgeColor,edgeStyle,arrowhead);
    }
}

struct treeNode* nTerBuild(char* l)
{
    struct treeNode* r;
	r = (struct treeNode*)malloc(sizeof(struct treeNode));
	r->lable = (char*)malloc(sizeof(char)*60);
	strcpy(r->lable,l);
    int i;
	for (i=0;i<10;i++) r->child[i] = NULL; //soon its child will be added
	r->code = curCode++;

	return r;
}

struct treeNode* terBuild(char* pl,char* sl)
{
    struct treeNode* p;
	p = (struct treeNode*)malloc(sizeof(struct treeNode));
	p->lable = (char*)malloc(sizeof(char)*60);
	strcpy(p->lable,pl);
    int i;
	for (i=0;i<10;i++) p->child[i] = NULL;
	p->code = curCode++;

	p->child[0] = (struct treeNode*)malloc(sizeof(struct treeNode)); //its only terminal child
	p->child[0]->lable = (char*)malloc(sizeof(char)*60);
	strcpy(p->child[0]->lable,sl);
	for (i=0;i<10;i++) p->child[0]->child[i] = NULL;
	p->child[0]->code = curCode++;

	return p;
}

void nTerInsert(struct treeNode* root, struct treeNode* sp)
{
    int i=0;
	while (root->child[i]) i++;

	root->child[i] = sp;
}

void terInsert(struct treeNode* root, char* l)
{
	int i=0;
	while (root->child[i]) i++;

	root->child[i] = (struct treeNode*)malloc(sizeof(struct treeNode));
	root->child[i]->lable = (char*)malloc(sizeof(char)*60);
	strcpy(root->child[i]->lable,l);
    int k;
	for (k=0;k<10;k++) root->child[i]->child[k] = NULL;
	root->child[i]->code = curCode++;
}

void preOrder(struct treeNode* root, int depth)
{
    int k;
    for (k=0;k<depth*4;k++) printf(" "); //indentation is 4 blanks per layer
	printf("%s\n",root->lable);

	int i = 0;
	if (root->child[i]) depth++;
	while (root->child[i]) {preOrder(root->child[i],depth); i++;}
}

// the rule of GraphViz file is so easy, a scan of out2.txt is enough
void lablePostOrder(struct treeNode* root)
{
	int i = 0;
	while (root->child[i]) {lablePostOrder(root->child[i]); i++;}

	printf("\tn%d [label=\"%s\"];\n",root->code,root->lable);
}

void childPostOrder(struct treeNode* root)
{
	int i = 0;
    if (!root->child[i]) return;
	while (root->child[i]) {childPostOrder(root->child[i]); i++;}

	printf("\tn%d -> { ",root->code);
	int k;
	for (k=0;k<i;k++) printf("n%d ",root->child[k]->code);
	printf("};\n");
}

void _Program(struct treeNode* root) //root of all syntax trees
{
    printf("@.str = private unnamed_addr constant [3 x i8] c\"%%d\\00\", align 1\n");
    printf("@.str1 = private unnamed_addr constant [2 x i8] c\"\\0A\\00\", align 1\n"); //这玩意儿是为LLVM IR中的换行符准备的

    _Extdefs(root->child[0]);

    printf("\ndeclare i32 @__isoc99_scanf(i8*, ...) #1\n");
    printf("declare i32 @printf(i8*, ...) #1\n");
}

void _Extdefs(struct treeNode* t) //external defininitions
{
    if (t->child[1]) //EXTDEF EXTDEFS
    {
        _Extdef(t->child[0]);
        _Extdefs(t->child[1]);
    }
}

void _Extdef(struct treeNode* t) //external definition
{
    if (t->child[1]->lable[0]=='E') //SPEC EXTVARS SEMI
    {
        if (t->child[0]->child[0]->lable[0]=='T') //TYPE, int case
        {
            //EXTVARS will never be void in this case
            _ExtvarsType(t->child[1]);
        }
        else //STSPEC, struct case
        {
            _ExtdefStruct(t);
        }
    }
    else //SPEC FUNC STMTBLOCK
    {
        //We don't need to check SPEC anymore, cause all the functions in Small-C returns INT
        _Func(t->child[1]);
        _Stmtblock(t->child[2]);
    }
}

void _ExtdefStruct(struct treeNode* t) //external definition of struct
{
    if (t->child[0]->child[0]->child[1]->lable[0]=='T')//STRUCT THEID
    {
        _ExtdefStrId(t);
    }
    else _ExtdefStrOp(t);
}

void _ExtdefStrOp(struct treeNode* t) //STSPEC -> STRUCT OPTTAG LC DEFS RC
{
    struct treeNode* nodeId = t->child[0]->child[0]->child[1]->child[0]->child[0];

    char* tmp = (char*)malloc(sizeof(char)*200);
    int len = strlen(nodeId->lable);
    int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

    structMemNum = 0;
    printf("%%struct.%s = type { ",tmp);
    _DefsStrOp(t->child[0]->child[0]->child[3]);
    printf(" }\n",tmp);
    structMemNum = 0;
}

void _DefsStrOp(struct treeNode* t) //definitons, for STSPEC -> STRUCT OPTTAG LC DEFS RC case
{
    if (t->child[1]) //DEF DEFS
    {
        _DefStrOp(t->child[0]);
        structMemNum++;
        if (strcmp(t->child[1]->child[0]->lable,"NULL")) printf(", ");
        _DefsStrOp(t->child[1]);
    }
}

void _DefStrOp(struct treeNode* t) //definiton, for STSPEC -> STRUCT OPTTAG LC DEFS RC case
{
    struct treeNode* nodeId = t->child[1]->child[0]->child[0]->child[0]->child[0];

    char* tmp = (char*)malloc(sizeof(char)*200);
    int len = strlen(nodeId->lable);
    int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

    int dim1 = tmp[0]-'a';
    if (dim1<0) dim1 = tmp[0]-'A';
    if (tmp[0]=='_') dim1 = 26;
    i=0;
    while (symTable[dim1][i]) i++;
    symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
    struct symbol* s = symTable[dim1][i];
    s->word = (char*)malloc(sizeof(char)*200);
    strcpy(s->word,tmp); //don't need s->type here
    s->structMem = structMemNum;

    printf("i32");
}

void _ExtdefStrId(struct treeNode* t) //external definiton for STRUCT THEID
{
    strName = (char*)malloc(sizeof(char)*200);

    struct treeNode* nodeId = t->child[0]->child[0]->child[1]->child[0];
    char* tmp = (char*)malloc(sizeof(char)*200);
    int len = strlen(nodeId->lable);
    int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

    strcpy(strName,tmp);

    _ExtvarsStrId(t->child[1]);

    free(strName);
}

void _ExtvarsStrId(struct treeNode* t) //external variables for STRUCT ID
{
    if (t->child[1])
    {
        _DecStrId(t->child[0]);
        _ExtvarsStrId(t->child[2]);
    }
    else _DecStrId(t->child[0]);
}

void _DecStrId(struct treeNode* t) //declaration for STRUCT ID
{
    struct treeNode* nodeId = t->child[0]->child[0]->child[0];
    char* tmp = (char*)malloc(sizeof(char)*200);
    int len = strlen(nodeId->lable);
    int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

    int dim1 = tmp[0]-'a';
    if (dim1<0) dim1 = tmp[0]-'A';
    if (tmp[0]=='_') dim1 = 26;
    i=0;
    while (symTable[dim1][i]) i++;
    symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
    struct symbol* s = symTable[dim1][i];
    s->word = (char*)malloc(sizeof(char)*200);
    strcpy(s->word,tmp);
    s->structName = (char*)malloc(sizeof(char)*200);
    strcpy(s->structName,strName);
    s->type = 'g';

    printf("@%s",tmp);
    printf(" = common global %%struct.%s zeroinitializer, align 4\n",strName);
}

void _ExtvarsType(struct treeNode* t) //external variables for TYPE
{
    if (t->child[1]==NULL) //DEC case
    {
        _DecExt(t->child[0]); //the DECs outside the function(global variables) are different from those inside it! Mark it as a different case!
    }
    else //DEC COMMA EXTVARS case
    {
        _DecExt(t->child[0]);
        _ExtvarsType(t->child[2]);
    }
}

void _DecExt(struct treeNode* t) //declartion outside the stmtblock, in other words, global variables
{
    //DecExt has 4 cases in all, let's cite 4 examples here to illustrate them:
    //1. a
    //2. a = 1
    //3. a[3]
    //4. a[3] = {1,2,3}
    //QUESTION: Case like a = b, is it legal here, or not?
    //NOT!!! And the following is what clang reported:
    //error: initializer element is not a compile-time constant

    //In conclusion, 4 cases is all we have to deal with here
    //As a result, it should not be a difficult task ^_^
    if (t->child[1]==NULL) //VAR case
    {
        struct treeNode* nodeVar = t->child[0]; //node of VAR
        if (nodeVar->child[1]==NULL) //case 1. a
        {
            printf("@");
            struct treeNode* nodeId = nodeVar->child[0]->child[0];
            int len = strlen(nodeId->lable);

            char* tmp = (char*)malloc(sizeof(char)*60);
            int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
            int dim1 = tmp[0]-'a';
            if (dim1<0) dim1 = tmp[0]-'A';
            if (tmp[0]=='_') dim1 = 26;
            i=0;
            while (symTable[dim1][i]) i++;
            symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
            struct symbol* s = symTable[dim1][i];
            s->word = (char*)malloc(sizeof(char)*60);
            strcpy(s->word,tmp);
            s->type = 'g';

            printf("%s",tmp);
            printf(" = common global i32 0, align 4\n");
        }
        else //case 3. a[3]
        {
            //@b = common global [20 x i32] zeroinitializer, align 4
            printf("@");
            struct treeNode* nodeId = nodeVar->child[0]->child[0]->child[0];
            int len = strlen(nodeId->lable);

            char* tmp = (char*)malloc(sizeof(char)*60);
            int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
            int dim1 = tmp[0]-'a';
            if (dim1<0) dim1 = tmp[0]-'A';
            if (tmp[0]=='_') dim1 = 26;
            i=0;
            while (symTable[dim1][i]) i++;
            symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
            struct symbol* s = symTable[dim1][i];
            s->word = (char*)malloc(sizeof(char)*60);
            strcpy(s->word,tmp);
            s->type = 'g';

            printf("%s",tmp);
            printf(" = common global [");
            struct treeNode* nodeInt = nodeVar->child[2];
            len = strlen(nodeInt->lable);
            i=5; for (i=5;i<len;i++) printf("%c",nodeInt->lable[i]);

            s->arrSize = (char*)malloc(sizeof(char)*60);
            i=5; for (i=5;i<=len;i++) s->arrSize[i-5] = nodeInt->lable[i];

            printf(" x i32] zeroinitializer, align 4\n");
        }
    }
    else //VAR ASSIGNOP INIT case
    {
        struct treeNode* nodeVar = t->child[0]; //node of VAR
        if (nodeVar->child[1]==NULL) //case 2. a = 1
        {
            //@ans = global i32 0, align 4
            printf("@");
            struct treeNode* nodeId = nodeVar->child[0]->child[0];
            int len = strlen(nodeId->lable);

            char* tmp = (char*)malloc(sizeof(char)*60);
            int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
            int dim1 = tmp[0]-'a';
            if (dim1<0) dim1 = tmp[0]-'A';
            if (tmp[0]=='_') dim1 = 26;
            i=0;
            while (symTable[dim1][i]) i++;
            symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
            struct symbol* s = symTable[dim1][i];
            s->word = (char*)malloc(sizeof(char)*60);
            strcpy(s->word,tmp);
            s->type = 'g';

            printf("%s",tmp);

            struct treeNode* nodeInit = t->child[2]->child[0]->child[0];
            len = strlen(nodeInit->lable);
            i=5; for (i=5;i<=len;i++) tmp[i-5] = nodeInit->lable[i];

            printf(" = global i32 %s, align 4\n",tmp);
        }
        else //case 4, a[3] = {1,2,3}
        {
            //A little bit complicated
            //@mat = global [4 x i32] [i32 0, i32 1, i32 1, i32 1], align 4
            //@b = common global [20 x i32] zeroinitializer, align 4
            printf("@");
            struct treeNode* nodeId = nodeVar->child[0]->child[0]->child[0];
            int len = strlen(nodeId->lable);

            char* tmp = (char*)malloc(sizeof(char)*60);
            int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
            int dim1 = tmp[0]-'a';
            if (dim1<0) dim1 = tmp[0]-'A';
            if (tmp[0]=='_') dim1 = 26;
            i=0;
            while (symTable[dim1][i]) i++;
            symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
            struct symbol* s = symTable[dim1][i];
            s->word = (char*)malloc(sizeof(char)*60);
            strcpy(s->word,tmp);
            s->type = 'g';

            printf("%s",tmp);
            printf(" = global [");
            struct treeNode* nodeInt = nodeVar->child[2];
            len = strlen(nodeInt->lable);
            i=5; for (i=5;i<len;i++) printf("%c",nodeInt->lable[i]);

            s->arrSize = (char*)malloc(sizeof(char)*60);
            i=5; for (i=5;i<=len;i++) s->arrSize[i-5] = nodeInt->lable[i];

            printf(" x i32] [");
            _ArgsExt(t->child[2]->child[1]);
            printf("], align 4\n");
        }
    }
}

void _ArgsExt(struct treeNode* t) //arguments external
{
    if (t->child[1]==NULL) //EXP
    {
        printf("i32 ");
        char* val = (char*)malloc(sizeof(char)*60);
        val = _Exp(t->child[0]);
        printf("%s",val);
    }
    else //EXP COMMA ARGS
    {
        printf("i32 ");
        char* val = (char*)malloc(sizeof(char)*60);
        val = _Exp(t->child[0]);
        printf("%s, ",val);
        _ArgsExt(t->child[2]);
    }
}

void _Func(struct treeNode* t) //function
{
    rNum = callNum = ifNum = forNum = arridxNum = 0;

    printf("\n");
    //define i32 @dfs(i32 %x) #0 {
    //entry:
    printf("define i32 @");

    struct treeNode* nodeId = t->child[0]->child[0];
    int len = strlen(nodeId->lable);
    char* tmp = (char*)malloc(sizeof(char)*60);
    int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

    printf("%s(",tmp);
    if (t->child[2]->child[0]->lable[0]=='N') paraFlag = 0; //PARAS->NULL
    else
    {
         paraFlag = 1;
        _Paras(t->child[2]);
    }
    printf(") #0\n");
}

void _Paras(struct treeNode* t) //parametres
{
    if (t->child[0]->lable[0]=='N') {}
    else if (t->child[1]) //PARA COMMA PARAS
    {
        _Para(t->child[0]);
        printf(", ");
        _Paras(t->child[2]);
    }
    else _Para(t->child[0]); //PARA
}

void _Para(struct treeNode* t) //parametre
{
    struct treeNode* nodeId = t->child[1]->child[0]->child[0];
    int len = strlen(nodeId->lable);
    char* tmp = (char*)malloc(sizeof(char)*60);
    int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
    int dim1 = tmp[0]-'a';
    if (dim1<0) dim1 = tmp[0]-'A';
    if (tmp[0]=='_') dim1 = 26;
    i=0;
    while (symTable[dim1][i]) i++;
    symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
    struct symbol* s = symTable[dim1][i];
    s->word = (char*)malloc(sizeof(char)*60);
    strcpy(s->word,tmp);
    s->type = 'a';

    printf("i32 %%");
    printf("%s",tmp);

    paraArr[paraPoint] = (char*)malloc(sizeof(char)*60);
    strcpy(paraArr[paraPoint],tmp);
    paraPoint++;
    //把ID以.addr的形式存入符号表
}

void _Stmtblock(struct treeNode* t) //statement block
{
    //we need entryDepth to decide whether to print {}
    if (!entryDepth)
    {
        printf("{\n");
        printf("entry:\n");
    }

    if (paraFlag)
    {
        //%x.addr = alloca i32, align 4
        //store i32 %x, i32* %x.addr, align 4
        int i=0;
        while (paraArr[i])
        {
            printf("  %%%s.addr = alloca i32, align 4\n",paraArr[i]);
            printf("  store i32 %%%s, i32* %%%s.addr, align 4\n",paraArr[i],paraArr[i]);
            free(paraArr[i]);
            i++;
        }
        paraFlag = 0;
        paraPoint = 0;
    }

    _Defs(t->child[1]);
    _Stmts(t->child[2]);

    if (!entryDepth) printf("}\n");
}

void _Defs(struct treeNode* t) //definitions
{
    if (t->child[1]==NULL) {}//epsilon
    else
    {
        _Def(t->child[0]);
        _Defs(t->child[1]);
    }
}

void _Def(struct treeNode* t) //definition
{
    _Decs(t->child[1]);
}

void _Decs(struct treeNode* t) //declaration
{
    if (t->child[1]==NULL) //DEC case
    {
        _DecInner(t->child[0]); //the DECs inside the function(local variables)
    }
    else //DEC COMMA DECS case
    {
        _DecInner(t->child[0]);
        _Decs(t->child[2]);
    }
}

void _DecInner(struct treeNode* t)
{
    //4 cases
    if (t->child[1]==NULL) //VAR case
    {
        struct treeNode* nodeVar = t->child[0]; //node of VAR
        if (nodeVar->child[1]==NULL) //case 1. a
        {
            //%a = alloca i32, align 4
            printf("  %%");
            struct treeNode* nodeId = nodeVar->child[0]->child[0];
            int len = strlen(nodeId->lable);

            char* tmp = (char*)malloc(sizeof(char)*60);
            int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
            int dim1 = tmp[0]-'a';
            if (dim1<0) dim1 = tmp[0]-'A';
            if (tmp[0]=='_') dim1 = 26;
            i=0;
            while (symTable[dim1][i]) i++;

            symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
            struct symbol* s = symTable[dim1][i];
            s->word = (char*)malloc(sizeof(char)*60);
            strcpy(s->word,tmp);
            s->type = 'l';

            printf("%s",tmp);
            printf(" = alloca i32, align 4\n");
        }
        else //case 3. c[2]
        {
            //%c = alloca [2 x i32], align 4
            printf("  %%");
            struct treeNode* nodeId = nodeVar->child[0]->child[0]->child[0];
            int len = strlen(nodeId->lable);

            char* tmp = (char*)malloc(sizeof(char)*60);
            int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
            int dim1 = tmp[0]-'a';
            if (dim1<0) dim1 = tmp[0]-'A';
            if (tmp[0]=='_') dim1 = 26;
            i=0;
            while (symTable[dim1][i]) i++;
            symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
            struct symbol* s = symTable[dim1][i];
            s->word = (char*)malloc(sizeof(char)*60);
            strcpy(s->word,tmp);
            s->type = 'l';

            printf("%s",tmp);
            printf(" = alloca [");
            struct treeNode* nodeInt = nodeVar->child[2];
            len = strlen(nodeInt->lable);
            for (i=5;i<len;i++) printf("%c",nodeInt->lable[i]);

            s->arrSize = (char*)malloc(sizeof(char)*60);
            for (i=5;i<=len;i++) s->arrSize[i-5] = nodeInt->lable[i];

            printf(" x i32], align 4\n");
        }
    }
    else
    {
        struct treeNode* nodeVar = t->child[0]; //node of VAR
        if (nodeVar->child[1]==NULL) //case 2. b = 1
        {
            //%b = alloca i32, align 4
            //store i32 1, i32* %b, align 4
            printf("  %%");
            struct treeNode* nodeId = nodeVar->child[0]->child[0];
            int len = strlen(nodeId->lable);

            char* tmp = (char*)malloc(sizeof(char)*60);
            int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
            int dim1 = tmp[0]-'a';
            if (dim1<0) dim1 = tmp[0]-'A';
            if (tmp[0]=='_') dim1 = 26;
            i=0;
            while (symTable[dim1][i]) i++;
            symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
            struct symbol* s = symTable[dim1][i];
            s->word = (char*)malloc(sizeof(char)*60);
            strcpy(s->word,tmp);
            s->type = 'l';

            printf("%s",tmp);

            char* tmp2 = (char*)malloc(sizeof(char)*60);
            struct treeNode* nodeInit = t->child[2]->child[0]->child[0];
            len = strlen(nodeInit->lable);
            for (i=5;i<=len;i++) tmp2[i-5] = nodeInit->lable[i];

            printf(" = alloca i32, align 4\n");
            printf("  store i32 %s, i32* %%%s, align 4\n",tmp2,tmp);
        }
        else //case 4, d[2] = {10,20}
        {
            //d[2]; d[0] = 10; d[1] = 20，可破回填
            //%d = alloca [2 x i32], align 4
            //%arrayidx = getelementptr inbounds [2 x i32]* %d, i32 0, i32 0
            //store i32 10, i32* %arrayidx, align 4
            //%arrayidx1 = getelementptr inbounds [2 x i32]* %d, i32 0, i32 1
            //store i32 20, i32* %arrayidx1, align 4
            printf("  %%");
            struct treeNode* nodeId = nodeVar->child[0]->child[0]->child[0];
            int len = strlen(nodeId->lable);

            char* tmp = (char*)malloc(sizeof(char)*60);
            int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];
            int dim1 = tmp[0]-'a';
            if (dim1<0) dim1 = tmp[0]-'A';
            if (tmp[0]=='_') dim1 = 26;
            i=0;
            while (symTable[dim1][i]) i++;
            symTable[dim1][i] = (struct symbol*)malloc(sizeof(struct symbol));
            struct symbol* s = symTable[dim1][i];
            s->word = (char*)malloc(sizeof(char)*60);
            strcpy(s->word,tmp);
            s->type = 'l';

            printf("%s",tmp);
            printf(" = alloca [");
            struct treeNode* nodeInt = nodeVar->child[2];
            len = strlen(nodeInt->lable);
            for (i=5;i<len;i++) printf("%c",nodeInt->lable[i]);

            s->arrSize = (char*)malloc(sizeof(char)*60);
            for (i=5;i<=len;i++) s->arrSize[i-5] = nodeInt->lable[i];

            printf(" x i32], align 4\n");

            arrName = (char*)malloc(sizeof(char)*60);
            arrSize = (char*)malloc(sizeof(char)*60);
            strcpy(arrName,tmp);
            strcpy(arrSize,s->arrSize);

            _ArgsInner(t->child[2]->child[1]);

            free(arrName);
            free(arrSize);
        }
    }
}

void _ArgsInner(struct treeNode* t) //arguments inner
{
    //%arrayidx = getelementptr inbounds [2 x i32]* %d, i32 0, i32 0
    //store i32 10, i32* %arrayidx, align 4
    if (t->child[1]==NULL) //EXP
    {
        char* val = (char*)malloc(sizeof(char)*60);
        val = _Exp(t->child[0]);
        printf("  %%arrayidx%d = getelementptr inbounds [%s x i32]* %%%s, i32 0, i32 %s\n",arridxNum,arrSize,arrName,val);
        printf("  store i32 %s, i32* %%arrayidx%d, align 4\n",val,arridxNum);
        arridxNum++;
    }
    else //EXP COMMA ARGS
    {
        char* val = (char*)malloc(sizeof(char)*60);
        val = _Exp(t->child[0]);
        printf("  %%arrayidx%d = getelementptr inbounds [%s x i32]* %%%s, i32 0, i32 %s\n",arridxNum,arrSize,arrName,val);
        printf("  store i32 %s, i32* %%arrayidx%d, align 4\n",val,arridxNum);
        arridxNum++;
        _ArgsInner(t->child[2]);
    }
}

void _Stmts(struct treeNode* t) //statements
{
    if (t->child[1]) //STMT STMTS
    {
        _Stmt(t->child[0]);
        _Stmts(t->child[1]);
    }
    else {} //epsilon
}

void _Stmt(struct treeNode* t) //statement
{
    if (t->child[1]==NULL) //STMTBLOCK
    {
        entryDepth++;
        _Stmtblock(t->child[0]);
        entryDepth--;
    }
    else if (t->child[0]->lable[0]=='E') //EXP SEMI
    {
        _Exp(t->child[0]);
    }
    else if (t->child[0]->lable[0]=='I') //IF
    {
        /*
        br i1 %cmp, label %if.then, label %if.else

        if.then:                                          ; preds = %entry
        store i32 1, i32* %a, align 4
        br label %if.end

        if.else:                                          ; preds = %entry
        store i32 2, i32* %a, align 4
        br label %if.end

        if.end:                                           ; preds = %if.else, %if.then
        store i32 3, i32* %a, align 4
        */
        if (t->child[5]->child[1]!=NULL) //ESTMT not null
        {
            char* tmp = (char*)malloc(sizeof(char)*60);
            tmp = _Exp(t->child[2]);


            if (!strcmp(t->child[2]->child[1]->lable,"DOT"))//DOT, special case
            {
                char num[10];
                sprintf(num, "%d", rNum++);
                char* tmpReg = (char*)malloc(sizeof(char)*60);
                strcpy(tmpReg,"%r");
                strcat(tmpReg,num);

                printf("  %s = icmp ne i32 %s, 0\n",tmpReg,tmp);
                strcpy(tmp,tmpReg);
            }


            printf("  br i1 %s, label %%if%d.then, label %%if%d.else\n\n",tmp, ifNum, ifNum);

            printf("if%d.then:\n",ifNum);
            _Stmt(t->child[4]);
            printf("  br label %%if%d.end\n\n",ifNum);

            printf("if%d.else:\n",ifNum);
            _Stmt(t->child[5]->child[1]);
            printf("  br label %%if%d.end\n\n",ifNum);

            printf("if%d.end:\n",ifNum);

            ifNum++;
        }
        else
        {
            char* tmp = (char*)malloc(sizeof(char)*60);
            tmp = _Exp(t->child[2]);


            if (!strcmp(t->child[2]->child[1]->lable,"DOT"))//DOT, special case
            {
                char num[10];
                sprintf(num, "%d", rNum++);
                char* tmpReg = (char*)malloc(sizeof(char)*60);
                strcpy(tmpReg,"%r");
                strcat(tmpReg,num);

                printf("  %s = icmp ne i32 %s, 0\n",tmpReg,tmp);
                strcpy(tmp,tmpReg);
            }


            printf("  br i1 %s, label %%if%d.then, label %%if%d.end\n\n",tmp, ifNum, ifNum);

            printf("if%d.then:\n",ifNum);
            _Stmt(t->child[4]);
            printf("  br label %%if%d.end\n\n",ifNum);

            printf("if%d.end:\n",ifNum);

            ifNum++;
        }
    }
    else if (t->child[0]->lable[0]=='R') //RETURN EXP SEMI
    {
        printf("  %%r%d = alloca i32, align 4\n",rNum);
        int oldrNum = rNum;
        rNum++;

        char* tmp = (char*)malloc(sizeof(char)*60);
        tmp = _Exp(t->child[1]);

        printf("  store i32 %s, i32* %%r%d\n",tmp,oldrNum);
        printf("  %%r%d = load i32* %%r%d\n",rNum,oldrNum);
        printf("  ret i32 %%r%d\n",rNum);
        rNum++;
    }
    else if (t->child[0]->lable[0]=='F') //FOR
    {
        //store i32 0, i32* %i, align 4
        //br label %for.cond
        _Exp(t->child[2]);
        printf("  br label %%for%d.cond\n\n",forNum);

        printf("for%d.cond:\n",forNum);
        char* tmp = (char*)malloc(sizeof(char)*60);
        tmp = _Exp(t->child[4]);

        //EXP->iNT will crash here!
        if (t->child[4]->child[0]->lable[0]=='T' && t->child[4]->child[1]->lable[0]=='A') //special case, ID ARRS
        {
            //%cmp = icmp sgt i32 %0, 16
            printf("  %%r%d = icmp sgt i32 %s, 0",rNum,tmp);
            printf("  br i1 %%r%d, label %%for%d.body, label %%for%d.end\n\n",rNum,forNum,forNum);
            rNum++;
        }
        else printf("  br i1 %s, label %%for%d.body, label %%for%d.end\n\n",tmp,forNum,forNum);

        printf("for%d.body:\n",forNum);
        _Stmt(t->child[8]);
        printf("  br label %%for%d.inc\n\n",forNum);

        printf("for%d.inc:\n",forNum);
        _Exp(t->child[6]);
        printf("  br label %%for%d.cond\n\n",forNum);

        printf("for%d.end:\n",forNum);

        forNum++;
    }
}

char* _Exp(struct treeNode* t)
{
    if (t->child[1]==NULL && t->child[0]->lable[0]=='I') //EXP->INT
    {
        char* tmp = (char*)malloc(sizeof(char)*60);
        struct treeNode* nodeInt = t->child[0];
        int len = strlen(nodeInt->lable);
        int i; for (i=5;i<=len;i++) tmp[i-5] = nodeInt->lable[i];

        return tmp;
    }
    else if (t->child[1]==NULL && t->child[0]->lable[0]=='N') //EXP->NEGINT
    {
        char* tmp = (char*)malloc(sizeof(char)*60);
        struct treeNode* nodeInt = t->child[0];
        int len = strlen(nodeInt->lable);
        int i; for (i=8;i<=len;i++) tmp[i-8] = nodeInt->lable[i];

        return tmp;
    }
    else if (!strcmp(t->child[0]->lable,"WR")) //EXP->WR
    {
        struct treeNode* nodeWr = t->child[0];
        if (nodeWr->child[0]->lable[0]=='W') //WRITE case
        {
            char* tmp = (char*)malloc(sizeof(char)*60);
            tmp = _Exp(nodeWr->child[2]);

            int trans;
            if (strlen(tmp)>1 && (tmp[0]=='0' || (tmp[0]=='-' && tmp[1]=='0')))
            {
                trans = strtol(tmp,NULL,0);
                printf("  %%call%d = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32 %d)\n",callNum,trans);
                callNum++;
                printf("  %%call%d = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str1, i32 0, i32 0))\n",callNum);
                callNum++;
            }
            else
            {
                printf("  %%call%d = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32 %s)\n",callNum,tmp);
                callNum++;
                printf("  %%call%d = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str1, i32 0, i32 0))\n",callNum);
                callNum++;
            }
        }
        else //READ case
        {
            //%call = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32* %a)
            char* tmp = (char*)malloc(sizeof(char)*200);
            loadFlag = 0;
            tmp = _Exp(nodeWr->child[2]);
            loadFlag = 1;

            printf("  %%call%d = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32* %s)\n",callNum,tmp);
            callNum++;
        }
        return NULL;
    }
    else if (t->child[0]->lable[0]=='T' && t->child[1]->lable[0]=='A') //EXP->THEID ARRS
    {
        //printf("%s, %c",symTable[0][0]->word,symTable[0][0]->type);
        //return symTable[0][0]->word;

        struct treeNode* nodeArrs = t->child[1];
        if (nodeArrs->child[0]->lable[0]=='N') //ARRS->NULL, ID case
        {
            char* tmp = (char*)malloc(sizeof(char)*60);
            struct treeNode* nodeId = t->child[0]->child[0];
            int len = strlen(nodeId->lable);
            int i; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

            int index = tmp[0]-'a';
            if (index<0) index = tmp[0]-'A';
            if (tmp[0]=='_') index = 26;

            i=0;
            while (strcmp(tmp,symTable[index][i]->word)) i++;

            struct symbol* id = symTable[index][i];
            switch (id->type)
            {
                case 'g':
                for (i=strlen(tmp);i>=0;i--) tmp[i+1] = tmp[i];
                tmp[0] = '@';
                break;

                case 'l':
                for (i=strlen(tmp);i>=0;i--) tmp[i+1] = tmp[i];
                tmp[0] = '%';
                break;

                case 'a':
                for (i=strlen(tmp);i>=0;i--) tmp[i+1] = tmp[i];
                tmp[0] = '%';
                strcat(tmp,".addr");
                break;
            }

            if (loadFlag)
            {
                char num[10];
                sprintf(num, "%d", rNum++);
                char* tmpReg = (char*)malloc(sizeof(char)*60);
                strcpy(tmpReg,"%r");
                strcat(tmpReg,num);

                printf("  %s = load i32* %s, align 4\n",tmpReg,tmp);
                return tmpReg;
            }
            else return tmp;
        }
        else //we need to return arrindex
        {
            char* tmp = (char*)malloc(sizeof(char)*60);
            struct treeNode* nodeId = t->child[0]->child[0];
            int len = strlen(nodeId->lable);
            int i; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

            char* arrsIndex = (char*)malloc(sizeof(char)*60);
            if (loadFlag==0)
            {
                loadFlag = 1;
                arrsIndex = _Exp(t->child[1]->child[1]); //what we obtained could be register or INT
                loadFlag = 0;
            }
            else arrsIndex = _Exp(t->child[1]->child[1]);

            char* ret = (char*)malloc(sizeof(char)*60);
            strcpy(ret,"%arrayidx");

            char num[10];
            sprintf(num, "%d", arridxNum++);
            strcat(ret,num);

            int index = tmp[0]-'a';
            if (index<0) index = tmp[0]-'A';
            if (tmp[0]=='_') index = 26;

            i=0;
            while (strcmp(tmp,symTable[index][i]->word)) i++;

            struct symbol* id = symTable[index][i];
            switch (id->type)
            {
                case 'g':
                for (i=strlen(tmp);i>=0;i--) tmp[i+1] = tmp[i];
                tmp[0] = '@';
                break;

                case 'l':
                for (i=strlen(tmp);i>=0;i--) tmp[i+1] = tmp[i];
                tmp[0] = '%';
                break;

                case 'a':
                for (i=strlen(tmp);i>=0;i--) tmp[i+1] = tmp[i];
                tmp[0] = '%';
                strcat(tmp,".addr");
                break;
            }

            //%arrayidx4 = getelementptr inbounds [2 x i32]* %d, i32 0, i32 1
            printf("  %s = getelementptr inbounds [%s x i32]* %s, i32 0, i32 %s\n",ret,id->arrSize,tmp,arrsIndex);

            if (loadFlag)
            {
                char num[10];
                sprintf(num, "%d", rNum++);
                char* tmpReg = (char*)malloc(sizeof(char)*60);
                strcpy(tmpReg,"%r");
                strcat(tmpReg,num);

                printf("  %s = load i32* %s, align 4\n",tmpReg,ret);
                return tmpReg;
            }
            else return ret;
        }
    }
    else if (!strcmp(t->child[0]->lable,"INCRE")) //++
    {
        //%27 = load i32* %i, align 4
        //%inc26 = add nsw i32 %27, 1
        //store i32 %inc26, i32* %i, align 4
        char* op = (char*)malloc(sizeof(char)*60);
        loadFlag = 0;
        op = _Exp(t->child[1]);
        loadFlag = 1;

        printf("  %%r%d = load i32* %s, align 4\n",rNum,op);
        printf("  %%r%d = add nsw i32 %%r%d, 1\n",rNum+1,rNum);
        printf("  store i32 %%r%d, i32* %s, align 4\n",rNum+1,op);

        rNum+=2;
        return NULL;
    }
    else if (!strcmp(t->child[0]->lable,"UMINUS")) //-
    {
        //%27 = load i32* %i, align 4
        //%inc26 = add nsw i32 %27, 1
        //store i32 %inc26, i32* %i, align 4
        char* op = (char*)malloc(sizeof(char)*60);
        loadFlag = 0;
        op = _Exp(t->child[1]);
        loadFlag = 1;

        printf("  %%r%d = load i32* %s, align 4\n",rNum,op);
        printf("  %%r%d = sub nsw i32 0, %%r%d\n",rNum+1,rNum);
        printf("  store i32 %%r%d, i32* %s, align 4\n",rNum+1,op);

        char num[10];
        sprintf(num, "%d", rNum+1);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        rNum+=2;
        return tmpReg;
    }
    else if (!strcmp(t->child[0]->lable,"LOGICN")) //!
    {
        //%tobool = icmp eq i32 %0, 0
        char* op = (char*)malloc(sizeof(char)*60);
        op = _Exp(t->child[1]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = icmp eq i32 %s, 0\n",tmpReg,op);
        return tmpReg;
    }
    else if (t->child[1]->lable[0]=='N' && t->child[1]->lable[1]=='E' && t->child[1]->lable[2]=='G') //EXP -> EXP NEGINT
    {
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);

        char* op2 = (char*)malloc(sizeof(char)*60);
        struct treeNode* nodeInt = t->child[1];
        int len = strlen(nodeInt->lable);
        int i; for (i=9;i<=len;i++) op2[i-9] = nodeInt->lable[i];

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = sub nsw i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"ASSIGNOP")) //EXP->EXP ASSIGNOP EXP
    {
        char* op2 = (char*)malloc(sizeof(char)*200);
        op2 = _Exp(t->child[2]);

        loadFlag = 0;
        char* op1 = (char*)malloc(sizeof(char)*200);
        op1 = _Exp(t->child[0]);
        loadFlag = 1;

        printf("  store i32 %s, i32* %s, align 4\n",op2,op1);
        return NULL;
    }
    else if (!strcmp(t->child[0]->lable,"LP")) //LP EXP RP
    {
        return _Exp(t->child[1]);
    }
    else if (!strcmp(t->child[1]->lable,"DOT")) ////EXP->EXP DOT THEID
    {
        //%0 = load i32* getelementptr inbounds (%struct.doubleO* @T, i32 0, i32 0), align 4
        struct treeNode* nodeId = t->child[0]->child[0]->child[0];
        char* tmp = (char*)malloc(sizeof(char)*200);
        int len = strlen(nodeId->lable);
        int i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

        int index = tmp[0]-'a';
        if (index<0) index = tmp[0]-'A';
        if (tmp[0]=='_') index = 26;

        i=0;
        while (strcmp(tmp,symTable[index][i]->word)) i++;

        struct symbol* id = symTable[index][i];

        char* op1 = (char*)malloc(sizeof(char)*200);
        strcpy(op1,tmp);

        char* opStr = (char*)malloc(sizeof(char)*200);
        strcpy(opStr,id->structName); //opStr, doubleO

        free(tmp);

        nodeId = t->child[2]->child[0];
        tmp = (char*)malloc(sizeof(char)*200);
        len = strlen(nodeId->lable);
        i=4; for (i=4;i<=len;i++) tmp[i-4] = nodeId->lable[i];

        index = tmp[0]-'a';
        if (index<0) index = tmp[0]-'A';
        if (tmp[0]=='_') index = 26;

        i=0;
        while (strcmp(tmp,symTable[index][i]->word)) i++;

        id = symTable[index][i];

        int op2 = id->structMem; //op2, 0

        char* ret = (char*)malloc(sizeof(char)*200);
        strcpy(ret,"getelementptr inbounds (%struct.");
        strcat(ret,opStr);
        strcat(ret,"* @");
        strcat(ret,op1);
        strcat(ret,", i32 0, i32 ");
        char indTmp = '0'+op2;
        char* ind = (char*)malloc(sizeof(char)*50); ind[0] = indTmp; ind[1] = '\0';
        strcat(ret,ind);
        strcat(ret,")");

        if (loadFlag)
        {
            char num[10];
            sprintf(num, "%d", rNum++);
            char* tmpReg = (char*)malloc(sizeof(char)*200);
            strcpy(tmpReg,"%r");
            strcat(tmpReg,num);

            printf("  %s = load i32* %s, align 4\n",tmpReg,ret);
            return tmpReg;
        }
        else return ret;
    }
    else if (!strcmp(t->child[1]->lable,"EQUAL")) //EXP->EXP EQUAL EXP
    {
        //%cmp = icmp eq i32 %0, %1
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = icmp eq i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"GREATER")) //EXP->EXP GREATER EXP
    {
        //%cmp = icmp sgt i32 %0, 16
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = icmp sgt i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"LESS")) //EXP->EXP LESS EXP
    {
        //%cmp = icmp sgt i32 %0, 16
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = icmp slt i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"LOGICAND")) //EXP->EXP LOGICAND EXP
    {
        //%cmp = icmp eq i32 %0, %1
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        int reg1 = rNum, reg2 = rNum+1; rNum+=2;
        printf("  %%r%d = icmp ne i1 %s, 0\n",reg1,op1);
        printf("  %%r%d = icmp ne i1 %s, 0\n",reg2,op2);

        int reg3 = rNum; rNum++;
        printf("  %%r%d = and i1 %%r%d, %%r%d\n",reg3,reg1,reg2);

        char num[10];
        sprintf(num, "%d", reg3);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"ADD")) //EXP ADD EXP
    {
        //%0 = load i32* %a, align 4
        //%1 = load i32* %b, align 4
        //%add = add nsw i32 %0, %1
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = add nsw i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"MINUS")) //EXP MINUS EXP
    {
        //%0 = load i32* %a, align 4
        //%1 = load i32* %b, align 4
        //%add = add nsw i32 %0, %1
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = sub nsw i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"MULT")) //EXP MULT EXP
    {
        //%0 = load i32* %a, align 4
        //%1 = load i32* %b, align 4
        //%add = add nsw i32 %0, %1
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = mul nsw i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"MOD"))//MOD srem
    {
        //%0 = load i32* %a, align 4
        //%1 = load i32* %b, align 4
        //%add = add nsw i32 %0, %1
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = srem i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"BITAND"))//BITAND
    {
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = and i32 %s, %s\n",tmpReg,op1,op2);
        //这里最后返回了一个i1,实质上是不合理的，不过无所谓吧
        sprintf(num, "%d", rNum++);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = icmp ne i32 %%r%d, 0\n",tmpReg,rNum-2);
        return  tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"BITXOR"))//BITXOR
    {
        char* op1 = (char*)malloc(sizeof(char)*60);
        op1 = _Exp(t->child[0]);
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        char num[10];
        sprintf(num, "%d", rNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%r");
        strcat(tmpReg,num);

        printf("  %s = xor i32 %s, %s\n",tmpReg,op1,op2);
        return tmpReg;
    }
    else if (!strcmp(t->child[1]->lable,"SHIFTRA")) //EXP SHIFTRA EXP
    {
        //%0 = load i32* %x, align 4
        //%shr = ashr i32 %0, 1
        //store i32 %shr, i32* %x, align 4
        char* op1 = (char*)malloc(sizeof(char)*60);
        loadFlag = 0;
        op1 = _Exp(t->child[0]);
        loadFlag = 1;
        char* op2 = (char*)malloc(sizeof(char)*60);
        op2 = _Exp(t->child[2]);

        printf("%%r%d = load i32* %s, align 4\n",rNum,op1);
        printf("  %%r%d = ashr i32 %%r%d, %s\n",rNum+1,rNum,op2);
        printf("  store i32 %%r%d, i32* %s, align 4\n",rNum+1,op1);
        rNum+=2;
        return NULL;
    }
    else if (!strcmp(t->child[2]->lable,"ARGS")) //ID LP ARGS RP
    {
        //%0 = load i32* @a, align 4
        //%1 = load i32* @b, align 4
        //%call2 = call i32 @gcd(i32 %0, i32 %1)
        _ArgsFunc(t->child[2]);

        char num[10];
        sprintf(num, "%d", callNum++);
        char* tmpReg = (char*)malloc(sizeof(char)*60);
        strcpy(tmpReg,"%call");
        strcat(tmpReg,num);

        char* funcName = (char*)malloc(sizeof(char)*60);
        struct treeNode* nodeId = t->child[0]->child[0];
        int len = strlen(nodeId->lable);
        int i; for (i=4;i<=len;i++) funcName[i-4] = nodeId->lable[i];

        printf("  %s = call i32 @%s(",tmpReg,funcName);
        for (i=0;i<paraPoint-1;i++)
        {
            printf("i32 %s, ",paraArr[i]);
            free(paraArr[i]);
        }
        if (paraPoint>0)
        {
            printf("i32 %s",paraArr[paraPoint-1]);
            free(paraArr[i]);
            paraPoint = 0;
        }
        printf(")\n");

        return tmpReg;
    }
    else return NULL;
}

void _ArgsFunc(struct treeNode* t) //ARGS for function call
{
    if (t->child[1]==NULL) //EXP
    {
        char* tmp = (char*)malloc(sizeof(char)*60);
        tmp = _Exp(t->child[0]);
        paraArr[paraPoint] = (char*)malloc(sizeof(char)*60);
        strcpy(paraArr[paraPoint],tmp);
        paraPoint++;
    }
    else //EXP COMMA ARGS
    {
        char* tmp = (char*)malloc(sizeof(char)*60);
        tmp = _Exp(t->child[0]);
        paraArr[paraPoint] = (char*)malloc(sizeof(char)*60);
        strcpy(paraArr[paraPoint],tmp);
        paraPoint++;

        _ArgsFunc(t->child[2]);
    }
}

yyerror(s)
char *s;
{
	warning(s,(char*)0);
	yyparse();
}

warning(s,t)
char *s, *t;
{
	fprintf(stderr,"%s\n",s);
	if (t)
	fprintf(stderr," %s\n",t);
}
