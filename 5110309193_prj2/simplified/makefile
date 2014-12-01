scc: lex.yy.o smallC.tab.o
	gcc lex.yy.o smallC.tab.o -o scc
smallC.tab.o: smallC.tab.c 
	gcc -c smallC.tab.c
lex.yy.o: lex.yy.c smallC.tab.h
	gcc -c lex.yy.c
smallC.tab.c smallC.tab.h: smallC.y
	bison -d smallC.y
lex.yy.c: smallC.l
	lex smallC.l
clean:
	rm lex.yy.o smallC.tab.o smallC.tab.c lex.yy.c smallC.tab.h

