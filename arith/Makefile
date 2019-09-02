tudo: mcalc direto

mcalc: mcalc.tab.o lex.yy.o main.o 
	gcc -o $@ $^  -lfl

mcalc.tab.o: mcalc.y
	bison -d mcalc.y
	gcc -c mcalc.tab.c

lex.yy.o: mcalc.l
	flex mcalc.l
	gcc -c lex.yy.c

direto: direto.rkt
	raco exe $<

clean:
	rm -f *.o lex.yy.c mcalc.tab.c mcalc.tab.h direto *~
