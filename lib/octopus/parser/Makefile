LEX     = flex --header-file=flex.h -P octyy_
YACC    = bison
YFLAGS  = -d -y -p octyy_
CFLAGS = -DTEST_PARSER -std=c99 
objects = parse.o scan.o y.tab.h test_parser.o ast.o

test_parser: $(objects)
parse.o: parse.c parse.y
scan.o: scan.c scan.l
flex.h: scan.o
y.tab.h: parse.c scan.c scan.o
test_parser.o: test_parser.c ast.c y.tab.h flex.h

clean:
	rm -f $(objects) parse.c scan.c test_parser flex.h


