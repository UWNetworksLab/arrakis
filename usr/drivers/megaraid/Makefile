CPPFLAGS = -I.
CFLAGS = -std=gnu99 -g -Wall -DLIBRARY -O2 -fPIC
#CFLAGS = -std=gnu99 -g -Wall -DLIBRARY -DMEGARAID_DEBUG -fPIC

all: megaraid libmegaraid.a libmegaraid.so test

test: test.o

test.o: test.c

megaraid: megaraid.o megaraid_vsic.o main.o

libmegaraid.a: megaraid.o megaraid_vsic.o main.o
	rm -f $@
	$(AR) rcs $@ $^

libmegaraid.so: megaraid.o megaraid_vsic.o main.o vsic_vfs.o
	$(CC) -shared -fPIC -o $@ $^ -ldl

megaraid.o: megaraid.c megaraid.h
megaraid_vsic.o: megaraid_vsic.c megaraid.h
main.o: main.c
vsic_vfs.o: vsic_vfs.c

clean:
	rm -f *.o *.a megaraid
