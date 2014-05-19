# Linux Makefile for libtenaciousd.

CFLAGS = -std=gnu99 -g
CPPFLAGS = -I.

#libtenaciousd.a: log.o aio_vsic.o ram_vsic.o
libtenaciousd.a: log.o ram_vsic.o
	rm -f $@
	$(AR) rcs $@ $^

log.o: log.c
aio_vsic.o: aio_vsic.c
ram_vsic.o: ram_vsic.c

clean:
	rm -f *.o *.a
