CC = gcc
CFLAGS = -std=c99 -O2

all: replay-master replay-slave

replay-master: master.o
	$(CC) -o $@ $<

replay-slave: slave.o
	$(CC) -o $@ $<

master.o: master.c
slave.o: slave.c
