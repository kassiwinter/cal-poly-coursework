CC = gcc
CFLAGS = -Wall -g
PROG = tinyFSDemo
OBJS = tinyFSDemo.o libTinyFS.o libDisk.o

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS)

diskDemo:  libDisk.h diskTest.c libDisk.c 
	$(CC) $(CFLAGS) -o $@ $<

tinyFsDemo.o: tinyFSDemo.c libTinyFS.h tinyFS.h TinyFS_errno.h
	$(CC) $(CFLAGS) -c -o $@ $<

libTinyFS.o: libTinyFS.c libTinyFS.h tinyFS.h libDisk.h libDisk.o TinyFS_errno.h
	$(CC) $(CFLAGS) -c -o $@ $<

libDisk.o: libDisk.c libDisk.h tinyFS.h TinyFS_errno.h
	$(CC) $(CFLAGS) -c -o $@ $<