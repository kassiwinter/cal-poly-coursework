#include <stdio.h>

#ifndef BLOCKSIZE

#define BLOCKSIZE 256

int openDisk(char *filename, int nBytes);
int closeDisk(int disk);
int readBlock(int disk, int bNum, void *block);
int writeBlock(int disk, int bNum, void *block);



typedef struct DiskInfo DiskInfo;

struct DiskInfo {
    int disk_num;
    FILE *fp;
    DiskInfo *prev;
    DiskInfo *next;
};

void addToOpenList(DiskInfo *disk);
DiskInfo* removeFromOpenList(int disknum);
DiskInfo* findDisk(int disknum);
#endif