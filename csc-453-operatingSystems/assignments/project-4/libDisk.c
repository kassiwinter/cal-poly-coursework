#include <stdio.h>
#include <stdlib.h>

#include "libDisk.h"

DiskInfo *open_disk_list = NULL;
int disk_num_counter = 1;

// opens a regular UNIX file and designates the first nBytes of it as space for the emulated disk.
int openDisk(char *filename, int nBytes)
{

    // can't have disk smaller than BLOCKSIZE (256)
    if (nBytes < BLOCKSIZE && nBytes)
    {
        return -1;
    }

    FILE *disk_fp = NULL;

    // if nbytes is 0, open existing disk as read, otherwise open and truncate
    disk_fp = nBytes == 0 ? fopen(filename, "r+") : fopen(filename, "w+");

    // check if open succeeded
    if (!disk_fp)
    {
        return -1;
    }

    // create disk entry for opened disk
    DiskInfo *newdisk = malloc(sizeof(DiskInfo));
    newdisk->disk_num = disk_num_counter++;
    newdisk->fp = disk_fp;

    // add disk entry to open disk list
    addToOpenList(newdisk);

    // return disk number
    return newdisk->disk_num;
}

// closes a disk that is in the open list
int closeDisk(int disk)
{

    // grab disk from open list
    DiskInfo *victim = removeFromOpenList(disk);

    // check that disk was even in open list in the first place
    if (!victim)
    {
        // can't close disk that isn't open
        return -1;
    }

    // close disk file
    if (fclose(victim->fp) == EOF)
    {
        // failed closing
        return -1;
    };

    // free malloc'd diskinfo struct
    free(victim);
    return 0;
}

// reads from a given disk into a buffer
int readBlock(int disk, int bNum, void *block)
{
    // grab disk to read from from open disk list
    DiskInfo *read_disk = findDisk(disk);

    // check that disk exists
    if (!read_disk)
    {
        return -1;
    }

    // move fp head to where we want to read from
    fseek(read_disk->fp, bNum * BLOCKSIZE, SEEK_SET);

    // read block from disk file
    if (fread(block, 1, BLOCKSIZE, read_disk->fp) != BLOCKSIZE)
    {
        return -1;
    }

    return 0;
}

// writes from a buffer onto the disk
int writeBlock(int disk, int bNum, void *block)
{

    // grab disk to read from from open disk list
    DiskInfo *write_disk = findDisk(disk);

    // check that disk exists
    if (!write_disk)
    {
        return -1;
    }

    // seek to where we want to write
    fseek(write_disk->fp, bNum * BLOCKSIZE, SEEK_SET);

    // write block to disk file
    if (fwrite(block, 1, BLOCKSIZE, write_disk->fp) != BLOCKSIZE)
    {
        return -1;
    }
    return 0;
}

// helper functions

// adds disk to list of open files
void addToOpenList(DiskInfo *disk)
{
    // check if list is empty
    if (!open_disk_list)
    {
        disk->prev = disk;
        disk->next = disk;
        open_disk_list = disk;
    }
    else
    {
        disk->prev = open_disk_list->prev;
        open_disk_list->prev->next = disk;
        disk->next = open_disk_list;
        open_disk_list->prev = disk;
    }
    return;
}

// removes disk from list of open files
DiskInfo *removeFromOpenList(int disknum)
{
    // if open list is empty
    if (!open_disk_list)
    {
        return NULL;
    }

    DiskInfo *disk = findDisk(disknum);

    // if removing only disk in open list
    if (disk->next == disk)
    {
        open_disk_list = NULL;
        return disk;
    }

    open_disk_list = disk->next; // head of open list doesn't matter, since it doesn't need to be ordered, just make sure it's not the one that's removed

    // reconnect open list without victim
    disk->prev->next = disk->next;
    disk->next->prev = disk->prev;
    return disk;
}

// finds disk in list of open files
DiskInfo *findDisk(int disknum)
{
    DiskInfo *disk = open_disk_list;
    while (disk->disk_num != disknum)
    {
        disk = disk->next;

        // full loop through list
        if (disk == open_disk_list)
        {
            return NULL;
        }
    }
    return disk;
}
