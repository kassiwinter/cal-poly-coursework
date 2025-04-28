#include "libTinyFS.h"
#include "libDisk.h"
#include <string.h>

typedef struct drt_entry {
    fileDescriptor fd; // # we generate so no two files can be the same (allows opening multiple of the same file)
    int block; // block number of file on disk
    int file_pointer; // position of where we are in the file (initially at start of data)
    drt_entry *next;
    drt_entry *prev;
} drt_entry; 

drt_entry *drt = NULL; // Dynamic Resource Table: used to store open files

#define SUPERBLOCK 1 // there is only one of these bad boys
#define INODE 2
#define FILE_EXTENT 3 // contains the data of the file itself
#define FREE 4

#define TYPE_FIELD 0 // contains file type above (superblock, inode, file-extent, or free)
#define MAGIC_FIELD 1 // contains magic #
#define LINK_FIELD 2  // #1 links data together in separate file in FILE_EXTENT files; #2 links to open spaces in SUPERBLOCK file -- limited to 1 byte, which limits us to store up to the number 256
#define BLANK_FIELD 3  // acts as a divider between beginning info & data after
#define DATA_START 4  // index in the block where the actual data starts

// JACOB'S OPTIMIZATION SILLINESS EXPLAINED IN KASSI NOTE: because each block contains the first 4, the blocks are limited to 252 bytes
// so in order to maximize our space we can use the rest of the bits avalible in the SuperBlock
// to create a 'bit map' that keeps track of the free/taken blocks (expanding our file system size to 2,016)
// -> it is important to note that in this situation there is no use of LINK_FIELD (unless you are feeling extra spicy.. in which, insert rabbit hole here)

#define MAGIC 0x44

#define MAX_BLOCK_STORE 256

int fd_counter = 1; // incrementing our global fd

int mounted = 0; // NOTE: 0 (no file system/disk mounted) or a # (file in use, # is the disk number itself)
int mounted_size = 0;

// This function it to make a File System: Initializing the whole system (set up superblock, set the rest to free)
int tfs_mkfs(char *filename, int nBytes){
    // A. make disk itself
    int fs = openDisk(filename, nBytes);

    int block_count = nBytes / BLOCKSIZE + (BLOCKSIZE % nBytes ? 0 : 1);

    unsigned char *block = malloc(BLOCKSIZE * sizeof(unsigned char));

    block[TYPE_FIELD] = (unsigned char)FREE;
    block[MAGIC_FIELD] = (unsigned char)MAGIC;
    block[BLANK_FIELD] = (unsigned char)NULL;

    int i;
    // B. initialize system
    for (i = 0; i < BLOCKSIZE; i++){
            block[i] = NULL; // creating/making all blocks free (NULL); clearing the slate
    }

    // link all of the created/made free blocks together
    for (i = 0; i < block_count; i++)
    {
        block[LINK_FIELD] = (unsigned char)i + 1 < BLOCKSIZE ? i + 1 : 0; // link
        if (writeBlock(fs, i, block) < 0){ // writing the first 4 bytes of the block (type, magic, link, blank)
            return -1;
        }
    }

    // C. make superblock 
    block[TYPE_FIELD] = (unsigned char)SUPERBLOCK;
    block[LINK_FIELD] = (unsigned char)1; // link to first free block
    block[DATA_START] = (unsigned char)block_count; // used to keep track of how big the file system is
    if (writeBlock(fs, 0, block) < 0){
        return -1;
    };

    free(block); // because we are just writing it to the file, we are not actually using the block

    return 0;
}

// This function is when you want to access a given file system on a given disk
int tfs_mount(char *diskname)
{
    if (mounted){ // is another filesystem already mounted / disk currently opened?
        return -1;
    }
    mounted = openDisk(diskname, 0);

    unsigned char *block = malloc(BLOCKSIZE * sizeof(char));

    readBlock(mounted, 0, block); // read superblock

    int system_size = mounted_size = block[DATA_START]; // grab system size (know how many blocks the user can go through)
    int magic = block[MAGIC_FIELD]; // grab magic number from superblock (how to check to make sure the fields are the same)
    if (magic != MAGIC){
        return -1; // this will occur when the disk you are opening is not formmated correctly (magic number incorrect)
    }

    // verify the magic number in the rest of the blocks
    int i;
    for (i = 1; i < system_size; i++){ // go through all of the blocks, make sure the magic # is the same in all of them
        readBlock(mounted, i, block);
        magic = block[MAGIC_FIELD];
        if (magic != MAGIC){
            return -1;
        }
    }
    free(block);

    return 0;     // disk is verified, feel free to fuck around with it from here
}

// This function is when you want to close a given file system on a given disk
int tfs_unmount(void)
{
    if (closeDisk(mounted) < 0){
        // close disk failed
        return -1;
    }
    mounted = 0;
    return 0;
}



/* Creates or Opens a file for reading and writing on the currently
mounted file system. Creates a dynamic resource table entry for the file,
and returns a file descriptor (integer) that can be used to reference
this entry while the filesystem is mounted. */
fileDescriptor tfs_openFile(char *name){
    
    int i;
    unsigned char *block = malloc(BLOCKSIZE * sizeof(char));

    for (i = 1; i < mounted_size; i++){ // traverse through blocks, check to see if it is an inode
        readBlock(mounted, i, block);
        if (block[TYPE_FIELD] == INODE){ // see if the file name provided matches the file name of the given inode

            if (!strcmp(block[DATA_START], name)){  // WE GOT THE FILE WOOOOOOO
                drt_entry *newentry = malloc(sizeof(drt_entry));
                newentry->fd = fd_counter;
                newentry->block = i;
                newentry->file_pointer = 0;
                fd_counter++;
                addToDRT(newentry);
            }
        }
    }


    // If not found in the inodes
    //  .. set the first block (found in superblock) and make an inode for the file
    //  .. update the first free block in the super block to be the next one
     







}

int tfs_closeFile(fileDescriptor FD);
/* Closes the file, de-allocates all system resources, and removes table
entry */
int tfs_writeFile(fileDescriptor FD, char *buffer, int size);
/* Writes buffer ‘buffer’ of size ‘size’, which represents an entire
file’s content, to the file system. Previous content (if any) will be
completely lost. Sets the file pointer to 0 (the start of file) when
done. Returns success/error codes. */
int tfs_deleteFile(fileDescriptor FD);
/* deletes a file and marks its blocks as free on disk. */
int tfs_readByte(fileDescriptor FD, char *buffer);
/* reads one byte from the file and copies it to buffer, using the
current file pointer location and incrementing it by one upon success.
If the file pointer is already past the end of the file then
tfs_readByte() should return an error and not increment the file pointer.
*/
int tfs_seek(fileDescriptor FD, int offset);
/* change the file pointer location to offset (absolute). Returns
success/error codes.*/


// HELPER FUNCTIONS
// A. Adds entry to DRT
void addToDRT(drt_entry *entry)
{
    // check if list is empty
    if (!drt)
    {
        entry->prev = entry;
        entry->next = entry;
        drt = entry;
    }
    else
    {
        entry->prev = drt->prev;
        drt->prev->next = entry;
        entry->next = drt;
        drt->prev = entry;
    }
    return;
}

// B. removes entry from DRT
drt_entry *removeFromDRT(fileDescriptor fd)
{
    // if drt list is empty
    if (!drt)
    {
        return NULL;
    }

    drt_entry *entry = findDisk(fd);

    // if removing only drt entry in list
    if (entry->next == entry)
    {
        drt = NULL;
        return entry;
    }

    drt = entry->next; // head of list doesn't matter, since it doesn't need to be ordered, just make sure it's not the one that's removed

    // reconnect open list without victim
    entry->prev->next = entry->next;
    entry->next->prev = entry->prev;
    return entry;
}

// C. finds entry in DRT
drt_entry *findDisk(fileDescriptor fd)
{
    drt_entry *entry = drt;
    while (entry->fd != fd)
    {
        entry = entry->next;

        // full loop through list
        if (entry == drt)
        {
            return NULL;
        }
    }
    return entry;
}
