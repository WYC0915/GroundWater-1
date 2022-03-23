
/* Return:
     0 -> does not exist
     1 -> regular file
     2 -> dir
     3 -> unknown
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#define TRUE 1
#define FALSE 0

int iftype_(char *fil) {
    struct stat buf;
    int i,j,iret;
    int slash=FALSE;

    /*
     turn first blank into a null
    */
    j=0;
    while (fil[j] != ' ') j++;
    fil[j]='\0';
    if ((j>0) && (fil[j-1]=='/')) {
        fil[j-1]='\0';
        slash=TRUE;
    }

    i = stat(fil,&buf);
    if (i != 0) {
        iret=0;
    } else {
        if (S_ISDIR(buf.st_mode)) {
            iret=2;
        } else if (S_ISCHR(buf.st_mode)) {
            iret=3;
        } else if (S_ISBLK(buf.st_mode)) {
            iret=3;
        } else if (S_ISREG(buf.st_mode)) {
            iret=1;
        } else if (S_ISFIFO(buf.st_mode)) {
            iret=3;
        } else {
            iret=3;
        }
    }
    fil[j]=' ';
    if (slash) {
        fil[j-1]='/';
    }
    return(iret);
}

int iftype(char *fil) {
    return iftype_(fil);
}
