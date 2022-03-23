
#include <stdlib.h>

void syscmd_(char* string, int* lgth) {
  int ii;
  char* local_string = (char*) malloc( (*lgth+1)*sizeof(char) ) ;

  for (ii=0; ii<*lgth; ii++) local_string[ii] = string[ii] ;

  local_string[*lgth] = '\0';

  system(local_string);

  free(local_string);
}

void syscmd(char* string, int* lgth) {
  syscmd_(string, lgth);
}

