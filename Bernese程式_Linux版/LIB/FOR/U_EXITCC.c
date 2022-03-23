
/*
 *  Subroutine exitrc (Unix Version)
 *
 *  Author:   L. Mervart
 *
 *  Created:  11-Sep-95
 *
 */

#include <stdlib.h>

int exitcc_(int* ircode) {
  if (*ircode == 0 || *ircode == 1) {
    exit(0);
  }
  else {
    exit(1);
  }
}

int exitcc(int* ircode) {
  return exitcc_(ircode);
}
