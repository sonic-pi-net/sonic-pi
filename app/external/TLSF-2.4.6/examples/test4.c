#include <stdio.h>
#include <stdlib.h>
#include "tlsf.h"

int main(void){ 
  int *ptr[100];
  int i;

  for (i=0; i< 100; i++)
    if (!(ptr[i]=tlsf_malloc(1024))){
      printf("Error\n");
      exit(-1);
    }

  for (i=0; i< 100; i++)
    tlsf_free(ptr[i]);

  printf("Test OK\n");

  exit(0);
}

    
	
