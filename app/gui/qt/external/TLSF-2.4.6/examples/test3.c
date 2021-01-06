#include <stdio.h>
#include <stdlib.h>
#include "tlsf.h"

#define POOL_SIZE 1024 * 10


// Pool size is in bytes.
char p0[POOL_SIZE];
char p1[POOL_SIZE];
char p2[POOL_SIZE];
char p3[POOL_SIZE];

int main(void){
  char *ptr;
  
  printf("Pools' addresses: (0) %p (1) %p (2) %p (3) %p\n", p0, p1, p2, p3);
  init_memory_pool(POOL_SIZE, p0);
  add_new_area(p3, POOL_SIZE, p0);
  add_new_area(p1, POOL_SIZE, p0);
  add_new_area(p2, POOL_SIZE, p0);
 
  if (!(ptr=malloc_ex(POOL_SIZE*2, p0))){
    printf("Error\n");
    exit(-1);
  }

  free_ex(ptr, p0);
 
  destroy_memory_pool(p0);
  printf("Test OK\n");
  exit(0);
}

    
	
