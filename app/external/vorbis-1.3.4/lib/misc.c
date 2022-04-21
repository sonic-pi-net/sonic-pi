/********************************************************************
 *                                                                  *
 * THIS FILE IS PART OF THE OggVorbis SOFTWARE CODEC SOURCE CODE.   *
 * USE, DISTRIBUTION AND REPRODUCTION OF THIS LIBRARY SOURCE IS     *
 * GOVERNED BY A BSD-STYLE SOURCE LICENSE INCLUDED WITH THIS SOURCE *
 * IN 'COPYING'. PLEASE READ THESE TERMS BEFORE DISTRIBUTING.       *
 *                                                                  *
 * THE OggVorbis SOURCE CODE IS (C) COPYRIGHT 1994-2007             *
 * by the Xiph.Org Foundation http://www.xiph.org/                  *
 *                                                                  *
 ********************************************************************/

#define HEAD_ALIGN 32
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "vorbis/codec.h"
#define MISC_C
#include "misc.h"
#include <sys/time.h>

static pthread_mutex_t memlock=PTHREAD_MUTEX_INITIALIZER;
static void **pointers=NULL;
static long *insertlist=NULL; /* We can't embed this in the pointer list;
                          a pointer can have any value... */

static char **files=NULL;
static long *file_bytes=NULL;
static int  filecount=0;

static int ptop=0;
static int palloced=0;
static int pinsert=0;

typedef struct {
  char *file;
  long line;
  long ptr;
  long bytes;
} head;

long global_bytes=0;
long start_time=-1;

static void *_insert(void *ptr,long bytes,char *file,long line){
  ((head *)ptr)->file=file;
  ((head *)ptr)->line=line;
  ((head *)ptr)->ptr=pinsert;
  ((head *)ptr)->bytes=bytes-HEAD_ALIGN;

  pthread_mutex_lock(&memlock);
  if(pinsert>=palloced){
    palloced+=64;
    if(pointers){
      pointers=(void **)realloc(pointers,sizeof(void **)*palloced);
      insertlist=(long *)realloc(insertlist,sizeof(long *)*palloced);
    }else{
      pointers=(void **)malloc(sizeof(void **)*palloced);
      insertlist=(long *)malloc(sizeof(long *)*palloced);
    }
  }

  pointers[pinsert]=ptr;

  if(pinsert==ptop)
    pinsert=++ptop;
  else
    pinsert=insertlist[pinsert];

#ifdef _VDBG_GRAPHFILE
  {
    FILE *out;
    struct timeval tv;
    static struct timezone tz;
    int i;
    char buffer[80];
    gettimeofday(&tv,&tz);

    for(i=0;i<filecount;i++)
      if(!strcmp(file,files[i]))break;

    if(i==filecount){
      filecount++;
      if(!files){
        files=malloc(filecount*sizeof(*files));
        file_bytes=malloc(filecount*sizeof(*file_bytes));
      }else{
        files=realloc(files,filecount*sizeof(*files));
        file_bytes=realloc(file_bytes,filecount*sizeof(*file_bytes));
      }
      files[i]=strdup(file);
      file_bytes[i]=0;
    }

    file_bytes[i]+=bytes-HEAD_ALIGN;

    if(start_time==-1)start_time=(tv.tv_sec*1000)+(tv.tv_usec/1000);

    snprintf(buffer,80,"%s%s",file,_VDBG_GRAPHFILE);
    out=fopen(buffer,"a");
    fprintf(out,"%ld, %ld\n",-start_time+(tv.tv_sec*1000)+(tv.tv_usec/1000),
            file_bytes[i]-(bytes-HEAD_ALIGN));
    fprintf(out,"%ld, %ld # FILE %s LINE %ld\n",
            -start_time+(tv.tv_sec*1000)+(tv.tv_usec/1000),
            file_bytes[i],file,line);
    fclose(out);

    out=fopen(_VDBG_GRAPHFILE,"a");
    fprintf(out,"%ld, %ld\n",-start_time+(tv.tv_sec*1000)+(tv.tv_usec/1000),
            global_bytes);
    fprintf(out,"%ld, %ld\n",-start_time+(tv.tv_sec*1000)+(tv.tv_usec/1000),
            global_bytes+(bytes-HEAD_ALIGN));
    fclose(out);
  }
#endif

  global_bytes+=(bytes-HEAD_ALIGN);

  pthread_mutex_unlock(&memlock);
  return(ptr+HEAD_ALIGN);
}

static void _ripremove(void *ptr){
  int insert;
  pthread_mutex_lock(&memlock);

#ifdef _VDBG_GRAPHFILE
  {
    FILE *out=fopen(_VDBG_GRAPHFILE,"a");
    struct timeval tv;
    static struct timezone tz;
    char buffer[80];
    char *file =((head *)ptr)->file;
    long bytes =((head *)ptr)->bytes;
    int i;

    gettimeofday(&tv,&tz);
    fprintf(out,"%ld, %ld\n",-start_time+(tv.tv_sec*1000)+(tv.tv_usec/1000),
            global_bytes);
    fprintf(out,"%ld, %ld\n",-start_time+(tv.tv_sec*1000)+(tv.tv_usec/1000),
            global_bytes-((head *)ptr)->bytes);
    fclose(out);

    for(i=0;i<filecount;i++)
      if(!strcmp(file,files[i]))break;

    snprintf(buffer,80,"%s%s",file,_VDBG_GRAPHFILE);
    out=fopen(buffer,"a");
    fprintf(out,"%ld, %ld\n",-start_time+(tv.tv_sec*1000)+(tv.tv_usec/1000),
            file_bytes[i]);
    fprintf(out,"%ld, %ld\n",-start_time+(tv.tv_sec*1000)+(tv.tv_usec/1000),
            file_bytes[i]-bytes);
    fclose(out);

    file_bytes[i]-=bytes;

  }
#endif

  global_bytes-=((head *)ptr)->bytes;

  insert=((head *)ptr)->ptr;
  insertlist[insert]=pinsert;
  pinsert=insert;

  if(pointers[insert]==NULL){
    fprintf(stderr,"DEBUGGING MALLOC ERROR: freeing previously freed memory\n");
    fprintf(stderr,"\t%s %ld\n",((head *)ptr)->file,((head *)ptr)->line);
  }

  if(global_bytes<0){
    fprintf(stderr,"DEBUGGING MALLOC ERROR: freeing unmalloced memory\n");
  }

  pointers[insert]=NULL;
  pthread_mutex_unlock(&memlock);
}

void _VDBG_dump(void){
  int i;
  pthread_mutex_lock(&memlock);
  for(i=0;i<ptop;i++){
    head *ptr=pointers[i];
    if(ptr)
      fprintf(stderr,"unfreed bytes from %s:%ld\n",
              ptr->file,ptr->line);
  }

  pthread_mutex_unlock(&memlock);
}

void *_VDBG_malloc(void *ptr,long bytes,char *file,long line){
  if(bytes<=0)
    fprintf(stderr,"bad malloc request (%ld bytes) from %s:%ld\n",bytes,file,line);

  bytes+=HEAD_ALIGN;
  if(ptr){
    ptr-=HEAD_ALIGN;
    _ripremove(ptr);
    ptr=realloc(ptr,bytes);
  }else{
    ptr=malloc(bytes);
    memset(ptr,0,bytes);
  }
  return _insert(ptr,bytes,file,line);
}

void _VDBG_free(void *ptr,char *file,long line){
  if(ptr){
    ptr-=HEAD_ALIGN;
    _ripremove(ptr);
    free(ptr);
  }
}

