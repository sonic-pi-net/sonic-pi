/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

#ifndef __RP_METHOD_INFO__
#define __RP_METHOD_INFO__

#include <ruby.h>

extern VALUE cMethodInfo;

/* A key used to identify each method */
typedef struct
{
    VALUE klass;                            /* The method's class. */
    ID mid;                                 /* The method id. */
    st_index_t key;                         /* Cache calculated key */
} prof_method_key_t;


/* Forward declaration, see rp_call_info.h */
struct prof_call_infos_t;

/* Profiling information for each method. */
typedef struct 
{
    prof_method_key_t *key;                 /* Method key */
    const char *source_file;                /* The method's source file */
    int line;                               /* The method's line number. */
    struct prof_call_infos_t *call_infos;   /* Call info objects for this method */
    VALUE object;                           /* Cached ruby object */
} prof_method_t;

void rp_init_method_info(void);

void method_key(prof_method_key_t* key, VALUE klass, ID mid);

st_table * method_table_create();
prof_method_t * method_table_lookup(st_table *table, const prof_method_key_t* key);
size_t method_table_insert(st_table *table, const prof_method_key_t *key, prof_method_t *val);
void method_table_free(st_table *table);

prof_method_t* prof_method_create(VALUE klass, ID mid, const char* source_file, int line);
VALUE prof_method_wrap(prof_method_t *result);
void prof_method_mark(prof_method_t *method);

/* Setup infrastructure to use method keys as hash comparisons */
int method_table_cmp(prof_method_key_t *key1, prof_method_key_t *key2);
st_index_t method_table_hash(prof_method_key_t *key);

extern struct st_hash_type type_method_hash;

#endif
