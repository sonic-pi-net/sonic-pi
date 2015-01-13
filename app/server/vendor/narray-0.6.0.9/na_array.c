/*
  na_array.c
  Numerical Array Extention for Ruby
    (C) Copyright 1999-2008 by Masahiro TANAKA

  This program is free software.
  You can distribute/modify this program
  under the same terms as Ruby itself.
  NO WARRANTY.
*/
#include <ruby.h>
#include "narray.h"
#include "narray_local.h"

/* Multi-Dimensional Array Investigation */
typedef struct {
  int shape;
  VALUE val;
} na_mdai_item_t;

typedef struct {
  int n;
  na_mdai_item_t *item;
  int *type;
} na_mdai_t;


int na_object_type(VALUE v)
{
  switch(TYPE(v)) {

  case T_TRUE:
  case T_FALSE:
    return NA_BYTE;

  case T_FIXNUM:
  case T_BIGNUM:
    return NA_LINT;

  case T_FLOAT:
    return NA_DFLOAT;

  case T_NIL:
    return NA_NONE;

  default:
    if (IsNArray(v))
      return ((struct NARRAY *)(RDATA(v)->data))->type ;

    if (CLASS_OF(v) == cComplex)
      return NA_DCOMPLEX;
  }
  return NA_ROBJ;
}


static na_mdai_t *
  na_alloc_mdai(VALUE ary)
{
  int i, n=2;
  na_mdai_t *mdai;

  mdai = ALLOC(na_mdai_t);
  mdai->n = n;
  mdai->item = ALLOC_N( na_mdai_item_t, n );
  for (i=0; i<n; ++i) {
    mdai->item[i].shape = 0;
    mdai->item[i].val = Qnil;
  }
  mdai->item[0].val = ary;
  mdai->type = ALLOC_N( int, NA_NTYPES );
  for (i=0; i<NA_NTYPES; ++i)
    mdai->type[i]=0;

  return mdai;
}

static void
  na_realloc_mdai(na_mdai_t *mdai, int n_extra)
{
  int i, n;

  i = mdai->n;
  mdai->n += n_extra;
  n = mdai->n;
  REALLOC_N( mdai->item, na_mdai_item_t, n );
  for (; i<n; ++i) {
    mdai->item[i].shape = 0;
    mdai->item[i].val = Qnil;
  }
}

static int *
  na_free_mdai(na_mdai_t *mdai, int *rank, int *type)
{
  int i, t, r;
  int *shape;

  for (t=i=NA_BYTE; i<NA_NTYPES; ++i) {
    if ( mdai->type[i] > 0 )
      t = na_upcast[t][i];
  }
  *type = t;
  for (i=0; i < mdai->n && mdai->item[i].shape > 0; ++i) ;
  *rank = r = i;
  shape = ALLOC_N(int,r);
  for (i=0; r-->0; ++i) {
    shape[i] = mdai->item[r].shape;
  }
  xfree(mdai->type);
  xfree(mdai->item);
  xfree(mdai);
  return shape;
}


#define EXCL(r) (RTEST(rb_funcall((r),na_id_exclude_end,0)))

/* Range as a Sequence of numbers */
static void
 na_range_to_sequence(VALUE obj, int *n, int *beg, int *step)
{
  int end,len;

  *beg = NUM2INT(rb_funcall(obj, na_id_beg, 0));
  end = NUM2INT(rb_funcall(obj, na_id_end, 0));
  len = end - *beg;

  /* direction */
  if (len>0) {
    *step = 1;
    if (EXCL(obj)) --end; else ++len;
  }
  else if (len<0) {
    len   = -len;
    *step = -1;
    if (EXCL(obj)) ++end; else ++len;
  }
  else /*if(len==0)*/ {
    *step = 0;
    if (!EXCL(obj)) {
      ++len;
    }
  } 
  *n = len;
}


/* investigate rank, shape, type of Array */
static int
  na_do_mdai(na_mdai_t *mdai, int rank)
{
  int i, j, len, length, start, dir;
  VALUE v;
  VALUE ary;

  ary = mdai->item[rank-1].val;
  len = RARRAY_LEN(ary);

  for (i=0; i < RARRAY_LEN(ary); ++i) {

    v = RARRAY_PTR(ary)[i];

    if (TYPE(v) == T_ARRAY) {
      /* check recursive array */
      for (j=0; j<rank; ++j) {
	if (mdai->item[j].val == v)
	  rb_raise(rb_eStandardError,"converting recursive Array to NArray");
      }
      if ( rank >= mdai->n ) {
	na_realloc_mdai(mdai,2);
      }
      mdai->item[rank].val = v;
      if ( na_do_mdai(mdai,rank+1) ) {
	--len; /* Array is empty */
      }
    }
    else
    if ( rb_obj_is_kind_of(v, rb_cRange) ) {
      na_range_to_sequence(v,&length,&start,&dir);
      len += length-1;
      mdai->type[ na_object_type(rb_funcall(v, na_id_beg, 0)) ] = 1;
      mdai->type[ na_object_type(rb_funcall(v, na_id_end, 0)) ] = 1;
    }
    else {

      mdai->type[ na_object_type(v) ] = 1;

      if (IsNArray(v)) {
	int r;
	struct NARRAY *na;  GetNArray(v,na);

	if ( na->rank == 0 ) {
	  --len; /* NArray is empty */
	} else {
	  if ( rank+na->rank > mdai->n ) {
	    na_realloc_mdai(mdai,((na->rank-1)/4+1)*4);
	  }
	  for ( j=na->rank, r=rank; j-- > 0  ; ++r ) {
	    if ( mdai->item[r].shape < na->shape[j] )
	      mdai->item[r].shape = na->shape[j];
	  }
	}
      }
    }
  }

  if (len==0) return 1; /* this array is empty */
  if (mdai->item[rank-1].shape < len) {
    mdai->item[rank-1].shape = len;
  }
  return 0;
}


/* get index from multiple-index  */
static int
 na_index_pos(struct NARRAY *ary, int *idxs)
{
  int i, idx, pos = 0;

  for ( i = ary->rank; (i--)>0; ) {
    idx = idxs[i];
    if (idx < 0 || ary->shape[i] <= idx) {
      abort();
      rb_raise(rb_eRuntimeError,
	       "Subsctipt out of range: accessing shape[%i]=%i with %i",
	       i, ary->shape[i], idx );
    }
    pos = pos * ary->shape[i] + idx;
  }
  return pos;
}


static void
 na_copy_nary_to_nary(VALUE obj, struct NARRAY *dst,
		      int thisrank, int *idx)
{
  struct NARRAY *src;
  struct slice *s;
  int  i, n;

  GetNArray(obj,src);
  n = thisrank - src->rank + 1;

  s = ALLOCA_N(struct slice, dst->rank+1);
  for (i=0; i < n; ++i) {
    s[i].n    = 1;
    s[i].beg  = 0;
    s[i].step = 0;
    s[i].idx  = NULL;
  }
  for (   ; i <= thisrank; ++i) {
    s[i].n    = src->shape[i-n];
    s[i].beg  = 0;
    s[i].step = 1;
    s[i].idx  = NULL;
  }
  for (   ; i < dst->rank; ++i) {
    s[i].n    = 1;
    s[i].beg  = idx[i];
    s[i].step = 0;
    s[i].idx  = NULL;
  }
  na_aset_slice(dst,src,s);
}


/* copy Array to NArray */
static void
 na_copy_ary_to_nary( VALUE ary, struct NARRAY *na,
		      int thisrank, int *idx, int type )
{
  int i, j, pos, len, start, step, dir;
  VALUE v;

  if (thisrank==0) {
    for (i = idx[0] = 0; i < RARRAY_LEN(ary); ++i) {
      v = RARRAY_PTR(ary)[i];
      if (rb_obj_is_kind_of(v, rb_cRange)) {
	na_range_to_sequence(v,&len,&start,&dir);
	if (len>0) {
	  pos = na_index_pos(na,idx);
	  IndGenFuncs[type](len, NA_PTR(na,pos),na_sizeof[type], start,dir);
	  idx[0] += len;
	}
      }
      else if (TYPE(v) != T_ARRAY) {
	/* NIL if empty */
	if (v != Qnil) {
	  pos = na_index_pos(na,idx);
	  SetFuncs[type][NA_ROBJ]( 1, NA_PTR(na,pos), 0, &v, 0 );
	  /* copy here */
	}
	idx[0] ++;
      }
    }
  }
  else /* thisrank > 0 */
  { 
    for (i = idx[thisrank] = 0; i < RARRAY_LEN(ary); ++i) {
      v = RARRAY_PTR(ary)[i];
      if (TYPE(v) == T_ARRAY) {
	na_copy_ary_to_nary(v,na,thisrank-1,idx,type);
	if (idx[thisrank-1]>0) ++idx[thisrank];
      }
      else if (IsNArray(v)) {
	na_copy_nary_to_nary(v,na,thisrank-1,idx);
	++idx[thisrank];
      }
      else {
	for (j=thisrank; j; ) idx[--j] = 0;

	if (rb_obj_is_kind_of(v, rb_cRange)) {
	  na_range_to_sequence(v,&len,&start,&dir);
	  if (len>0) {
	    pos = na_index_pos(na,idx);
	    ++idx[thisrank];
	    step = na_index_pos(na,idx)-pos;
	    IndGenFuncs[type]( len, NA_PTR(na,pos), na_sizeof[type]*step,
			       start, dir );
	    idx[thisrank] += len-1;
	  }
	}
	else {
	  pos = na_index_pos(na,idx);
	  SetFuncs[type][NA_ROBJ]( 1, NA_PTR(na,pos), 0, &(RARRAY_PTR(ary)[i]), 0 );
	  ++idx[thisrank];
	}
	/* copy here */
      }
    }
  }
}


static VALUE
 na_ary_to_nary_w_type(VALUE ary, int type_spec, VALUE klass)
{
  int  i, rank;
  int  type = NA_BYTE;
  int *shape, *idx;
  na_mdai_t *mdai;
  struct NARRAY *na;
  VALUE v;

  /* empty array */
  if (RARRAY_LEN(ary) < 1) {
    return na_make_empty( type, klass );
  }

  mdai  = na_alloc_mdai(ary);
  na_do_mdai(mdai,1);
  shape = na_free_mdai(mdai,&rank,&type);

  /*
  printf("rank=%i\n", rank);
  printf("type=%i\n", type);
  for (i=0; i<rank; ++i) {
    printf("shape[%i]=%i\n", i, shape[i]);
  }
  */

  /* type specification */
  if (type_spec!=NA_NONE)
    type = type_spec;

  /* empty array */
  if (rank==0)
    return na_make_empty( type, klass );

  /* Create NArray */
  v  = na_make_object(type,rank,shape,klass);
  xfree(shape);

  GetNArray(v,na);
  na_clear_data(na);

  idx = ALLOCA_N(int,rank);
  for (i=0; i<rank; ++i) idx[i]=0;

  na_copy_ary_to_nary( ary, na, rank-1, idx, type );

  return v;
}


VALUE
 na_ary_to_nary(VALUE ary, VALUE klass)
{
  return na_ary_to_nary_w_type( ary, NA_NONE, klass );
}


/* obj.kind_of?(NArray) == true */

VALUE
 na_dup_w_type(VALUE v2, int type)
{
  VALUE  v1;
  struct NARRAY *a1, *a2;

  GetNArray(v2,a2);
  v1 = na_make_object(type, a2->rank, a2->shape, CLASS_OF(v2));
  GetNArray(v1,a1);
  na_copy_nary(a1,a2);
  return v1;
}


VALUE
 na_change_type(VALUE obj, int type)
{
  struct NARRAY *a2;

  GetNArray(obj,a2);

  if (a2->type == type)
    return obj;

  return na_dup_w_type(obj, type);
}


VALUE
 na_upcast_type(VALUE obj, int type)  /* na_upcast_narray */
{
  int newtype;
  struct NARRAY *a2;

  GetNArray(obj,a2);
  newtype = na_upcast[a2->type][type];

  if (newtype == a2->type)
    return obj;

  return na_dup_w_type(obj, newtype);
}


/* obj.kind_of?(Object) == true */

VALUE
 na_cast_object(VALUE obj, int type) /* na_cast_certain */
{
  if (IsNArray(obj)) {
    return na_change_type(obj,type);
  }
  if (TYPE(obj) == T_ARRAY) {
    return na_ary_to_nary_w_type(obj,type,cNArray);
  }
  return na_make_scalar(obj,type);
}


VALUE
 na_cast_unless_narray(VALUE obj, int type)
{
  if (IsNArray(obj)) {
    return obj;
  }
  if (TYPE(obj) == T_ARRAY) {
    return na_ary_to_nary_w_type(obj,type,cNArray);
  }
  return na_make_scalar(obj,type);
}


VALUE
 na_cast_unless_array(VALUE obj, int type)
{
  if (IsNArray(obj)) {
    return obj;
  }
  if (TYPE(obj) == T_ARRAY) {
    return na_ary_to_nary(obj,cNArray);
  }
  return na_make_scalar(obj,type);
}


VALUE
 na_upcast_object(VALUE obj, int type)
{
  if (IsNArray(obj)) {
    return na_upcast_type(obj,type);
  }
  if (TYPE(obj) == T_ARRAY) {
    return na_ary_to_nary_w_type(obj,type,cNArray);
  }
  return na_make_scalar(obj,type);
}


/* :nodoc: */
VALUE
 na_to_narray(VALUE obj)
{
  if (IsNArray(obj)) {
    return obj;
  }
  if (TYPE(obj) == T_ARRAY) {
    return na_ary_to_nary(obj,cNArray);
  }
  return na_make_scalar(obj,na_object_type(obj));
}


/* convert NArray to Array */
static VALUE
 na_to_array0(struct NARRAY* na, int *idx, int thisrank, void (*func)())
{
  int i, elmsz;
  char *ptr;
  VALUE ary, val;

  /* Create New Array */
  ary = rb_ary_new2(na->shape[thisrank]);

  if (thisrank == 0) {
    ptr   = NA_PTR( na, na_index_pos(na,idx) );
    elmsz = na_sizeof[na->type];
    for (i = na->shape[0]; i; --i) {
      (*func)( 1, &val, 0, ptr, 0 );
      ptr += elmsz;
      rb_ary_push( ary, val );
    }
  }
  else {
    for (i = 0; i < na->shape[thisrank]; ++i) {
      idx[thisrank] = i;
      rb_ary_push( ary, na_to_array0(na,idx,thisrank-1,func) );
    }
  }
  return ary;
}


/* method: to_a -- convert itself to Array */
VALUE
 na_to_array(VALUE obj)
{
  struct NARRAY *na;
  int *idx, i;

  GetNArray(obj,na);

  if (na->rank<1)
    return rb_ary_new();

  idx = ALLOCA_N(int,na->rank);
  for (i = 0; i<na->rank; ++i) idx[i] = 0;
  return na_to_array0(na,idx,na->rank-1,SetFuncs[NA_ROBJ][na->type]);
}


static VALUE
 na_inspect_col( int n, char *p2, int p2step, void (*tostr)(),
		 VALUE sep, int rank )
{
  VALUE str=Qnil, tmp;
  int max_col = 77;
  int sep_len = RSTRING_LEN(sep);

  if (n>0)
    (*tostr)(&str,p2);

  for (n--; n>0; --n) {
    p2 += p2step;
    (*tostr)(&tmp,p2);

    if (!NIL_P(sep)) rb_str_concat(str, sep);

    if (RSTRING_LEN(str) + RSTRING_LEN(tmp) + rank*4 + sep_len < max_col) {
      rb_str_concat(str, tmp);
    } else {
      rb_str_cat(str,"...",3);
      return str;
    }
  }
  return str;
}


/*
 *   Create inspect string ... under construction
 */

VALUE
 na_make_inspect(VALUE val)
{
  int   i, ii, rank, count_line=0, max_line=10;
  int  *si;
  struct NARRAY *ary;
  struct slice *s1;

  VALUE fs = rb_str_new(", ",2);

  GetNArray(val,ary);
  if (ary->total < 1) return rb_str_new(0, 0);

  /* Allocate Structure */
  rank = ary->rank;
  s1 = ALLOCA_N(struct slice, rank+1);
  si = ALLOCA_N(int,rank);
  na_set_slice_1obj(rank,s1,ary->shape);

  /* Iteration */
  na_init_slice(s1, rank, ary->shape, na_sizeof[ary->type]);
  i = rank;
  s1[i].p = ary->ptr;
  val = rb_str_new(0,0);
  for(;;) {
    /* set pointers */
    while (i > 0) {
      --i;
      rb_str_cat(val, "[ ", 2);
      s1[i].p = s1[i].pbeg + s1[i+1].p;
      si[i] = s1[i].n;
    }

    rb_str_concat(val, na_inspect_col( s1[0].n, s1[0].p, s1[0].pstep,
				       InspFuncs[ary->type], fs, rank ));

    /* rank up */
    do {
      rb_str_cat(val, " ]", 2);
      if ( ++i == rank ) return val;
    } while ( --si[i] == 0 );
    s1[i].p += s1[i].pstep;

    rb_str_concat(val, fs);
    rb_str_cat(val, "\n", 1);

    /* count check */
    if (++count_line>=max_line) {
      rb_str_cat(val, " ...", 4);
      return val;
    }
    /* indent */
    for (ii=i; ii<rank; ++ii)
      rb_str_cat(val, "  ", 2);
  }
}


void Init_na_array() {
    rb_define_method(cNArray, "to_a", na_to_array,0); //
}
