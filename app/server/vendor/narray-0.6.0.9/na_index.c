/*
  na_index.c
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

#define EXCL(r) (RTEST(rb_funcall((r),na_id_exclude_end,0)))

static int
 na_index_range(VALUE obj, int size, struct slice *sl)
{
  int beg,end,len,step;
  VALUE vbeg, vend;

  sl->idx  = NULL;

  /* Beginning */
  vbeg = rb_funcall(obj, na_id_beg, 0);
  if (vbeg==Qnil) /* First is nil */
    beg = 0;
  else
    beg = NUM2INT(vbeg);
  if (beg<0) beg += size;

  /* End */
  vend = rb_funcall(obj, na_id_end, 0);
  if (vend==Qnil) { /* Last is nil */
    sl->beg  = beg;
    sl->step = 1;
    return sl->n = 0;
  }
  else
    end = NUM2INT(vend);
  if (end<0) end += size;

  /* length */
  len = end-beg;

  /* direction */
  if (len>0) {
    step = 1;
    if (EXCL(obj)) --end; else ++len;
  }
  else if (len<0) {
    len  = -len;
    step = -1;
    if (EXCL(obj)) ++end; else ++len;
  }
  else /*if(len==0)*/ {
    if (EXCL(obj))
      rb_raise(rb_eIndexError, "empty range");
    else {
      ++len;
      step = 1;  /* or 0 ? depend on whether removing rank */
    }
  } 

  if ( beg<0 || beg>=size || end<0 || end>=size )
    rb_raise(rb_eIndexError, "index out of range");

  sl->n    = len;
  sl->beg  = beg;
  sl->step = step;
  return len;
}


static int
 na_index_scalar(int idx, int size, struct slice *sl)
{
  if (idx<0) idx+=size;
  if (idx<0 || idx>=size)
    rb_raise(rb_eIndexError, "index out of range");
  sl->n    = 1;
  sl->beg  = idx;
  sl->step = 0;
  sl->idx  = NULL;
  return 1;
}


static int
 na_ary_to_index(struct NARRAY *a1, int size, struct slice *s)
{
  int i;
  na_index_t idx, *p;

  /* Empty Array */
  if (a1->total==0) {
    s->n    = 0;
    s->beg  = 0;
    s->step = 1;
    s->idx  = NULL;
  }
  else
  /* single element */
  if (a1->total==1) {
    SetFuncs[NA_LINT][a1->type](1, &idx, 0, a1->ptr, 0);
    if ( idx<0 ) idx += size;
    if ( idx<0 || idx>=size )
      rb_raise(rb_eIndexError, "index %i out of range %i", idx, size);
    s->n    = 1;
    s->beg  = idx;
    s->step = 1;
    s->idx  = NULL;
  }
  else {
    /* Copy index array */
    s->n    = a1->total;
    s->step = 1;
    s->idx  = p = ALLOC_N(na_index_t, a1->total);
    SetFuncs[NA_LINT][a1->type]( s->n,
				 s->idx, na_sizeof[NA_LINT],
				 a1->ptr, na_sizeof[a1->type] );
    for ( i=a1->total; i>0; --i ) {
      if ( *p<0 ) *p += size;
      if ( *p<0 || *p>=size )
	rb_raise(rb_eIndexError, "index %i out of range %i", *p, size);
      ++p;
    }
    s->beg  = s->idx[0];
  }

  return s->n;
}


static struct NARRAY *
 na_flatten_temporarily(struct NARRAY *dst, struct NARRAY *src)
{
  /* Not normal construction !! Do not wrap as object ! */
  dst->shape = &(dst->total);
  dst->rank  = 1;
  dst->total = src->total;
  dst->type  = src->type;
  dst->ptr   = src->ptr;
  dst->ref   = src->ref;
  return dst;
}
#define na_flatten_temp(ary) \
 {ary = na_flatten_temporarily(ALLOCA_N(struct NARRAY,1), ary);}


/* free index memory */
static void na_free_slice_index(struct slice *s, int n)
{
  while (n-->0)
    if (s[n].idx != NULL) xfree(s[n].idx);
}


static int na_index_test(volatile VALUE idx, int shape, struct slice *sl)
{
  int size;
  struct NARRAY *na;

  switch(TYPE(idx)) {

  case T_FIXNUM:
    /* scalar slice */
    na_index_scalar(FIX2LONG(idx),shape,sl);
    return 1;

  case T_FLOAT:
    /* scalar slice */
    na_index_scalar(NUM2LONG(idx),shape,sl);
    return 1;

  case T_NIL:
  case T_TRUE:
    /* entire slice */
    sl->n    = shape;
    sl->beg  = 0;
    sl->step = 1;
    sl->idx  = NULL;
    return shape;

  case T_ARRAY:
    /* Array Index */
    idx = na_cast_object(idx,NA_LINT);
    GetNArray(idx,na);
    size = na_ary_to_index(na,shape,sl);
    return size;

  default:
    /* Range object */
    if (rb_obj_is_kind_of(idx, rb_cRange)) {
      size = na_index_range(idx,shape,sl);
    }
    else
    /* NArray index */
    if (NA_IsNArray(idx)) {
      GetNArray(idx,na);
      size = na_ary_to_index(na,shape,sl);
    }
    else
    /* NO ALLOWED */
    if (TYPE(idx)==T_BIGNUM) {
      rb_raise(rb_eIndexError, "BigNum is not allowed");
    }
    else
      rb_raise(rb_eIndexError, "not allowed type");
  }
  return size;
}


static int
 na_index_analysis(int nidx, VALUE *idx, struct NARRAY *ary, struct slice *sl)
{
  int i, j, k, total=1, size;
  int multi_ellip=0;

  for (i=j=0; i<nidx; ++i) {
    if (TYPE(idx[i])==T_FALSE) {
      if (multi_ellip!=0)
	rb_raise(rb_eIndexError, "multiple ellipsis-dimension is not allowd");
      for (k=ary->rank-nidx+1; k>0; --k,++j) {
	size = na_index_test( Qtrue, ary->shape[j], &sl[j] );
	if (size != 1)
	  total *= size;
      }
      multi_ellip = 1;
    } else {
      if (j < ary->rank) {
         size = na_index_test( idx[i], ary->shape[j], &sl[j] );
         if (size != 1)
             total *= size;
      }
      ++j;
    }
  }
  if (j != ary->rank)
    rb_raise(rb_eIndexError, "# of index=%i != ary.dim=%i", j, ary->rank);

  return total;
}


/* --------------------  Class Dimension  -------------------- */
int
 na_shrink_class(int class_dim, int *shrink)
{
  int i;

  for (i=0; i<class_dim; ++i) {
    if (shrink[i]==0)      /* non-trim dimention */
      return 0;
  }
  return 1; /* all trim */
}


/* remove single-element rank */
VALUE
 na_shrink_rank(VALUE obj, int class_dim, int *shrink)
{
  int  i, j;
  struct NARRAY *ary;

  GetNArray(obj,ary);

  if (ary->rank < class_dim)
    return obj;
  
  for (j=i=0; i<class_dim; ++i) {
    if (ary->shape[i]!=1 || shrink[i]==0) /* not trim */
      ++j;
  }

  if (j>0)		/* if   non-trim dimensions exist, */
    j = class_dim;      /* then do not trim class_dimension.  */
			/* if (j==0) then all trim. */

  for (i=class_dim; i<ary->rank; ++i) {
    if (ary->shape[i]!=1 || shrink[i]==0) {    /* not trim */
      if (i>j) ary->shape[j] = ary->shape[i];
      ++j;
    }
  }
  ary->rank = j;

  if (j==0 && ary->total==1) {
    SetFuncs[NA_ROBJ][ary->type](1, &obj, 0, ary->ptr, 0);
  }
  return obj;
}


/* ------------------- bracket methods ------------------ */
/*
	[] -- Reference method
*/

static VALUE
 na_aref_slice(struct NARRAY *a2, struct slice *s2, VALUE klass, int flag)
{
  int i, ndim, class_dim, *shape, *shrink;
  VALUE  extr;
  struct NARRAY *a1;
  struct slice  *s1;

  ndim = a2->rank;
  shape = ALLOCA_N(int,ndim);
  shrink = ALLOCA_N(int,ndim);

  for (i=0; i<ndim; ++i) {
    shape[i] = s2[i].n;
    if (shape[i]==1 && s2[i].step==0) /* shrink? */
      shrink[i] = 1;
    else
      shrink[i] = 0;
  }

  class_dim = na_class_dim(klass);

  if (ndim < class_dim)
    rb_raise(rb_eRuntimeError,
	     "dimension(%i) is smaller than CLASS_DIMENSION(%i)",
	     ndim, class_dim);

  if ((!flag) && class_dim>0 && na_shrink_class(class_dim,shrink))
    klass = cNArray;

  extr = na_make_object( a2->type, ndim, shape, klass );
  GetNArray(extr,a1);

  s1 = ALLOC_N(struct slice, ndim+1);
  na_set_slice_1obj(ndim,s1,a1->shape);

  na_init_slice( s1, ndim, shape, na_sizeof[a2->type] );
  na_init_slice( s2, ndim, a2->shape, na_sizeof[a2->type] );
  na_loop_index_ref( a1, a2, s1, s2, SetFuncs[a2->type][a2->type] );

  xfree(s1);
  if (!flag)
    extr = na_shrink_rank(extr,class_dim,shrink);

  return extr;
}



static VALUE
 na_aref_single_dim_array(VALUE self, volatile VALUE vidx)
{
  int  total;
  struct NARRAY *a1, *a2, *aidx;
  struct slice  *s1, *s2;
  VALUE v;

  GetNArray( self, a1 );
  vidx = na_cast_object( vidx, NA_LINT );
  GetNArray(vidx,aidx);

  /* make Slice from index */
  s1    = ALLOCA_N(struct slice, 2);
  total = na_ary_to_index( aidx, a1->total, s1 );

  if (total==0) {
    return na_make_empty(a1->type,cNArray);
  }
  else {
    /* create New NArray & 1-dimentionize */
    v = na_make_object( a1->type, aidx->rank, aidx->shape, CLASS_OF(vidx) );
    GetNArray(v,a2);
    if (a2->rank>1) na_flatten_temp(a2);
    if (a1->rank>1) na_flatten_temp(a1);

    /* Slice for Destination array */
    s2 = ALLOCA_N(struct slice, 2);
    na_set_slice_1obj(1,s2,a2->shape);

    /* Iteration */
    na_init_slice( s2, 1, a2->shape, na_sizeof[a1->type] );
    na_init_slice( s1, 1, a1->shape, na_sizeof[a1->type] );
    na_loop_index_ref( a2, a1, s2, s1, SetFuncs[a1->type][a1->type] );
  }

  na_free_slice_index(s1,1);
  return v;
}


static VALUE
 na_aref_single_dim(VALUE self, VALUE idx, int flag)
{
  int size;
  VALUE v;
  struct NARRAY *ary, *arynew;
  struct slice *sl;

  GetNArray(self,ary);

  sl   = ALLOCA_N(struct slice, 2);
  size = na_index_test(idx, ary->total, sl);

  if ( size == 1 ) {
    if (flag || sl->step!=0) {
      /* single-element NArray */
      v = na_make_object(ary->type,1,&size,cNArray);
      GetNArray(v,arynew);
      SetFuncs[ary->type][ary->type](1, arynew->ptr,0, NA_PTR(ary,sl->beg),0);
    } else {
      SetFuncs[NA_ROBJ][ary->type](1, &v,0, NA_PTR(ary,sl->beg),0);
    }
  }
  else
  if ( size > 1 ) {
    if ( ary->rank > 1 )  /* 1-dimensional serial index */
      na_flatten_temp(ary);
    v = na_aref_slice(ary, sl, CLASS_OF(self), flag);
  }
  else /* size < 1 */ {
    v = na_make_empty(ary->type,cNArray);
  }
  /* na_free_slice_index(sl,1);  free index memory */
  return v;
}


static VALUE
 na_aref_multi_dim_single_elm(VALUE self, struct slice *sl, int flag)
{
  int i, rank, pos, *shape;
  struct NARRAY *ary, *arynew;
  VALUE v;

  ary = (struct NARRAY *)DATA_PTR(self); /* type is already checked */

  /* check rank-shrink; whether return NArray or Element */
  if (flag==0) {
    rank = 0; /* [] */
    for ( i=ary->rank; (i--)>0; ) {
      if (sl[i].step!=0) ++rank;
    }
  }
  else {
    rank = ary->rank; /* slice() */
  }
  /* get position */
  pos = 0;
  for ( i=ary->rank; i-->0; ) {
    pos = pos * ary->shape[i] + sl[i].beg;
  }
  if (rank==0) {
    SetFuncs[NA_ROBJ][ary->type](1, &v, 0, NA_PTR(ary,pos), 0);
  } else {
    VALUE klass;
    int   class_dim;
    klass = CLASS_OF(self);
    class_dim = na_class_dim(klass);
    if (rank < class_dim) rank = class_dim;
    shape = ALLOCA_N(int, rank);
    for (i=0;i<rank;++i) shape[i]=1;
    v = na_make_object(ary->type,rank,shape,klass);
    GetNArray(v,arynew);
    SetFuncs[ary->type][ary->type](1, arynew->ptr, 0, NA_PTR(ary,pos), 0);
  }
  return v;
}


static VALUE
 na_aref_multi_dim(VALUE self, int nidx, VALUE *idx, int flag)
{
  VALUE v;
  int   size;
  struct NARRAY *ary;
  struct slice *sl;

  GetNArray(self,ary);

  if (ary->rank==0)
    rb_raise(rb_eIndexError, "Cannot extract from Empty NArray");

  /* make Slice */
  sl   = ALLOC_N(struct slice, ary->rank+1);
  size = na_index_analysis(nidx, idx, ary, sl);

  if ( size == 1 ) { /* return Single Element */
    v = na_aref_multi_dim_single_elm(self, sl, flag);
  }
  else
  if ( size > 1 ) {
    v = na_aref_slice(ary, sl, CLASS_OF(self), flag);
  }
  else /* size < 1 */ {
    v = na_make_empty(ary->type,cNArray);
  }
  na_free_slice_index(sl,ary->rank); /* free index memory */
  xfree(sl);
  return v;
}


/* vvv mask vvv */
static int
 na_count_true_body(VALUE self)
{
  struct NARRAY *ary;
  int  n, count=0;
  u_int8_t *ptr;

  GetNArray(self,ary);

  if ( ary->type == NA_BYTE ) {
    ptr = (u_int8_t *)ary->ptr;
    n   = ary->total;
    for (; n; --n) {
      if (*ptr++) ++count;
    }
  } else
    rb_raise(rb_eTypeError,"cannot count_true NArray except BYTE type");
  return count;
}

/*
 *  call-seq:
 *     narray.count_true  -> int
 *
 *  Returns the number of true (non-zero) in narray
 */
VALUE
 na_count_true(VALUE self)
{
  return( INT2NUM(na_count_true_body(self)) );
}

static int
 na_count_false_body(VALUE self)
{
  struct NARRAY *ary;
  int  n, count=0;
  u_int8_t *ptr;

  GetNArray(self,ary);

  if ( ary->type == NA_BYTE ) {
    ptr = (u_int8_t *)ary->ptr;
    n   = ary->total;
    for (; n; --n) {
      if (!*ptr++) ++count;
    }
  } else
    rb_raise(rb_eTypeError,"cannot count_false NArray except BYTE type");
  return count;
}

/*
 *  call-seq:
 *     narray.count_false  -> int
 *
 *  Returns the number of false (zero-value) in narray
 */
VALUE
 na_count_false(VALUE self)
{
  return( INT2NUM(na_count_false_body(self)) );
}

/* :nodoc: */
VALUE
 na_aref_mask(VALUE self, VALUE mask)
{
  int total, i;
  struct NARRAY *a1, *am, *a2;
  VALUE v;

  GetNArray( self, a1 );
  GetNArray( mask, am );

  if (a1->total != am->total)
    rb_raise(rb_eTypeError,"self.size(=%i) != mask.size(=%i)",
	     a1->total, am->total);
  if (a1->rank != am->rank) 
    rb_raise(rb_eTypeError,"self.rank(=%i) != mask.rank(=%i)",
	     a1->rank, am->rank);
  for (i=0; i<a1->rank; ++i)
    if (a1->shape[i] != am->shape[i])
      rb_raise(rb_eTypeError,"self.shape[%i](=%i) != mask.shape[%i](=%i)",
	       i, a1->shape[i], i, am->shape[i]);

  total = na_count_true_body(mask);

  v = na_make_object( a1->type, 1, &total, CLASS_OF(self) );
  GetNArray(v,a2);

  RefMaskFuncs[a1->type]
    ( a1->total, a2->ptr, na_sizeof[a2->type], a1->ptr,
      na_sizeof[a1->type], am->ptr, 1 );

  return(v);
}

/* ^^^ mask ^^^ */


/* method: [](idx1,idx2,...,idxN) */
static VALUE
 na_aref_body(int nidx, VALUE *idx, VALUE self, int flag)
{
  if (nidx==0) {
    return na_clone(self);
  }
  if (nidx==1) {
    if ( NA_IsNArray(idx[0]) ) {
      if( NA_TYPE(idx[0]) == NA_BYTE ) /* then supposed to be a mask */
	return na_aref_mask(self, idx[0]);
    }
    if ( na_class_dim(CLASS_OF(self)) != 1 ) {
      if ( NA_IsArray(idx[0]) ) /* Array Index ? */
	return na_aref_single_dim_array( self, idx[0] );
      else
	return na_aref_single_dim( self, idx[0], flag );
    }
  }
  /* if (nidx>1) */
    return na_aref_multi_dim( self, nidx, idx, flag );
}

/* method: [](idx1,idx2,...,idxN) */
VALUE na_aref(int argc, VALUE *argv, VALUE self)
{ return na_aref_body(argc, argv, self, 0); }

/* method: slice(idx1,idx2,...,idxN) */
VALUE na_slice(int argc, VALUE *argv, VALUE self)
{ return na_aref_body(argc, argv, self, 1); }



/*
	[]=  --  Set elements to specified indices
*/

/* make slice for array-set: a[0..-1,1..2] = 1 */
static void
 na_make_slice_aset_fill(int rank, struct NARRAY *src_ary,
			 struct slice *src_slc, int *src_shape,
			 struct slice *dst_slc)
{
  int i;

  for (i=0; i<rank; ++i) {
    src_shape[i]    = 1; /* all 1 */
    if ( (src_slc[i].n = dst_slc[i].n) < 1 )
      rb_raise(rb_eIndexError, "dst_slice[%i].n=%i ???", i, dst_slc[i].n);
    src_slc[i].beg  = 0;
    src_slc[i].idx  = NULL;
    src_slc[i].step = 0;
  }
}


/* make slice for array-set */
static void
 na_make_slice_aset(struct NARRAY *dst, struct NARRAY *src,
		    struct slice *s1, struct slice *s2, int *src_shape)
{
  int  i, j, idx_end;
  
  /* count range index */
  for (j=i=0; i<dst->rank; ++i) {

    if ( s1[i].step !=0 ) { /* Range index */

      /* rank check */
      if ( j >= src->rank )
	  rb_raise(rb_eIndexError, "dst.range-dim=%i > src.dim=%i",
		   j+1, src->rank);

      if ( s1[i].n == 0 ) {
	/* Size is NOT specified:
	   a[0..nil] = other_array
	   a[0]      = other_array
	 */
	s1[i].n = src->shape[j];

	idx_end = s1[i].beg + (s1[i].n-1) * s1[i].step;
	if ( idx_end < 0 || idx_end >= dst->shape[i] )
	  rb_raise(rb_eIndexError, "end-index=%i is out of dst.shape[%i]=%i",
		   idx_end, i, dst->shape[i]);

      } else
	/* Size is specified:
	     a[0..10] = other
	 */
      if ( src->shape[j] >1 && s1[i].n != src->shape[j] ) {
	  rb_raise(rb_eIndexError, "dst.shape[%i]=%i != src.shape[%i]=%i",
		   i, s1[i].n, j, src->shape[j]);
      }
      /* copy source shape */
      src_shape[i] = src->shape[j++];

    }
    else /* if ( s1[i].n==1 )
	 Scalar index:
	   a[0, 0..-1] = other  --- first rank is skipped.
       */
      src_shape[i] = 1;  /* insert dummy rank */

    s2[i].beg  = 0;
    s2[i].idx  = NULL;
    s2[i].n    = s1[i].n;  /* repeate number is same as a1 index */

    if ( s1[i].n >1 && src_shape[i]==1 ) /* Extensible index */
      s2[i].step = 0;
    else
      s2[i].step = 1;
  }

  /* rank check */
  if ( j != src->rank )
    rb_raise(rb_eIndexError, "dst.range-dim=%i < src.dim=%i", j, src->rank);
}



/* Iterate with bifinc, src has extensible index */
void
 na_aset_slice(struct NARRAY *dst, struct NARRAY *src, struct slice *dst_slc)
{
  int   rank = dst->rank;
  int  *src_shape;
  struct slice *src_slc;

  /* rank check */
  if (rank < src->rank)
    rb_raise(rb_eIndexError, "%i dst.ranks < %i src.ranks", rank, src->rank);
  if (src->rank == 0)
    rb_raise(rb_eIndexError, "cannot store empty array");

  /* extend rank */
  src_shape = ALLOCA_N(int, rank);
  src_slc   = ALLOC_N(struct slice, rank+1);

  if (src->total==1)
    na_make_slice_aset_fill( rank, src, src_slc, src_shape, dst_slc );
  else
    na_make_slice_aset( dst, src, dst_slc, src_slc, src_shape );

  /* Iteration */
  na_init_slice( dst_slc, rank, dst->shape, na_sizeof[dst->type] );
  na_init_slice( src_slc, rank, src_shape,  na_sizeof[src->type] );
  na_loop_general( dst,src, dst_slc,src_slc, SetFuncs[dst->type][src->type] );
  xfree(src_slc);
}


static void
 na_aset_array_index( VALUE self, volatile VALUE idx, volatile VALUE val )
{
  int i, total;
  struct NARRAY *aidx, *src, *dst;
  struct slice *sl;

  GetNArray(self,dst);
  idx = na_cast_object(idx,NA_LINT);
  GetNArray(idx,aidx);
  val = na_cast_unless_narray(val,dst->type);
  GetNArray(val,src);

  /* empty index -- do nothing */
  if (aidx->total==0 && (src->total==0 || src->total==1))
    return;

  /* check rank */
  if (aidx->rank != src->rank)
    rb_raise( rb_eIndexError, "idx.rank=%i != src.rank=%i",
	      aidx->rank, src->rank );
  /* check shape */
  for (i=0;i<src->rank;++i)
    if (aidx->shape[i] != src->shape[i] && src->shape[i] != 1)
      rb_raise( rb_eIndexError, "idx.shape[%i]=%i != src.shape[%i]=%i",
		i, aidx->shape[i], i, src->shape[i] );

  /* make Slice from index */
  sl    = ALLOCA_N(struct slice,2);
  total = na_ary_to_index( NA_STRUCT(idx), dst->total, sl );

  /* 1-dimensionize */
  if (dst->rank > 1) {
    na_flatten_temp(dst);
  }
  if (src->rank > 1) {
    na_flatten_temp(src);
  }

  na_aset_slice( dst, src, sl );
  na_free_slice_index( sl, 1 ); /* free index memory */
}


static void
 na_aset_single_dim(VALUE self, VALUE idx, volatile VALUE val)
{
  int size;
  struct NARRAY *src, *dst;
  struct slice *sl;

  GetNArray(self,dst);
  if (dst->total==0)
    rb_raise(rb_eRuntimeError, "cannot set value to empty array");

  sl   = ALLOCA_N(struct slice, 2);
  size = na_index_test(idx, dst->total, sl);

  if ( size == 1 ) {
    if (NA_IsNArray(val)) {
      GetNArray(val,src);
      if ( src->total == 1 ) {
	SetFuncs[dst->type][src->type](1, NA_PTR(dst,sl->beg),0, src->ptr,0);
	return;
      }
    }
    else if (TYPE(val)!=T_ARRAY) {
      /* Storing single element:
	 a[1] = 1
      */
      SetFuncs[dst->type][NA_ROBJ](1, NA_PTR(dst,sl->beg),0, &val,0);
      return;
    }
    /* Beginning index:
       a[1] = [1,2,3]
    */
    sl[0].n = 0;
    sl[0].step = 1;
  }
  else if ( size == 0 ) return; /* Empty index */

  if ( dst->rank > 1 ) { /* 1-dimensionize */
    na_flatten_temp(dst);
  }
  val = na_cast_unless_narray(val,dst->type);
  GetNArray(val,src);
  na_aset_slice( dst, src, sl );

  na_free_slice_index(sl,1); /* free index memory */
}


static void
 na_aset_multi_dim(VALUE self, int nidx, VALUE *idx, volatile VALUE val)
{
  int    i, pos, size;
  struct NARRAY *dst, *src;
  struct slice *sl;

  GetNArray(self,dst);
  if (dst->total==0)
    rb_raise(rb_eRuntimeError, "cannot set value to empty array");

  /* make Slice from index-argv */
  sl   = ALLOC_N(struct slice, dst->rank+1);
  size = na_index_analysis( nidx, idx, dst, sl );

  if ( size == 0 ) { xfree(sl); return; } /* Empty index */
  if ( size == 1 ) {
    if (NA_IsArray(val)) {
      /* Beginning index:
	   a[2,3,4] = other
       */
      val = na_cast_unless_narray(val,dst->type);
      GetNArray(val,src);
      if (src->total > 1)
	for( i=0; i<src->rank; ++i ) {
	  sl[i].n = 0;
	  sl[i].step = 1;
	}
    }
    else {
      /* Single Element:
         a[2,3,4] = 5
      */
      for ( pos=0, i=dst->rank; i-->0; )
	pos = pos * dst->shape[i] + sl[i].beg;
      SetFuncs[dst->type][NA_ROBJ](1, NA_PTR(dst,pos), 0, &val, 0 );
      xfree(sl);
      return;
    }
  }
  else
    val = na_cast_unless_narray(val,dst->type);
    GetNArray(val,src);

  /* if ( size>1 ) */
    /* Range index:
         a[0..9,0] = other
     */
  na_aset_slice( dst, src, sl );

  na_free_slice_index(sl,nidx); /* free index memory */
  xfree(sl); 
}



static void
 na_aset_fill(VALUE self, volatile VALUE val)
{
  struct NARRAY *dst, *src;
  struct slice *sl;

  GetNArray(self,dst);
  if (dst->total==0)
    rb_raise(rb_eRuntimeError, "cannot set value to empty array");

  if ( NA_IsArray(val) ) {    /* store Array? */
    sl = ALLOC_N(struct slice, dst->rank+1);
    na_set_slice_1obj(dst->rank,sl,dst->shape);

    val = na_cast_unless_narray(val,dst->type);
    GetNArray(val,src);
    na_aset_slice( dst, src, sl );
    xfree(sl);
  }
  else {
    na_fill( self, val );  /* Simple filling */
  }
}


/* --- mask --- */
void
 na_aset_mask(VALUE self, VALUE mask, VALUE val)
{
  int size, step, i;
  struct NARRAY *a1, *am, *a2;

  GetNArray( self, a1 );
  GetNArray( mask, am );

  if (a1->total != am->total)
    rb_raise(rb_eTypeError,"self.size(=%i) != mask.size(=%i)",
	     a1->total, am->total);
  if (a1->rank != am->rank) 
    rb_raise(rb_eTypeError,"self.rank(=%i) != mask.rank(=%i)",
	     a1->rank, am->rank);
  for (i=0; i<a1->rank; ++i)
    if (a1->shape[i] != am->shape[i])
      rb_raise(rb_eTypeError,"self.shape[%i](=%i) != mask.shape[%i](=%i)",
	       i, a1->shape[i], i, am->shape[i]);

  size = na_count_true_body(mask);

  val = na_cast_object(val,a1->type);
  GetNArray( val, a2 );
  if (a2->total == 1) {
    step = 0;
  } else if (a2->total == size) { 
    step = na_sizeof[a2->type];
  } else {
    rb_raise(rb_eTypeError,"val.length != mask.count_true");
  }

  SetMaskFuncs[a1->type]
    ( a1->total, a1->ptr, na_sizeof[a1->type],
      a2->ptr, step, am->ptr, 1 );
}

/* method: []=(idx1,idx2,...,idxN,val) */
VALUE
 na_aset(int nidx, VALUE *idx, VALUE self)
{
  --nidx;

  if (nidx==0) {
    na_aset_fill( self, idx[0] );
  }
  else
  if (nidx==1) {
    if ( NA_IsNArray(idx[0]) ) {
      if( NA_TYPE(idx[0]) == NA_BYTE ) { /* then supposed to be a mask */
	na_aset_mask(self, idx[0], idx[1]);
	return(idx[1]);
      }
    }
    if ( NA_IsArray(idx[0]) ) /* Array Index ? */
      na_aset_array_index( self, idx[0], idx[1] );
    else
      na_aset_single_dim( self, idx[0], idx[1] );
  }
  else
  if (nidx>1) {
    na_aset_multi_dim( self, nidx, idx, idx[nidx] );
  }
  else /* if (nidx<0) */
    rb_raise( rb_eArgError, "No value specified" );

  return idx[nidx];
}


void Init_na_index() {
    /* slice */
    rb_define_method(cNArray, "[]", na_aref,-1);
    rb_define_method(cNArray, "[]=", na_aset,-1);
    rb_define_method(cNArray, "slice", na_slice,-1);
    /* mask */
    rb_define_method(cNArray, "count_false", na_count_false, 0);
    rb_define_method(cNArray, "count_true", na_count_true, 0);
    rb_define_method(cNArray, "mask", na_aref_mask, 1);
}
