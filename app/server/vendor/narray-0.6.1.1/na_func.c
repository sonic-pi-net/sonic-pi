/*
  na_func.c
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

int
 na_max3(int a, int b, int c)
{
  int m;

  if ((a) > (b))
    m = a;
  else
    m = b;
  if ((c) > m)
    m = c;
  return m;
}


void
  na_shape_max3(int ndim, int *max_shp, int *shp1, int *shp2, int *shp3)
{
  int i;

  for (i=0; i<ndim; ++i) {
    max_shp[i] = na_max3(shp1[i], shp2[i], shp3[i]);
  }
}


/* initialize slice structure */
void na_init_slice( struct slice *s, int rank, int *shape, int elmsz )
{
  int r, i, b;
  na_index_t *idx;

  /*
  if (rank<1)
    rb_raise(rb_eRuntimeError,"cannot execute for empty array");
  */

  /* set strides and clear index */
  s[0].stride = 1;
  for (r=1; r<rank; ++r)
    s[r].stride = s[r-1].stride * shape[r-1];

  for (r=0; r<rank; ++r) {
    if ( s[r].idx == NULL )
      /* regular interval */
      s[r].pstep = s[r].step * s[r].stride * elmsz;
    else {
      /* index */
      s[r].pstep = b = s[r].stride * elmsz;
      /* convert index to byte-unit */
      for (i=0; i<16; ++i)
	if ( (1<<i) == b ) { b=i; break; }
      if (i==16)
	for (idx=s[r].idx,i=s[r].n; i-->0; ) { *(idx++)*=b; }
      else
	for (idx=s[r].idx,i=s[r].n; i-->0; ) { *(idx++)<<=b; }
    }
  }

  /* set termination mark */
  s[rank].n = 0;
  s[rank].idx = NULL;

  for (r=rank-1;r>=0;--r) {
    /* set beginning pointers */
    if ( s[r].idx == NULL )
      s[r].pbeg = s[r].stride * s[r].beg * elmsz;
    else
      s[r].pbeg = s[r].idx[0];
  }
}


static void
 na_do_loop_unary( int nd, char *p1, char *p2,
		   struct slice *s1, struct slice *s2, void (*func)() )
{
  int *si;
  int  i;
  int  ps1 = s1[0].pstep;
  int  ps2 = s2[0].pstep;

  i  = nd;
  si = ALLOCA_N(int,nd);
  s1[i].p = p1;
  s2[i].p = p2;

  for(;;) {
    /* set pointers */
    while (i > 0) {
      --i;
      s2[i].p = s2[i].pbeg + s2[i+1].p;
      s1[i].p = s1[i].pbeg + s1[i+1].p;
      si[i] = s1[i].n;
    }
    (*func)(s2[0].n, s1[0].p, ps1, s2[0].p, ps2);
    /* rank up */
    do {
      if ( ++i >= nd ) return;
    } while ( --si[i] == 0 );
    /* next point */
    s1[i].p += s1[i].pstep;
    s2[i].p += s2[i].pstep;
  }
}


static void
 na_do_loop_binary( int nd, char *p1, char *p2, char *p3,
		    struct slice *s1, struct slice *s2, struct slice *s3,
		    void (*func)() )
{
  int i;
  int ps1 = s1[0].pstep;
  int ps2 = s2[0].pstep;
  int ps3 = s3[0].pstep;
  int *si;

  si = ALLOCA_N(int,nd);
  i  = nd;
  s1[i].p = p1;
  s2[i].p = p2;
  s3[i].p = p3;

  for(;;) {
    /* set pointers */
    while (i > 0) {
      --i;
      s3[i].p = s3[i].pbeg + s3[i+1].p;
      s2[i].p = s2[i].pbeg + s2[i+1].p;
      s1[i].p = s1[i].pbeg + s1[i+1].p;
      si[i] = s1[i].n;
    }
    /* rank 0 loop */
    (*func)(s2[0].n, s1[0].p, ps1, s2[0].p, ps2, s3[0].p, ps3);
    /* rank up */
    do {
      if ( ++i >= nd ) return;
    } while ( --si[i] == 0 );
    /* next point */
    s1[i].p += s1[i].pstep;
    s2[i].p += s2[i].pstep;
    s3[i].p += s3[i].pstep;
  }
}



void na_loop_index_ref( struct NARRAY *a1, struct NARRAY *a2,
			struct slice *s1, struct slice *s2, void (*func)() )
{
  char *p1, *p2;
  int nr, i, ii;
  int ps1 = s1[0].pstep;
  int ps2 = s2[0].pstep;
  int *si;
  na_index_t *idx;

  /*
  int copy;
  if (a1->type==a2->type && s1[0].step==1 && s2[0].step==1)
    copy = s1[0].n * na_sizeof[a1->type];
  else
    copy = 0;
  */

  /* Initialize */
  nr = i = a1->rank;
  si = ALLOCA_N(int,nr);
  s1[i].p = a1->ptr;
  s2[i].p = a2->ptr;

  for(;;) {
    /* set pointers */
    while (i > 0) {
      --i;
      s2[i].p = s2[i].pbeg + s2[i+1].p;
      s1[i].p = s1[i].pbeg + s1[i+1].p;
      si[i] = 0;
    }

    /* rank 0 loop */
    if ( s2[0].idx == NULL ) {
      /* regular interval */
      /*
      if (copy) {
	memcpy(s1[0].p, s2[0].p, copy);
      } else
      */
      (*func)(s2[0].n, s1[0].p, ps1, s2[0].p, ps2);
    } else {
      /* a2 has index */
      p1 = s1[0].p;
      p2 = s2[1].p;
      for ( idx=s2[0].idx, ii=s2[0].n; ii-->0;) {
	(*func)( 1, p1, 0, p2+*(idx++), 0 );
	p1 += ps1;
      }
    }
    /* rank up */
    do {
      if ( ++i >= nr ) return;
    } while ( ++si[i] == s1[i].n );
    /* next point */
    s1[i].p += s1[i].pstep;
    /* array2 may have index */
    if ( s2[i].idx == NULL )
      s2[i].p += s2[i].pstep;
    else
      s2[i].p = s2[i+1].p + s2[i].idx[si[i]]; /* * s2[i].pstep; */
  }
}


void na_loop_general( struct NARRAY *a1, struct NARRAY *a2,
		      struct slice *s1, struct slice *s2, void (*func)() )
{
  char *p1, *p2;
  int nr, i, ii;
  int ps1 = s1[0].pstep;
  int ps2 = s2[0].pstep;
  int *si;
  na_index_t *idx1, *idx2;

  /* Initialize */
  nr = i = a1->rank;
  si = ALLOCA_N(int,nr);
  s1[i].p = a1->ptr;
  s2[i].p = a2->ptr;

  for(;;) {
    /* set pointers */
    while (i > 0) {
      --i;
      s2[i].p = s2[i].pbeg + s2[i+1].p;
      s1[i].p = s1[i].pbeg + s1[i+1].p;
      si[i] = 0;
    }

    /* rank 0 loop */
    if ( s1[0].idx == NULL ) {
      if ( s2[0].idx == NULL ) {
	/* normal interval */
	(*func)(s2[0].n, s1[0].p, ps1, s2[0].p, ps2);
      } else {
	/* s2 has index */
	p1 = s1[0].p;
	p2 = s2[1].p;
	for ( idx2=s2[0].idx, ii=s2[0].n; ii-->0; ) {
	  (*func)( 1, p1, 0, p2+*(idx2++), 0 );
	  p1 += ps1;
	}
      }
    } else {
      if ( s2[0].idx == NULL ) {
	/* s1 has index */
	p1 = s1[1].p;
	p2 = s2[0].p;
	for ( idx1=s1[0].idx, ii=s2[0].n; ii-->0; ) {
	  (*func)( 1, p1+*(idx1++), 0, p2, 0 );
	  p2 += ps2;
	}
      } else {
	/* s1 & s2 has index */
	p1 = s1[1].p;
	p2 = s2[1].p;
	for ( idx1=s1[0].idx, idx2=s2[0].idx, ii=s2[0].n; ii-->0; ) {
	  (*func)( 1, p1+*(idx1++), 0, p2+*(idx2++), 0 );
	}
      }
    }

    /* rank up */
    do {
      if ( ++i >= nr ) return;
    } while ( ++si[i] == s1[i].n );

    /* next point for a1 */
    if ( s1[i].idx == NULL )
      s1[i].p += s1[i].pstep;
    else
      s1[i].p = s1[i+1].p + s1[i].idx[si[i]];
    /* next point for a2 */
    if ( s2[i].idx == NULL )
      s2[i].p += s2[i].pstep;
    else
      s2[i].p = s2[i+1].p + s2[i].idx[si[i]];
  }
}


void
 na_shape_copy( int ndim, int *shape, struct NARRAY *a )
{
  int i;

  for (i=0; i<a->rank; ++i)
    shape[i] = a->shape[i];
  for (   ; i<ndim; ++i)
    shape[i] = 1;
}


void
 na_set_slice_1obj( int ndim, struct slice *slc, int *shape )
{
  int i;

  /* for normal access */
  for (i=0; i<ndim; ++i) {
    slc[i].n    = shape[i];
    slc[i].beg  = 0;
    slc[i].step = 1;
    slc[i].idx  = NULL;
  }
}



static int
 na_set_slice_2obj( int ndim, struct slice *s1, struct slice *s2,
		    int *shp1, int *shp2 )
{
  int i, j;

  for (i=j=0; i<ndim; ++i) {

    if ( shp1[i]==1 && shp2[i]>1 ) {
      s1[j].n    =
      s2[j].n    = shp2[i];
      s1[j].step = 0;
      s2[j].step = 1;
    } else
    if ( shp2[i]==1 && shp1[i]>1 ) {
      s1[j].n    =
      s2[j].n    = shp1[i];
      s1[j].step = 1;
      s2[j].step = 0;
    } else
    if ( shp1[i] == shp2[i] ) {
      s1[j].n    =
      s2[j].n    = shp1[i];
      s1[j].step = 1;
      s2[j].step = 1;
    } else
      rb_raise(rb_eRuntimeError, "Array size mismatch: %i != %i in %i-th dim",
	       shp1[i], shp2[i], i);

    if (j<i) {
      shp1[j] = shp1[i];
      shp2[j] = shp2[i];
    }

    if (j>0)
      if ( s1[j].step == s1[j-1].step &&
	   s2[j].step == s2[j-1].step   ) {   /* contract dimension */
	s1[j-1].n  =
	s2[j-1].n *= s2[j].n;
	shp1[j-1] *= shp1[j];
	shp2[j-1] *= shp2[j];
	continue;
      }
    s1[j].beg =
    s2[j].beg = 0;
    s1[j].idx =
    s2[j].idx = NULL;
    ++j;
  }

  return j;
}


static int
 na_set_slice_check(int ary_sz, int itr_sz, int i)
{
  if ( ary_sz == itr_sz )
    return 1;
  else if ( ary_sz == 1 )
    return 0;
  else
    rb_raise(rb_eRuntimeError, "Array size mismatch: %i != %i at %i-th dim",
	     ary_sz, itr_sz, i);
}


int
 na_set_slice_3obj( int ndim,
		    struct slice *s1, struct slice *s2, struct slice *s3,
		    int *shp1, int *shp2, int *shp3, int *shape )
{
  int i, j;

  /* for repetitous access */
  for (i=j=0; i<ndim; ++i) {

    s1[j].step = na_set_slice_check(shp1[i],shape[i],i);
    s2[j].step = na_set_slice_check(shp2[i],shape[i],i);
    s3[j].step = na_set_slice_check(shp3[i],shape[i],i);

    if (j<i) {
      shp1[j] = shp1[i];
      shp2[j] = shp2[i];
      shp3[j] = shp3[i];
    }

    if (j>0) {
      if ( s1[j].step == s1[j-1].step &&
	   s2[j].step == s2[j-1].step &&
	   s3[j].step == s3[j-1].step   )   /* contract dimension */
      {
	s1[j-1].n =
	s2[j-1].n =
	s3[j-1].n *= shape[i];

	shp1[j-1] *= shp1[j];
	shp2[j-1] *= shp2[j];
	shp3[j-1] *= shp3[j];
	continue;
      }
    }

    s1[j].n   =
    s2[j].n   =
    s3[j].n   = shape[i];

    s1[j].beg =
    s2[j].beg =
    s3[j].beg = 0;

    s1[j].idx =
    s2[j].idx =
    s3[j].idx = NULL;

    ++j;
  }

  return j;
}


static void
 na_exec_unary(struct NARRAY *a1, struct NARRAY *a2, void (*func)())
{
  int  ndim;
  int *shp1, *shp2;
  struct slice *s1, *s2;

  /* empty check */
  if ( a1->total==0 || a2->total==0 )
    /* rb_raise( rb_eTypeError, "cannot execute for empty array" ); */
    return; /* do nothing */

  ndim = NA_MAX(a1->rank,a2->rank);

  NA_ALLOC_SLICE(s1,(ndim+1)*2,shp1,ndim*2);
  shp2 = &shp1[ndim];
  s2   = &s1[ndim+1];

  na_shape_copy( ndim, shp1, a1 );
  na_shape_copy( ndim, shp2, a2 );

  ndim = na_set_slice_2obj( ndim, s1, s2, shp1, shp2 );
  na_init_slice( s1, ndim, shp1, na_sizeof[a1->type] );
  na_init_slice( s2, ndim, shp2, na_sizeof[a2->type] );

  na_do_loop_unary( ndim, a1->ptr, a2->ptr, s1, s2, func );

  xfree(s1);
}


/* a1 and/or a2 and/or a3 have extensible index */
static void
 na_exec_binary( struct NARRAY *a1, struct NARRAY *a2,
		 struct NARRAY *a3, void (*func)() )
{
  int   ndim;
  int  *itr, *shp1, *shp2, *shp3;
  struct slice *s1, *s2, *s3;

  /* empty check */
  if (a1->total==0) return; /* do nothing */

  ndim = na_max3(a1->rank, a2->rank, a3->rank);

  NA_ALLOC_SLICE(s1,(ndim+1)*3,shp1,ndim*4);
  shp2 = &shp1[ndim];
  shp3 = &shp2[ndim];
  itr  = &shp3[ndim];
  s2   = &s1[ndim+1];
  s3   = &s2[ndim+1];

  na_shape_copy( ndim, shp1, a1 );
  na_shape_copy( ndim, shp2, a2 );
  na_shape_copy( ndim, shp3, a3 );
  na_shape_max3( ndim, itr, shp1, shp2, shp3 );

  ndim = na_set_slice_3obj( ndim, s1, s2, s3, shp1, shp2, shp3, itr );

  na_init_slice(s1, ndim, shp1, na_sizeof[a1->type] );
  na_init_slice(s2, ndim, shp2, na_sizeof[a2->type] );
  na_init_slice(s3, ndim, shp3, na_sizeof[a3->type] );

  na_do_loop_binary( ndim, a1->ptr, a2->ptr, a3->ptr, s1, s2, s3, func );
  xfree(s1);
}


static void
 na_shape_max_2obj(int ndim, int *shape, struct NARRAY *a1, struct NARRAY *a2)
{
  struct NARRAY *tmp;
  int  i;

  /* empty check */
  if ( a1->total==0 || a2->total==0 )
    rb_raise( rb_eTypeError, "cannot execute for empty array" );

  if (a1->rank < a2->rank) {
    NA_SWAP(a1,a2,tmp);
  }

  for (i=0; i<a2->rank; ++i) {
    shape[i] = NA_MAX(a1->shape[i],a2->shape[i]);
  }
  for (   ; i<a1->rank; ++i) {
    shape[i] = a1->shape[i];
  }
  for (   ; i<ndim; ++i) {
    shape[i] = 1;
  }
}


static VALUE
 na_make_object_extend(struct NARRAY *a1, struct NARRAY *a2,
		       int type, VALUE klass)
{
  int  ndim;
  int *shape;

  /* empty check */
  if ( a1->total==0 || a2->total==0 )
    return na_make_empty(type, klass); /* return empty */

  ndim  = NA_MAX(a1->rank, a2->rank);
  shape = ALLOCA_N(int, ndim);
  na_shape_max_2obj( ndim, shape, a1, a2 );

  return na_make_object( type, ndim, shape, klass );
}


static ID na_bifunc_to_id(na_bifunc_t funcs)
{
  if (funcs==AddBFuncs)	return na_id_add;
  if (funcs==SbtBFuncs)	return na_id_sbt;
  if (funcs==MulBFuncs)	return na_id_mul;
  if (funcs==DivBFuncs)	return na_id_div;
  if (funcs==ModBFuncs)	return na_id_mod;
  return 0;
  /* if (funcs==PowFuncs)	return na_id_power;
     rb_raise(rb_eRuntimeError, "coerce_rev: function not soppurted");
  */
}


static VALUE
 na_bifunc_class(VALUE klass1, VALUE klass2)
{
  if ( klass2==cNArray || klass2==cNArrayScalar ) {
    if ( klass1==cNArrayScalar ) return cNArray;
    else return klass1;
  }
  return Qnil;
}


static VALUE
 na_bifunc(VALUE obj1, VALUE obj2, VALUE klass, na_bifunc_t funcs)
{
  VALUE obj3;
  ID id;
  int type;

  Check_Type(obj1, T_DATA);
  obj2 = na_upcast_object(obj2,NA_STRUCT(obj1)->type);
  obj1 = na_upcast_type(obj1,type=NA_STRUCT(obj2)->type);

  if (klass==Qnil) {
    klass = na_bifunc_class(CLASS_OF(obj1),CLASS_OF(obj2));

    if (klass==Qnil) { /* coerce_rev */
      if ((id=na_bifunc_to_id(funcs))!=0)
	return rb_funcall( obj2, na_id_coerce_rev, 2, obj1, ID2SYM(id) );
      else
	klass = cNArray;
    }
  }

  obj3 = na_make_object_extend(NA_STRUCT(obj1),NA_STRUCT(obj2),type,klass);

  na_exec_binary( NA_STRUCT(obj3), NA_STRUCT(obj1), NA_STRUCT(obj2),
		  funcs[type] );

  return obj3;
}


static VALUE
 na_power(VALUE val1, VALUE val2)
{
  volatile VALUE obj1, obj2, obj3;
  struct NARRAY *a1, *a2;

  obj1 = val1;
  obj2 = val2;
  GetNArray(obj1,a1);
  obj2 = na_to_narray(obj2);
  GetNArray(obj2,a2);

  if (a1->type==NA_ROBJ && a2->type!=NA_ROBJ) {
    obj2 = na_change_type(obj2,NA_ROBJ);
    GetNArray(obj2,a2);
  } else
  if (a2->type==NA_ROBJ && a1->type!=NA_ROBJ) {
    obj1 = na_change_type(obj1,NA_ROBJ);
    GetNArray(obj1,a1);
  } else
  if (!NA_IsCOMPLEX(a1) && NA_IsCOMPLEX(a2)) {
    obj1 = na_upcast_type(obj1,a2->type);
    GetNArray(obj1,a1);
  }

  obj3 = na_make_object_extend( a1, a2, na_upcast[a1->type][a2->type],
				CLASS_OF(obj1) );

  na_exec_binary( NA_STRUCT(obj3), a1, a2,
		  PowFuncs[a1->type][a2->type] );

  return obj3;
}


static VALUE
 na_set_func(VALUE obj1, volatile VALUE obj2, na_ufunc_t funcs)
{
  struct NARRAY *a1;

  GetNArray(obj1,a1);
  obj2 = na_cast_object(obj2,a1->type);

  na_exec_unary( NA_STRUCT(obj1), NA_STRUCT(obj2), funcs[a1->type] );

  return obj1;
}


static VALUE
 na_imag_set(VALUE obj1, volatile VALUE obj2)
{
  struct NARRAY *a1;

  GetNArray(obj1,a1);
  obj2 = na_cast_object(obj2, na_cast_real[a1->type]);

  na_exec_unary( NA_STRUCT(obj1), NA_STRUCT(obj2), ImgSetFuncs[a1->type] );

  return obj1;
}


static VALUE
 na_unary_func(VALUE self, const int *cast, na_ufunc_t funcs)
{
  VALUE ans;
  struct NARRAY *a2;

  GetNArray(self,a2);
  ans = na_make_object(cast[a2->type], a2->rank, a2->shape, CLASS_OF(self));

  na_exec_unary( NA_STRUCT(ans), a2, funcs[a2->type] );
  return ans;
}



/* local function for comparison */
static VALUE
 na_compare_func(VALUE self, VALUE other, na_bifunc_t funcs)
{
  VALUE ans;
  int type;

  Check_Type(self, T_DATA);
  /*if (NA_IsComplex(a1)) rb_raise();*/
  other = na_upcast_object(other,NA_STRUCT(self)->type);
  self = na_upcast_type(self,type=NA_STRUCT(other)->type);

  ans = na_make_object_extend( NA_STRUCT(self), NA_STRUCT(other),
			       NA_BYTE, cNArray );

  na_exec_binary( NA_STRUCT(ans), NA_STRUCT(self), NA_STRUCT(other),
		  funcs[type] );
  return ans;
}


/* method: self + other */
static VALUE na_add(VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, Qnil, AddBFuncs ); }

/* method: self - other */
static VALUE na_sbt(VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, Qnil, SbtBFuncs ); }

/* method: self * other */
static VALUE na_mul(VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, Qnil, MulBFuncs ); }

/* method: self / other */
static VALUE na_div(VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, Qnil, DivBFuncs ); }

/* method: self / other */
static VALUE na_mod(VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, Qnil, ModBFuncs ); }

/* method: self & other */
static VALUE na_bit_and(VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, Qnil, BAnFuncs ); }

/* method: self | other */
static VALUE na_bit_or(VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, Qnil, BOrFuncs ); }

/* method: self ^ other */
static VALUE na_bit_xor(VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, Qnil, BXoFuncs ); }

/* method: atan2(y,x) */
static VALUE na_math_atan2(VALUE module, volatile VALUE y, volatile VALUE x)
{
  VALUE ans;
  struct NARRAY *ya, *xa, *aa;

  if (TYPE(y) == T_ARRAY) {
    y = na_ary_to_nary(y,cNArray);
  } else
  if (!IsNArray(y)) {
    y = na_make_scalar(y,na_object_type(y));
  }

  if (TYPE(x) == T_ARRAY) {
    x = na_ary_to_nary(x,cNArray);
  } else
  if (!IsNArray(x)) {
    x = na_make_scalar(x,na_object_type(x));
  }

  GetNArray(y,ya);
  GetNArray(x,xa);
  if (NA_IsINTEGER(ya) && NA_IsINTEGER(xa)) {
    y = na_upcast_type(y,NA_DFLOAT);
    x = na_upcast_type(x,NA_DFLOAT);
  }

  ans = na_bifunc( y, x, Qnil, atan2Funcs );
  GetNArray(ans,aa);

  if (CLASS_OF(y) == cNArrayScalar && CLASS_OF(x) == cNArrayScalar)
    SetFuncs[NA_ROBJ][aa->type](1,&ans,0,aa->ptr,0);

  return ans;
}



/* singleton method: NArray.mul( obj1, obj2 ) */
static VALUE
 na_s_mul(VALUE klass, VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, klass, MulBFuncs ); }

/* singleton method: NArray.div( obj1, obj2 ) */
static VALUE
 na_s_div(VALUE klass, VALUE obj1, VALUE obj2)
{ return na_bifunc( obj1, obj2, klass, DivBFuncs ); }



/* method: self.add!(other) */
static VALUE na_add_bang(VALUE obj1, VALUE obj2)
{ return na_set_func( obj1, obj2, AddUFuncs ); }

/* method: self.sbt!(other) */
static VALUE na_sbt_bang(VALUE obj1, VALUE obj2)
{ return na_set_func( obj1, obj2, SbtUFuncs ); }

/* method: self.div!(other) */
static VALUE na_div_bang(VALUE obj1, VALUE obj2)
{ return na_set_func( obj1, obj2, DivUFuncs ); }

/* method: self.mul!(other) */
static VALUE na_mul_bang(VALUE obj1, VALUE obj2)
{ return na_set_func( obj1, obj2, MulUFuncs ); }

/* method: self.mod!(other) */
static VALUE na_mod_bang(VALUE obj1, VALUE obj2)
{ return na_set_func( obj1, obj2, ModUFuncs ); }

/* method: self.conj! */
static VALUE na_conj_bang(VALUE self)
{ return na_set_func( self, self, ConjFuncs ); }


/* method: self.swap_byte */
static VALUE na_swap_byte(VALUE self)
{ return na_unary_func( self, na_no_cast, SwpFuncs ); }

/* method: self.hton , self.ntoh */
static VALUE na_hton(VALUE self)
{ return na_unary_func( self, na_no_cast, H2NFuncs ); }

/* method: self.htov , self.vtoh */
static VALUE na_htov(VALUE self)
{ return na_unary_func( self, na_no_cast, H2VFuncs ); }

/* method: ~self */
static VALUE na_bit_rev(VALUE self)
{ return na_unary_func( self, na_no_cast, BRvFuncs ); }

/* method: -self */
static VALUE na_neg(VALUE self)
{ return na_unary_func( self, na_no_cast, NegFuncs ); }

/* method: self.recip */
static VALUE na_recip(VALUE self)
{ return na_unary_func( self, na_no_cast, RcpFuncs ); }

/* method: self.abs */
static VALUE na_abs(VALUE self)
{ return na_unary_func( self, na_cast_real, AbsFuncs ); }

/* method: self.real */
static VALUE na_real(VALUE self)
{ return na_unary_func( self, na_cast_real, RealFuncs ); }

/* method: self.imag */
static VALUE na_imag(VALUE self)
{ return na_unary_func( self, na_cast_real, ImagFuncs ); }

/* method: self.imag */
static VALUE na_angle(VALUE self)
{ return na_unary_func( self, na_cast_real, AnglFuncs ); }

/* method: self.im */
static VALUE na_imag_mul(VALUE self)
{ return na_unary_func( self, na_cast_comp, ImagMulFuncs ); }

/* method: self.conj */
static VALUE na_conj(VALUE self)
{ return na_unary_func( self, na_no_cast, ConjFuncs ); }

/* method: self.floor */
static VALUE na_floor(VALUE self)
{ return na_unary_func( self, na_cast_round, FloorFuncs ); }

/* method: self.ceil */
static VALUE na_ceil(VALUE self)
{ return na_unary_func( self, na_cast_round, CeilFuncs ); }

/* method: self.round */
static VALUE na_round(VALUE self)
{ return na_unary_func( self, na_cast_round, RoundFuncs ); }

/* method: self.not */
static VALUE na_not(VALUE self)
{ return na_unary_func( self, na_cast_byte, NotFuncs ); }


/* method: self.and other */
static VALUE
 na_cond_and(VALUE obj1, VALUE obj2)
{ return na_compare_func( obj1, obj2, AndFuncs ); }

/* method: self.or other */
static VALUE
 na_cond_or(VALUE obj1, VALUE obj2)
{ return na_compare_func( obj1, obj2, Or_Funcs ); }

/* method: self.xor other */
static VALUE
 na_cond_xor(VALUE obj1, VALUE obj2)
{ return na_compare_func( obj1, obj2, XorFuncs ); }



/* method: self <=> other */
static VALUE
 na_compare(VALUE obj1, VALUE obj2)
{ return na_compare_func( obj1, obj2, CmpFuncs ); }



/* method: self.eq(other) */
static VALUE
 na_equal(VALUE obj1, VALUE obj2)
{
  return na_compare_func( obj1, obj2, EqlFuncs );
}

/* method: self.ne(other) */
static VALUE
 na_not_equal(VALUE obj1, VALUE obj2)
{
  VALUE obj;
  int  i;  char *p;
  struct NARRAY *a;

  obj = na_compare_func( obj1, obj2, EqlFuncs );
  GetNArray(obj,a);
  p = a->ptr;
  for( i=a->total; i-->0; ) {
    *p = *p==0 ? 1 : 0;
    ++p;
  }
  return obj;
}

/* method: self > other */
static VALUE
 na_greater_than(VALUE self, VALUE obj2)
{
  int  i;  char *p;
  struct NARRAY *a;

  self = na_compare_func( self, obj2, CmpFuncs );
  GetNArray(self,a);
  p = a->ptr;
  for( i=a->total; i-->0; ) {
    if (*p!=1) *p=0;
    ++p;
  }
  return self;
}

/* method: self >= other */
static VALUE
 na_greater_equal(VALUE obj1, VALUE obj2)
{
  VALUE obj;
  int  i;  char *p;
  struct NARRAY *a;

  obj = na_compare_func( obj1, obj2, CmpFuncs );
  GetNArray(obj,a);
  p = a->ptr;
  for( i=a->total; i-->0; ) {
    if (*p==1 || *p==0) *p=1;
    else *p=0;
    ++p;
  }
  return obj;
}

/* method: self < other */
static VALUE
 na_less_than(VALUE obj1, VALUE obj2)
{
  VALUE obj;
  int  i;  char *p;
  struct NARRAY *a;

  obj = na_compare_func( obj1, obj2, CmpFuncs );
  GetNArray(obj,a);
  p = a->ptr;
  for( i=a->total; i-->0; ) {
    if (*p==2) *p=1;
    else *p=0;
    ++p;
  }
  return obj;
}

/* method: self <= other */
static VALUE
 na_less_equal(VALUE obj1, VALUE obj2)
{
  VALUE obj;
  int  i;  char *p;
  struct NARRAY *a;

  obj = na_compare_func( obj1, obj2, CmpFuncs );
  GetNArray(obj,a);
  p = a->ptr;
  for( i=a->total; i-->0; ) {
    if (*p==2 || *p==0) *p=1;
    else *p=0;
    ++p;
  }
  return obj;
}




/*
 ------- Sum, Min, Max, Transpose --------
*/
VALUE
 rb_range_beg_len(VALUE range, long *begp, long *lenp, long len, int err );

static int
 na_arg_to_rank(int argc, VALUE *argv, int rankc, int *rankv, int flag)
/* e.g.: argv=[1,3..5]
	if flag==0
	  rankv = [0,1,0,1,1,1,0,..]
	else
	  rankv = [1,3,4,5]
*/
{
  int i, j, c=0;
  long r, n;
  VALUE v;
  volatile VALUE s;

  if (flag==0)
    MEMZERO(rankv,int,rankc);

  for (i=0;i<argc;++i) {
    if ( c >= rankc )
      rb_raise(rb_eArgError, "too many ranks");

    v = argv[i];

    if (TYPE(v)==T_FIXNUM) {
      r = NUM2INT(v);
      if (r<0) r += rankc;     /* negative for from end */
      if (r<0 || r>=rankc)
        rb_raise(rb_eArgError, "rank %ld out of range", r);
      if (flag)
	rankv[c] = r;
      else
	rankv[r] = 1;
      ++c;
    }
    else
    if (CLASS_OF(v)==rb_cRange) {
      rb_range_beg_len( v, &r, &n, rankc, 1 );
      if ( c+n > rankc ) {
        s = rb_inspect(v);
        rb_raise(rb_eArgError,"invalid dimension range: %s",StringValueCStr(s));
      }
      if (flag) {
	for(j=0; j<n; ++j)
	  rankv[c++] = r++;
      } else {
	for(j=0; j<n; ++j) {
	  rankv[r++] = 1;
	  ++c;
	}
      }
    }
    else
      rb_raise(rb_eArgError, "wrong type");
  }
  return c;
}



/*  Transpose procedure  */
static struct NARRAY *
 na_transpose_bifunc(struct NARRAY *a1, struct NARRAY *a2, int *trans)
{
  int  i, ndim=a2->rank;
  struct slice *s1, *s2;

  s1 = ALLOC_N(struct slice, (ndim+1)*2);
  s2 = &s1[ndim+1];

  /* for Source array -- s1 is temporarily used */
  na_set_slice_1obj(a2->rank,s1,a2->shape);
  na_init_slice( s1, ndim, a2->shape, na_sizeof[a2->type] );

  /* Transpose Slice */
  for (i=0; i<ndim; ++i)
    s2[i] = s1[trans[i]];
  s2[ndim] = s1[ndim];

  /* for Destination */
  na_set_slice_1obj(a1->rank,s1,a1->shape);
  na_init_slice( s1, ndim, a1->shape, na_sizeof[a1->type] );

  /* Loop */
  na_do_loop_unary( ndim, a1->ptr, a2->ptr, s1, s2,
		    SetFuncs[a1->type][a2->type] );
  xfree(s1);
  return a1;
}


/* method: self.transpose( ... ) */
static VALUE
 na_transpose(int argc, VALUE *argv, VALUE self)
{
  struct NARRAY *a2;
  int i, rankc, *rankv, *shape;
  VALUE obj;

  GetNArray(self,a2);

  /* Parse Argument */
  rankv = ALLOC_N( int, a2->rank*2 );
  shape = &rankv[a2->rank];
  rankc = na_arg_to_rank( argc, argv, a2->rank, rankv, 1 );
  for ( ;rankc<a2->rank; ++rankc)
    rankv[rankc] = rankc;

  /* Argument Check */
  MEMZERO(shape,int,rankc);
  for (i=0; i<rankc; ++i) {
    if (shape[rankv[i]] != 0)
      rb_raise(rb_eArgError,"rank doublebooking");
    shape[rankv[i]] = 1;
  }

  for (i=0; i<a2->rank; ++i)
    shape[i] = a2->shape[rankv[i]];

  obj = na_make_object(a2->type, a2->rank, shape, CLASS_OF(self));

  na_transpose_bifunc( NA_STRUCT(obj), a2, rankv );
  xfree(rankv);
  return obj;
}




static void
 na_accum_set_shape(int *itr_shape, int rank, int *ary_shape,
		    int rankc, int *rankv)
{
  int i;

  if (rankc==0) {
    /* Accumulate all elements */
    for (i=0; i<rank; ++i) {
      itr_shape[i] = 1;
      rankv[i] = 1;
    }
  } else {
    /* Select Accumulate ranks */
    for (i=0; i<rank; ++i) {
      if (rankv[i]==1)
	itr_shape[i] = 1;
      else
	itr_shape[i] = ary_shape[i];
    }
  }
}


static void
 na_zero_obj(struct NARRAY *ary)
{
  int i;
  VALUE zero = INT2FIX(0);
  VALUE *v = (VALUE*)ary->ptr;

  for (i=ary->total; i>0; --i)
    *(v++) = zero;
}

static void
 na_zero_data(struct NARRAY *ary)
{
  if (ary->type==NA_ROBJ)
    na_zero_obj(ary);
  else
    na_clear_data(ary);
}

static VALUE
 na_sum_body(int argc, VALUE *argv, VALUE self, int flag)
{
  int *shape, rankc, *rankv, cl_dim;
  struct NARRAY *a1, *a2;
  VALUE obj, klass;

  GetNArray(self,a1);

  rankv = ALLOC_N(int,a1->rank*2);
  rankc = na_arg_to_rank( argc, argv, a1->rank, rankv, 0 );

  shape = &rankv[a1->rank];
  na_accum_set_shape( shape, a1->rank, a1->shape, rankc, rankv );

  klass  = CLASS_OF(self);
  cl_dim = na_class_dim(klass);
  if (flag==0 && cl_dim>0 && na_shrink_class(cl_dim,rankv))
    klass = cNArray;

  obj = na_make_object(a1->type,a1->rank,shape,klass);
  GetNArray(obj,a2);

  na_zero_data(a2);
  na_exec_unary( a2, a1, AddUFuncs[a1->type] );

  if (flag==0)
    obj = na_shrink_rank(obj,cl_dim,rankv);

  xfree(rankv);
  return obj;
}

/* method: sum( rank, ... ) */
static VALUE
 na_sum(int argc, VALUE *argv, VALUE self)
{ return na_sum_body(argc,argv,self,0); }

/* method: accum( rank, ... ) */
static VALUE
 na_accum(int argc, VALUE *argv, VALUE self)
{ return na_sum_body(argc,argv,self,1); }



static VALUE
 na_prod_body(int argc, VALUE *argv, VALUE self, int flag)
{
  int *shape, rankc, *rankv, cl_dim;
  struct NARRAY *a1, *a2;
  VALUE obj, klass;
  int32_t one = 1;

  GetNArray(self,a1);

  rankv = ALLOC_N(int,a1->rank*2);
  rankc = na_arg_to_rank( argc, argv, a1->rank, rankv, 0 );

  shape = &rankv[a1->rank];
  na_accum_set_shape( shape, a1->rank, a1->shape, rankc, rankv );

  klass  = CLASS_OF(self);
  cl_dim = na_class_dim(klass);
  if (flag==0 && cl_dim>0 && na_shrink_class(cl_dim,rankv))
    klass = cNArray;

  obj = na_make_object(a1->type,a1->rank,shape,klass);
  GetNArray(obj,a2);

  SetFuncs[a2->type][NA_LINT](a2->total, a2->ptr, na_sizeof[a2->type], &one, 0);

  na_exec_unary( a2, a1, MulUFuncs[a1->type] );

  if (flag==0)
    obj = na_shrink_rank(obj,cl_dim,rankv);

  xfree(rankv);
  return obj;
}

/* method: prod( rank, ... ) */
static VALUE
 na_prod(int argc, VALUE *argv, VALUE self)
{ return na_prod_body(argc,argv,self,0); }


static VALUE
 na_mul_add_body(int argc, VALUE *argv, volatile VALUE self, volatile VALUE other,
		 VALUE wrap_klass, int flag)
{
  VALUE ans, op_klass;
  int  rank, cl_dim;
  int *dst_shape, *max_shape;
  int  rankc, *rankv;
  int  type;
  struct NARRAY *a1, *a2;

  GetNArray(self,a1);
  other = na_upcast_object(other,a1->type);
  GetNArray(other,a2);
  self  = na_upcast_type(self,type=a2->type);
  GetNArray(self,a1);

  rank = NA_MAX(a1->rank,a2->rank);

  rankv = ALLOC_N(int,rank*3);
  rankc = na_arg_to_rank( argc, argv, rank, rankv, 0 );

  max_shape = &rankv[rank];
  na_shape_max_2obj(rank,max_shape,a1,a2);

  dst_shape = &max_shape[rank];
  na_accum_set_shape( dst_shape, rank, max_shape, rankc, rankv );

  op_klass = na_bifunc_class(CLASS_OF(self),CLASS_OF(other));
  if (op_klass==Qnil) /* coerce_rev --- unsupported */
    op_klass = cNArray;

  cl_dim = na_class_dim(op_klass);
  if (flag==0 && cl_dim>0 && na_shrink_class(cl_dim,rankv))
    op_klass = cNArray;

  ans = na_make_object( type, rank, dst_shape,
			(wrap_klass==Qnil) ? op_klass : wrap_klass);

  na_zero_data( NA_STRUCT(ans) );
  na_exec_binary( NA_STRUCT(ans), a1, a2, MulAddFuncs[type] );

  if (flag==0)
    ans = na_shrink_rank(ans,cl_dim,rankv);

  xfree(rankv);
  return ans;
}



/* method: mul_add( other, rank, ... ) */
static VALUE
 na_mul_add(int argc, VALUE *argv, VALUE self)
{
  if (argc<2)
    rb_raise(rb_eArgError, "wrong # of arguments (%d for >=2)", argc);
  return na_mul_add_body(argc-1,argv+1,self,argv[0],Qnil,0);
}

/* method: mul_accum( other, rank, ... ) */
static VALUE
 na_mul_accum(int argc, VALUE *argv, VALUE self)
{
  if (argc<2)
    rb_raise(rb_eArgError, "wrong # of arguments (%d for >=2)", argc);
  return na_mul_add_body(argc-1,argv+1,self,argv[0],Qnil,1);
}

/* singleton method: NArray.mul_add( obj1, obj2, rank, ... ) */
static VALUE
 na_s_mul_add(int argc, VALUE *argv, VALUE klass)
{
  if (argc<3)
    rb_raise(rb_eArgError, "wrong # of arguments (%d for >=3)", argc);
  return na_mul_add_body(argc-2,argv+2,argv[0],argv[1],klass,0);
}


/* cumsum!
 [1 2 3 4 5] -> [1 3 6 10 15]
*/
static VALUE
  na_cumsum_bang(VALUE self)
{
  struct NARRAY *a;
  int step;

  GetNArray(self,a);

  if ( a->rank != 1 )
    rb_raise( rb_eTypeError, "only for 1-dimensional array" );
  if ( a->total < 2 )
    return self; /* do nothing */

  step = na_sizeof[a->type];
  AddUFuncs[a->type](a->total-1, a->ptr+step,step, a->ptr,step);

  return self;
}

/* cumsum */
static VALUE
  na_cumsum(VALUE self)
{
  return na_cumsum_bang(na_clone(self));
}


/* cumprod!
 [1 2 3 4 5] -> [1 3 6 10 15]
*/
static VALUE
  na_cumprod_bang(VALUE self)
{
  struct NARRAY *a;
  int step;

  GetNArray(self,a);

  if ( a->rank != 1 )
    rb_raise( rb_eTypeError, "only for 1-dimensional array" );
  if ( a->total < 2 )
    return self; /* do nothing */

  step = na_sizeof[a->type];
  MulUFuncs[a->type](a->total-1, a->ptr+step,step, a->ptr,step);

  return self;
}

/* cumprod */
static VALUE
  na_cumprod(VALUE self)
{
  return na_cumprod_bang(na_clone(self));
}


/*  Copy element of idx=0  from a2 to a1, as start of accumulation */
/*   a1->rank <= a2->rank is assumed  */
static void
 na_minmax_copy0(struct NARRAY *a1, struct NARRAY *a2)
{
  int  i, ndim=a2->rank; /* a2 has larger rank */
  struct slice *s1, *s2;

  /* Allocate Structure */
  s1 = ALLOC_N(struct slice, (ndim+1)*2);
  s2 = &s1[ndim+1];

  na_set_slice_1obj(a1->rank,s1,a1->shape);
  for (i=0; i<ndim; ++i) {
    s2[i].n    = a1->shape[i]; /* no-repeat if a1->shape[i]==1 */
    s2[i].beg  = 0;	       /* copy idx=0 */
    s2[i].step = 1;
    s2[i].idx  = NULL;
  }

  /* Initialize */
  na_init_slice(s1, ndim, a1->shape, na_sizeof[a1->type] );
  na_init_slice(s2, ndim, a2->shape, na_sizeof[a2->type] );
  /* Loop */
  na_do_loop_unary( ndim, a1->ptr, a2->ptr, s1, s2,
		    SetFuncs[a1->type][a2->type] );
  xfree(s1);
}


static VALUE
 na_minmax_func(int argc, VALUE *argv, VALUE self, na_ufunc_t funcs)
{
  VALUE obj, klass;
  int *shape, rankc, *rankv, cl_dim;
  struct NARRAY *a1, *a2;

  GetNArray(self,a1);

  rankv = ALLOC_N(int,a1->rank*2);
  rankc = na_arg_to_rank( argc, argv, a1->rank, rankv, 0 );

  shape = &rankv[a1->rank];
  na_accum_set_shape( shape, a1->rank, a1->shape, rankc, rankv );

  klass  = CLASS_OF(self);
  cl_dim = na_class_dim(klass);
  if (na_shrink_class(cl_dim,rankv)) klass = cNArray;

  obj = na_make_object(a1->type,a1->rank,shape,klass);
  GetNArray(obj,a2);

  na_minmax_copy0( a2, a1 );
  na_exec_unary( a2, a1, funcs[a1->type] );

  obj = na_shrink_rank(obj, cl_dim, rankv);

  xfree(rankv);
  return obj;
}


/* method: min( rank, ... ) */
static VALUE
 na_min(int argc, VALUE *argv, VALUE self)
{ return na_minmax_func(argc,argv,self,MinFuncs); }

/* method: max( rank, ... ) */
static VALUE
 na_max(int argc, VALUE *argv, VALUE self)
{ return na_minmax_func(argc,argv,self,MaxFuncs); }



static int
 na_sort_number(int argc, VALUE *argv, struct NARRAY *a1)
{
  int i, nsort, rank;

  if (argc==0) {
    rank = a1->rank-1;
  } else {
    rank = NUM2INT(argv[0]);
    if (rank >= a1->rank || rank < -a1->rank)
      rb_raise(rb_eArgError,"illeagal rank:%i out of %i",rank,a1->rank);
    if (rank < 0) rank += a1->rank;
  }

  nsort = 1;
  for (i=0; i<=rank; ++i)
    nsort *= a1->shape[i];
  return nsort;
}


/* method: sort([rank]) */
static VALUE
 na_sort(int argc, VALUE *argv, VALUE self)
{
  struct NARRAY *a1, *a2;
  VALUE obj;
  int  (*func)(const void*, const void*);
  int   i, size, step, nloop, nsort;
  char *ptr;

  GetNArray(self,a1);

  nsort = na_sort_number(argc,argv,a1);
  nloop = a1->total/nsort;

  obj = na_make_object(a1->type,a1->rank,a1->shape,CLASS_OF(self));
  GetNArray(obj,a2);
  memcpy(a2->ptr, a1->ptr, a1->total*na_sizeof[a1->type]);
  func = SortFuncs[a2->type];
  size = na_sizeof[a2->type];
  ptr  = a2->ptr;
  step = size * nsort;

  for (i=0; i<nloop; ++i) {
    qsort( ptr, nsort, size, func );
    ptr += step;
  }
  return obj;
}


/* method: sort!([rank]) */
static VALUE
 na_sort_bang(int argc, VALUE *argv, VALUE self)
{
  struct NARRAY *a1;
  int  (*func)(const void*, const void*);
  int   i, size, step, nloop, nsort;
  char *ptr;

  GetNArray(self,a1);

  nsort = na_sort_number(argc,argv,a1);
  nloop = a1->total/nsort;

  func = SortFuncs[a1->type];
  size = na_sizeof[a1->type];
  ptr  = a1->ptr;
  step = size * nsort;

  for (i=0; i<nloop; ++i) {
    qsort( ptr, nsort, size, func );
    ptr += step;
  }
  return self;
}


/* method: sort_index([rank]) */
static VALUE
 na_sort_index(int argc, VALUE *argv, VALUE self)
{
  struct NARRAY *a1, *a2;
  VALUE obj;
  int  (*func)(const void*, const void*);
  int   i, size, nloop, nsort;
  char **ptr_ptr, **ptr_p;
  char  *ptr_ary,  *ptr_a;
  int32_t *ptr_i;

  GetNArray(self,a1);

  nsort = na_sort_number(argc,argv,a1);
  nloop = a1->total/nsort;

  size = na_sizeof[a1->type];
  ptr_p = ptr_ptr = ALLOC_N(char*, a1->total);
  ptr_a = ptr_ary = a1->ptr;

  for (i=a1->total; i>0; --i) {
    *(ptr_p++) = ptr_a;
    ptr_a += size;
  }

  func = SortIdxFuncs[a1->type];
  ptr_p = ptr_ptr;

  for (i=0; i<nloop; ++i) {
    qsort( ptr_p, nsort, sizeof(char*), func );
    ptr_p += nsort;
  }

  obj = na_make_object(NA_LINT,a1->rank,a1->shape,CLASS_OF(self));
  GetNArray(obj,a2);
  ptr_p = ptr_ptr;
  ptr_i = (int32_t*)(a2->ptr);
  for (i=a2->total; i>0; --i) {
    *(ptr_i++) = (int32_t)(*(ptr_p++)-ptr_ary)/size;
  }
  xfree(ptr_ptr);
  return obj;
}



void Init_na_funcs(void)
{
  rb_define_method(cNArray, "+",  na_add, 1);
  rb_define_method(cNArray, "-",  na_sbt, 1);
  rb_define_method(cNArray, "*",  na_mul, 1);
  rb_define_method(cNArray, "/",  na_div, 1);
  rb_define_method(cNArray, "%",  na_mod, 1);
  rb_define_alias (cNArray, "mod", "%");
  rb_define_method(cNArray, "&",  na_bit_and, 1);
  rb_define_method(cNArray, "|",  na_bit_or, 1);
  rb_define_method(cNArray, "^",  na_bit_xor, 1);
  rb_define_method(cNArray, "**", na_power, 1);

  rb_define_method(cNArray, "add!", na_add_bang, 1);
  rb_define_method(cNArray, "sbt!", na_sbt_bang, 1);
  rb_define_method(cNArray, "mul!", na_mul_bang, 1);
  rb_define_method(cNArray, "div!", na_div_bang, 1);
  rb_define_method(cNArray, "mod!", na_mod_bang, 1);
  rb_define_method(cNArray, "imag=",na_imag_set, 1);

  rb_define_method(cNArray, "swap_byte", na_swap_byte, 0);
  rb_define_method(cNArray, "hton", na_hton, 0);
  rb_define_alias (cNArray, "ntoh", "hton");
  rb_define_method(cNArray, "htov", na_htov, 0);
  rb_define_alias (cNArray, "vtoh", "htov");
  rb_define_method(cNArray, "-@",   na_neg, 0);
  rb_define_method(cNArray, "recip",na_recip, 0);
  rb_define_method(cNArray, "abs",  na_abs, 0);
  rb_define_method(cNArray, "real", na_real, 0);
  rb_define_method(cNArray, "imag", na_imag, 0);
  rb_define_alias (cNArray, "image", "imag");
  rb_define_method(cNArray, "angle", na_angle, 0);
  rb_define_alias (cNArray, "arg", "angle");
  rb_define_method(cNArray, "conj", na_conj, 0);
  rb_define_alias (cNArray, "conjugate", "conj");
  rb_define_method(cNArray, "conj!", na_conj_bang, 0);
  rb_define_alias (cNArray, "conjugate!", "conj!");
  rb_define_method(cNArray, "im",   na_imag_mul, 0);
  rb_define_method(cNArray, "floor",na_floor, 0);
  rb_define_method(cNArray, "ceil", na_ceil, 0);
  rb_define_method(cNArray, "round",na_round, 0);
  rb_define_method(cNArray, "~",    na_bit_rev, 0);
  rb_define_method(cNArray, "not",  na_not, 0);

  rb_define_method(cNArray, "<=>", na_compare, 1);
  rb_define_method(cNArray, "eq",  na_equal, 1);
  rb_define_method(cNArray, "ne",  na_not_equal, 1);
  rb_define_method(cNArray, "gt",  na_greater_than, 1);
  rb_define_alias (cNArray, ">",   "gt");
  rb_define_method(cNArray, "ge",  na_greater_equal, 1);
  rb_define_alias (cNArray, ">=",  "ge");
  rb_define_method(cNArray, "lt",  na_less_than, 1);
  rb_define_alias (cNArray, "<",   "lt");
  rb_define_method(cNArray, "le",  na_less_equal, 1);
  rb_define_alias (cNArray, "<=",  "le");
  rb_define_method(cNArray, "and", na_cond_and, 1);
  rb_define_method(cNArray, "or",  na_cond_or, 1);
  rb_define_method(cNArray, "xor", na_cond_xor, 1);

  rb_define_method(cNArray, "mul_add",   na_mul_add, -1);
  rb_define_method(cNArray, "mul_accum", na_mul_accum, -1);

  rb_define_method(cNArray, "sum", na_sum, -1);
  rb_define_method(cNArray, "accum", na_accum, -1);
  rb_define_method(cNArray, "prod", na_prod, -1);
  rb_define_method(cNArray, "min", na_min, -1);
  rb_define_method(cNArray, "max", na_max, -1);
  rb_define_method(cNArray, "cumsum!", na_cumsum_bang, 0);
  rb_define_method(cNArray, "cumsum", na_cumsum, 0);
  rb_define_method(cNArray, "cumprod!", na_cumprod_bang, 0);
  rb_define_method(cNArray, "cumprod", na_cumprod, 0);
  rb_define_method(cNArray, "sort", na_sort, -1);
  rb_define_method(cNArray, "sort!", na_sort_bang, -1);
  rb_define_method(cNArray, "sort_index", na_sort_index, -1);
  rb_define_method(cNArray, "transpose", na_transpose, -1);

  rb_define_singleton_method(cNArray,"mul",na_s_mul,2);
  rb_define_singleton_method(cNArray,"div",na_s_div,2);
  rb_define_singleton_method(cNArray,"mul_add",na_s_mul_add,-1);

  rb_define_module_function(rb_mNMath,"atan2",na_math_atan2,2);
}
