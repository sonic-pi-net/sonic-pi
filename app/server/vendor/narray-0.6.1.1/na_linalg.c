/*
 * na_linalg.c
 * Numerical Array Extention for Ruby
 *   (C) Copyright 2000-2008 by Masahiro TANAKA
 */
#include <ruby.h>
#include "narray.h"
#include "narray_local.h"
#define ARRAY_BUF

/*
  a_ij == a[j,i]
     j  - >
   i 11 21 31
   | 12 22 32
   v 13 23 33
*/

#define SWAPMEM(a,b,tmp,sz) \
{ memcpy(tmp,a,sz); memcpy(a,b,sz); memcpy(b,tmp,sz); }

typedef struct NARRAY_FUNCSET {
  int   elmsz;
  char *zero;
  char *one;
  char *tiny;
  void (*set)();
  void (*neg)();
  void (*rcp)();
  void (*abs)();
  void (*add)();
  void (*sbt)();
  void (*mul)();
  void (*div)();
  void (*mod)();
  void (*muladd)();
  void (*mulsbt)();
  void (*cmp)();
  int  (*sort)();
  void (*min)();
  void (*max)();
} na_funcset_t;

VALUE cNMatrix, cNVector, cNMatrixLU;
static na_funcset_t na_funcset[NA_NTYPES];
static ID id_lu, id_pivot;


static void
na_loop_linalg( int nd, char *p1, char *p2, char *p3,
		struct slice *s1, struct slice *s2, struct slice *s3,
		void (*func)(), int *shape, int type )
{
  int i;
  int ps1 = s1[0].pstep;
  int ps2 = s2[0].pstep;
  int ps3 = s3[0].pstep;
  int *si;

  if (nd==0) {
    (*func)(1, p1, 0, p2, 0, p3, 0, shape, type);
    return;
  }

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
    (*func)(s2[0].n, s1[0].p, ps1, s2[0].p, ps2, s3[0].p, ps3, shape, type);
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

static int
na_shape_total( int n, int *shape )
{
  int total=1;

  for (; n>0; --n)
    total *= *(shape++);

  return total;
}

static void
na_exec_linalg( struct NARRAY *a1, struct NARRAY *a2, struct NARRAY *a3,
		int ncd1, int ncd2, int ncd3, void (*func)() )
{
  int   ndim, ncd, nsz1, nsz2, nsz3;
  int  *itr, *shp1, *shp2, *shp3;
  struct slice *s1, *s2, *s3;

  ncd  = na_max3(ncd1,ncd2,ncd3); /* class dim */
  ndim = na_max3(a1->rank-ncd1, a2->rank-ncd2, a3->rank-ncd3) + ncd;

  NA_ALLOC_SLICE(s1,(ndim+1)*3,shp1,ndim*4);
  shp2 = &shp1[ndim];
  shp3 = &shp2[ndim];
  itr  = &shp3[ndim];
  s2   = &s1[ndim+1];
  s3   = &s2[ndim+1];

  na_shape_copy( ndim, shp1, a1 );
  na_shape_copy( ndim, shp2, a2 );
  na_shape_copy( ndim, shp3, a3 );
  ndim -= ncd;
  shp1 += ncd1;
  shp2 += ncd2;
  shp3 += ncd3;
  na_shape_max3( ndim, itr, shp1, shp2, shp3 );

  ndim = na_set_slice_3obj( ndim, s1, s2, s3, shp1, shp2, shp3, itr );

  nsz1 = na_shape_total(a1->rank-ncd1,a1->shape+ncd1);
  nsz2 = na_shape_total(a2->rank-ncd2,a2->shape+ncd2);
  nsz3 = na_shape_total(a3->rank-ncd3,a3->shape+ncd3);

  na_init_slice(s1, ndim, shp1, na_sizeof[a1->type]*nsz1 );
  na_init_slice(s2, ndim, shp2, na_sizeof[a2->type]*nsz2 );
  na_init_slice(s3, ndim, shp3, na_sizeof[a3->type]*nsz3 );

  na_loop_linalg( ndim, a1->ptr, a2->ptr, a3->ptr,
		  s1, s2, s3, func, a2->shape, a2->type );
  xfree(s1);
}


static int
na_lu_fact_func_body(int ni, char *a, char *idx, int *shape, int type, char *buf)
{
  int i, j, k;
  int imax;

  char *amax, *rtmp;
  char *aa, *aii, *aij, *ai0, *a0i, *a0j;
  char *v, *vi;

  na_funcset_t *f = &na_funcset[type];
  na_funcset_t *r = &na_funcset[na_cast_real[type]];

  int status = 0;
  int n = shape[0];
  int relmsz = r->elmsz;
  int felmsz = f->elmsz;
  int rowsz  = felmsz*n;
  int matsz  = rowsz*n;
  int diagsz = rowsz + felmsz;

  v    = buf + rowsz;
  amax = v   + relmsz*n;

  while (ni-->0) {

    aa = a;
    vi = v;

    /* v[j] = 1/max( abs( a[i,j] ) ) */
    for (j=0;j<n;++j) {
      f->abs(n, buf, relmsz, aa, felmsz);

      r->set(1, amax,0, r->zero,0);
      rtmp = buf;
      for (i=0;i<n;++i) {
	if (r->sort(rtmp, amax) == 1)
	  r->set(1, amax,0, rtmp,0);
	rtmp += relmsz;
      }

      if (r->sort(amax,r->tiny) != 1)
	status = 2; /* Singular Matrix */

      r->rcp(1, vi,0, amax,0);
      vi += relmsz;
    }

    ai0 = a0i = aii = a;
    vi  = v;

    for (i=0;i<n;++i) {

      f->set(n, buf, felmsz, ai0, rowsz);

      aij = buf;
      a0j = a;
      /* a[i,j(<i)]  -=  sum(k<j){ a[i,k]*a[k,j] } */
      for (j=1;j<i;++j) {
	aij += felmsz;
	a0j += rowsz;
	f->mulsbt(j, aij, 0, buf, felmsz, a0j, felmsz);
      }
      /* a[i,j(>=i)]  -=  sum(k<i){ a[i,k]*a[k,j] } */
      for (  ;j<n;++j) {
	aij += felmsz;
	a0j += rowsz;
	f->mulsbt(i, aij, 0, buf, felmsz, a0j, felmsz);
      }
      f->set(n, ai0, rowsz, buf, felmsz);

      /* pivoting
	 imax = max_idx( abs( a[i,j(>=i)] ) * v[j(>=i)] ) */
      f->abs(n-i, buf, relmsz, aii, rowsz);
      r->mul(n-i, buf, relmsz, vi, relmsz);

      r->set(1, amax,0, r->zero,0);
      rtmp = buf;
      imax = 0;
      for (j=i;j<n;++j) {
	if (r->sort(rtmp,amax) == 1) {
	  r->set(1, amax,0, rtmp,0);
	  imax = j;
	}
	rtmp += relmsz;
      }

      if (r->sort(amax,r->tiny)!=1)
	status = 1; /* Singular Matrix */

      if (i != imax) {
	/* a[*,i] <=> a[*,imax] */
	SWAPMEM(a+i*rowsz, a+imax*rowsz, buf, rowsz);
	/* v[i]   <=> v[imax] */
	SWAPMEM(vi, v+imax*relmsz, buf, relmsz);
	NA_SWAP(((int32_t*)idx)[i],((int32_t*)idx)[imax],k);
      }

      /* a[i,j(>i)]  = a[i,j]/a[i,i] */
      f->div(n-i-1, aii+rowsz, rowsz, aii, 0);

      ai0 += felmsz;
      a0i += rowsz;
      aii += diagsz;
      vi  += relmsz;
    }

    a   += matsz;
    idx += sizeof(int32_t)*n;
  }
  return status;
}



static int
 na_lu_fact_func(int ni, char *a, char *idx, int *shape, int type)
{
  volatile VALUE val;
  char *buf;
  int status, size, n=shape[0];

  if (type==NA_ROBJ) {
    VALUE *mem;
    int i;
    size = n*2+1;
    mem = ALLOC_N(VALUE, size);
    for (i=0; i<size; i++) mem[i] = Qnil;
    val = rb_ary_new4(size, mem);
    xfree(mem);
    buf = (char*)((RARRAY_PTR(val)));
    status = na_lu_fact_func_body( ni, a, idx, shape, type, buf );
  } else {
    size = na_sizeof[type]*n + na_sizeof[na_cast_real[type]]*(n+1);
    buf = ALLOC_N(char, size);
    status = na_lu_fact_func_body( ni, a, idx, shape, type, buf );
    xfree(buf);
  }
  return status;
}


/* :nodoc: */
static VALUE
 na_lu_fact_bang(VALUE self)
{ 
  int i, total, n, sz, stat;
  struct NARRAY *ary;
  VALUE piv;
  char *ptr, *idx;
  void (*func)();

  GetNArray(self,ary);

  /* shape & dimension check */
  if (ary->rank<2)
    rb_raise(rb_eTypeError,"dim(=%i) < 2", ary->rank);

  n = ary->shape[0];
  if (n != ary->shape[1])
    rb_raise(rb_eTypeError,"not square matrix");

  total=1;
  for (i=2; i<ary->rank; ++i)
    total *= ary->shape[i];

  piv = na_make_object(NA_LINT, ary->rank-1, ary->shape+1, cNVector);

  /* prepare pivot index */
  func = IndGenFuncs[NA_LINT];
  sz   = na_sizeof[NA_LINT];
  ptr  = idx = ((struct NARRAY *)DATA_PTR(piv))->ptr;
  for (i=0; i<total; ++i) {
    func(n,ptr,sz,0,1);
    ptr += n*sz;
  }

  stat = na_lu_fact_func(total, ary->ptr, idx, ary->shape, ary->type);

  if (stat!=0)
    rb_raise(rb_eZeroDivError,"singular matrix, status=%i",stat);

  return rb_funcall(cNMatrixLU,na_id_new,2,self,piv);
}


/* :nodoc: */
static VALUE
 na_lu_fact(VALUE self)
{
  return na_lu_fact_bang( na_clone(self) );
}


static void
na_lu_pivot_func( int ni,
		  char *x, int ps1, char *y, int ps2, char *idx, int ps3,
		  int *shape, int type )
{
  int i, n, sz;
  char *xi;
  na_funcset_t *f = &na_funcset[type];

  n = shape[1];
  sz = f->elmsz * shape[0];

  for (; ni>0; --ni) {
    xi = x;
    for (i=0; i<n; ++i) {
      memcpy(xi, y+((int32_t*)idx)[i]*sz, sz);
      xi += sz;
    }
    x   += ps1;
    y   += ps2;
    idx += ps3;
  }
}



static void
na_lu_solve_func_body( int ni,
		       char *x, int ps1,  char *a, int ps2,
		       int *shape, int type, char *buf )
{
  char *aii, *a0i, *xx, *xi;
  int i,k;
  na_funcset_t *f = &na_funcset[type];
  int n = shape[1];
  int sz = na_sizeof[type];
  int xsz = shape[0] * sz;
  int rowsz = sz * n;
  int matsz = rowsz * n;
  int diagsz = rowsz + sz;

  for (; ni>0; --ni) {

    xx = x;

    for (k=shape[0]; k>0; --k) { /* once if x is vector */

      f->set(n, buf,sz, xx,xsz);

      xi  = buf;
      a0i = a;

      /* solve Lx' = y' */
      for (i=1; i<n; ++i) {
	/* x[i] -= a[j(<i),i] * x[j(<i)] */
	xi  += sz;
	a0i += rowsz;
	f->mulsbt(i, xi, 0, a0i, sz, buf, sz);
      }

      xi  = buf + sz*(n-1);
      aii = a + (matsz-sz);

      /* solve Ux = x' */
      f->div(1, xi,0, aii,0);
      for (i=n-1; i>0; --i) {
	xi  -= sz;
	aii -= diagsz;
	/* x[i] -= a[j(>i),i] * x[j(>i)] */
	f->mulsbt(n-i, xi,0, aii+sz, sz, xi+sz, sz);
	/* x[i] /= a[i,i] */
	f->div(1, xi,0, aii,0);
      }

      f->set(n, xx,xsz, buf,sz);

      xx += sz;
    }

    x += ps1;
    a += ps2;
  }
}


static void
na_lu_solve_func( int ni,
		  char *z, int ps,  char *x, int ps1,  char *a, int ps2,
		  int *shape, int type )
{
  volatile VALUE val;
  char *buf;
  int size;

  if (type==NA_ROBJ) {
    VALUE *mem;
    int i;
    size = shape[1];
    mem = ALLOC_N(VALUE, size);
    for (i=0; i<size; i++) mem[i] = Qnil;
    val = rb_ary_new4(size, mem);
    xfree(mem);
    buf = (char*)((RARRAY_PTR(val)));
    na_lu_solve_func_body( ni, x, ps1, a, ps2, shape, type, buf );
  } else {
    size = shape[1] * na_sizeof[type];
    buf = ALLOC_N(char, size);
    na_lu_solve_func_body( ni, x, ps1, a, ps2, shape, type, buf );
    xfree(buf);
  }
}


static void
na_shape_max2(int ndim, int *shape, int n1, int *shape1, int n2, int *shape2)
{
  int *tmp;
  int  i;

  if (n1 < n2) {
    NA_SWAP(shape1,shape2,tmp);
  }

  for (i=0; i<n2; ++i) {
    shape[i] = NA_MAX(shape1[i],shape2[i]);
  }
  for (   ; i<n1; ++i) {
    shape[i] = shape1[i];
  }
  for (   ; i<ndim; ++i) {
    shape[i] = 1;
  }
}



/*
 *  call-seq:
 *     lu.solve(arg)  -> result
 *
 *  Solve with the result of LU factorization.
 *  arg should be NMatrix or NVector instance.
 *  Returns an instance of same class with arg.
 */
static VALUE
na_lu_solve(VALUE self, volatile VALUE other)
{
  int  n, ndim;
  int *shape;
  struct NARRAY *a1, *a2, *l, *p;
  VALUE pv, obj, klass;
  volatile VALUE lu;

  klass = CLASS_OF(other);
  if (klass==cNVector)
    other = na_newdim_ref(1,(VALUE*)na_funcset[NA_ROBJ].zero,other);
  else if (klass!=cNMatrix)
    rb_raise(rb_eTypeError,"neither NMatrix or NVector");

  lu = rb_ivar_get(self, id_lu);
  pv = rb_ivar_get(self, id_pivot);

  GetNArray(lu,l);

  other = na_upcast_object(other,l->type);
  GetNArray(other,a1);

  lu = na_upcast_type(lu,a1->type);
  GetNArray(lu,l);
  GetNArray(pv,p);

  n = l->shape[0];
  if (n != a1->shape[1])
    rb_raise(rb_eTypeError,"size mismatch (%i!=%i)",n,a1->shape[1]);

  ndim  = NA_MAX(l->rank, a1->rank);
  shape = ALLOCA_N(int, ndim);

  shape[0] = a1->shape[0];
  na_shape_max2( ndim-1, shape+1, a1->rank-1, a1->shape+1,
		 l->rank-1, l->shape+1 );
  obj = na_make_object( a1->type, ndim, shape, klass );

  GetNArray(obj,a2);

  na_exec_linalg( a2, a1, p, 2, 2, 1, na_lu_pivot_func );
  na_exec_linalg( a2, a2, l, 2, 2, 2, na_lu_solve_func );

  if (klass==cNVector) {
    shape = ALLOC_N(int, ndim-1);
    memcpy(shape,a2->shape+1,sizeof(int)*(ndim-1));
    xfree(a2->shape);
    a2->shape = shape;
    --(a2->rank);
  }
  return obj;
}


/* :nodoc: */
static VALUE
na_lu_init(VALUE self, VALUE lu, VALUE piv)
{
  int i;
  struct NARRAY *l, *p;

  if (CLASS_OF(lu)!=cNMatrix)
    rb_raise(rb_eTypeError,"LU should be NMatrix");
  if (CLASS_OF(piv)!=cNVector)
    rb_raise(rb_eTypeError,"pivot should be NVector");

  GetNArray(lu,l);
  GetNArray(piv,p);

  if (p->type != NA_LINT)
    rb_raise(rb_eRuntimeError,"pivot type must be Integer");

  if (l->rank != p->rank+1)
    rb_raise(rb_eRuntimeError,"array dimension mismatch %i!=%i+1",
	     l->rank, p->rank);

  if (l->shape[0] != l->shape[1])
    rb_raise(rb_eRuntimeError,"LU matrix (%i,%i) is not square",
	     l->shape[0], l->shape[1]);

  for (i=1; i<l->rank; ++i)
    if (l->shape[i] != p->shape[i-1])
      rb_raise(rb_eRuntimeError,"array size mismatch %i!=%i at %i",
	       l->shape[i], p->shape[i-1], i);

  rb_ivar_set(self, id_lu, lu);
  rb_ivar_set(self, id_pivot, piv);
  return Qnil;
}



void Init_na_linalg()
{
  static double tiny_d = 1e-15;
  static float  tiny_f = (float)1e-7;
  int i, sz;
  int32_t one=1, zero=0;
  static VALUE zerov = INT2FIX(0);
  static VALUE onev = INT2FIX(1);
  char *a = malloc(NA_NTYPES*sizeof(dcomplex)*2);

  for (i=1;i<NA_NTYPES;++i) {
    sz = na_funcset[i].elmsz = na_sizeof[i];
    sz = (sz>((int)sizeof(int))) ? sz : (int)sizeof(int);
    SetFuncs[i][NA_LINT](1, a,0, &one, 0);
    na_funcset[i].one = a;
    a += sz;
    SetFuncs[i][NA_LINT](1, a,0, &zero,0);
    na_funcset[i].zero = a;
    na_funcset[i].tiny = a;
    a += sz;
    na_funcset[i].set = SetFuncs[i][i];
    na_funcset[i].neg = NegFuncs[i];
    na_funcset[i].rcp = RcpFuncs[i];
    na_funcset[i].abs = AbsFuncs[i];
    na_funcset[i].add = AddUFuncs[i];
    na_funcset[i].sbt = SbtUFuncs[i];
    na_funcset[i].mul = MulUFuncs[i];
    na_funcset[i].div = DivUFuncs[i];
    na_funcset[i].mod = ModUFuncs[i];
    na_funcset[i].muladd = MulAddFuncs[i];
    na_funcset[i].mulsbt = MulSbtFuncs[i];
    na_funcset[i].cmp = CmpFuncs[i];
    na_funcset[i].min = MinFuncs[i];
    na_funcset[i].max = MaxFuncs[i];
    na_funcset[i].sort = SortFuncs[i];
  }
  na_funcset[NA_SFLOAT].tiny = (char*)&tiny_f;
  na_funcset[NA_DFLOAT].tiny = (char*)&tiny_d;
  na_funcset[NA_ROBJ].zero = (char*)&zerov;
  na_funcset[NA_ROBJ].one  = (char*)&onev;

  cNVector = rb_define_class("NVector",cNArray);
  cNMatrix = rb_define_class("NMatrix",cNArray);
  cNMatrixLU = rb_define_class("NMatrixLU",rb_cObject);

  rb_define_method(cNMatrix, "lu_fact!", na_lu_fact_bang, 0);
  rb_define_alias(cNMatrix,  "lu!","lu_fact!");
  rb_define_method(cNMatrix, "lu_fact",  na_lu_fact, 0);
  rb_define_alias(cNMatrix,  "lu","lu_fact");

  rb_define_method(cNMatrixLU, "initialize", na_lu_init, 2);
  rb_define_method(cNMatrixLU, "solve", na_lu_solve, 1);

  id_lu    = rb_intern("@lu");
  id_pivot = rb_intern("@pivot");
}
