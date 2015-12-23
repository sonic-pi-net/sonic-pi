/*
  narray.c
  Numerical Array Extention for Ruby
    (C) Copyright 1999-2008 by Masahiro TANAKA

  This program is free software.
  You can distribute/modify this program
  under the same terms as Ruby itself.
  NO WARRANTY.
*/
#define NARRAY_C
#include <ruby.h>
#include "narray.h"
#include "narray_local.h"

/* global variables within this module */
VALUE cNArray, cNArrayScalar, cComplex;

ID na_id_beg, na_id_end, na_id_exclude_end;
ID na_id_minus, na_id_abs, na_id_power;
ID na_id_compare, na_id_ne, na_id_and, na_id_or;
ID na_id_class_dim;
ID na_id_add, na_id_sbt, na_id_mul, na_id_div, na_id_mod;
ID na_id_real, na_id_imag;
ID na_id_coerce_rev;
ID na_id_new;
ID na_id_Complex;
static ID na_id_to_i, na_id_usec, na_id_now;

const int na_sizeof[NA_NTYPES+1] = {
  0,
  sizeof(u_int8_t),
  sizeof(int16_t),
  sizeof(int32_t),
  sizeof(float),
  sizeof(double),
  sizeof(scomplex),
  sizeof(dcomplex),
  sizeof(VALUE),
  0
};

const char *na_typestring[] = {
  "none",
  "byte",	/* 1 */
  "sint",	/* 2 */
  "int",	/* 3 */
  "sfloat",	/* 4 */
  "float",	/* 5 */
  "scomplex",	/* 6 */
  "complex",	/* 7 */
  "object",	/* 8 */
  "ntypes"	/* 9 */
};

#ifdef NARRAY_GC
static int mem_count = 0;
static int na_gc_freq = 2500000;   /* Frequency of Garbage Collection */
#endif

void Init_na_array(void);
void Init_na_index(void);
void Init_nmath(void);
void Init_na_funcs(void);
void Init_na_linalg(void);
void Init_na_random(void);


#ifdef DEBUG
void na_xfree(void *ptr)
{
  if (!ptr) abort();
  free(ptr);
}
#endif

/* mark items */
static void
 na_mark_obj(struct NARRAY *ary)
{
  int i;
  VALUE *ptr;

  ptr = (VALUE*) ary->ptr;
  for (i=ary->total; i>0; --i)
    rb_gc_mark(*ptr++);
}

static void
 na_mark_ref(struct NARRAY *ary)
{
  struct NARRAY *a2;

  rb_gc_mark( ary->ref );

  GetNArray(ary->ref,a2);
  if (a2->type == NA_ROBJ) na_mark_obj(a2);
}


static void
 na_free(struct NARRAY* ary)
{
  if ( ary->total > 0 ) {
    if (ary->ref == Qnil || ary->ref == Qtrue) {  /* non reference */
      xfree(ary->ptr);
    }
    xfree(ary->shape);
#ifdef DEBUG
    ary->shape = NULL;
    ary->ptr = NULL;
#endif
  }
  xfree(ary);
}


/* allocation of NARRAY */
struct NARRAY*
 na_alloc_struct(int type, int rank, int *shape)
{
  int total=1, total_bak;
  int i, memsz;
  struct NARRAY *ary;

  for (i=0; i<rank; ++i) {
    if (shape[i] < 0) {
      rb_raise(rb_eArgError, "negative array size");
    } else if (shape[i] == 0) {
      total = 0;
      break;
    }
    total_bak = total;
    total *= shape[i];
    if (total < 1 || total > 2147483647 || total/shape[i] != total_bak) {
      rb_raise(rb_eArgError, "array size is too large");
    }
  }

  if (rank<=0 || total<=0) {
    /* empty array */
    ary = ALLOC(struct NARRAY);
    ary->rank  =
    ary->total = 0;
    ary->shape = NULL;
    ary->ptr   = NULL;
    ary->type  = type;
  }
  else {
    memsz = na_sizeof[type] * total;

    if (memsz < 1 || memsz > 2147483647 || memsz/na_sizeof[type] != total) {
      rb_raise(rb_eArgError, "allocation size is too large");
    }

    /* Garbage Collection */
#ifdef NARRAY_GC
    mem_count += memsz;
    if ( mem_count > na_gc_freq ) { rb_gc(); mem_count=0; }
#endif

    ary        = ALLOC(struct NARRAY);
    ary->shape = ALLOC_N(int,  rank);
    ary->ptr   = ALLOC_N(char, memsz);

    ary->rank  = rank;
    ary->total = total;
    ary->type  = type;
    for (i=0; i<rank; ++i)
      ary->shape[i] = shape[i];
  }
  ary->ref = Qtrue;
  return ary;
}

#if !defined RCLASS_SUPER
#define RCLASS_SUPER(v) (RCLASS(v)->super)
#endif

static void
 na_check_class_narray(VALUE v)
{
  if (TYPE(v) != T_CLASS) {
    rb_raise(rb_eRuntimeError, "class required");
  }

  if (v == cNArray)
    return;
  if (RTEST(rb_funcall(v, rb_intern("<="), 1, cNArray)))
    return;

  rb_raise(rb_eRuntimeError, "need NArray or its subclass");
}


static VALUE
 na_wrap_struct_class(struct NARRAY *ary, VALUE klass)
{
  VALUE v;
  int class_dim;

  /* Extract element */
  if (ary->rank==0 && ary->total==1) {
    SetFuncs[NA_ROBJ][ary->type](1,&v,0,ary->ptr,0);
    na_free(ary);
    return v;
  }

  /* check NArray >= klass */
  na_check_class_narray(klass);

  /* Check dimension */
  class_dim = NUM2INT(rb_const_get(klass, na_id_class_dim));
  if (ary->rank < class_dim)
    rb_raise(rb_eTypeError, "array.dim(=%i) < CLASS_DIMENSION(=%i)",
	     ary->rank, class_dim);

  if (ary->ref == Qnil)
    rb_raise(rb_eRuntimeError, "already wrapped object");

  /* Turn on WRAPPED flag */
  if (ary->ref == Qtrue) {
    ary->ref = Qnil;
    if (NA_IsROBJ(ary))
      return Data_Wrap_Struct(klass, na_mark_obj, na_free, ary);
    else
      return Data_Wrap_Struct(klass, 0, na_free, ary);
  }

  /* reference to another NArray*/
  return Data_Wrap_Struct(klass, na_mark_ref, na_free, ary);
}


static VALUE
 na_wrap_struct(struct NARRAY *ary, VALUE obj)
{
  return na_wrap_struct_class(ary,CLASS_OF(obj));
}


VALUE
 na_make_object(int type, int rank, int *shape, VALUE klass)
{
  struct NARRAY *na;

  na = na_alloc_struct(type, rank, shape);

  if (type==NA_ROBJ) {
    rb_mem_clear((VALUE*)(na->ptr), na->total);
  }
  return na_wrap_struct_class(na, klass);
}


/* restriction: Integer, Float, Complex === obj */
VALUE
 na_make_scalar(VALUE obj, int type)
{
  static int shape=1;
  VALUE v;
  struct NARRAY *ary;

  v = na_make_object(type,1,&shape,cNArrayScalar);
  GetNArray(v,ary);
  SetFuncs[ary->type][NA_ROBJ](1, ary->ptr,0, &obj,0);

  return v;
}


VALUE
 na_make_empty(int type, VALUE klass)
{
  struct NARRAY *na;

  na = na_alloc_struct(type, 0, NULL);
  return na_wrap_struct_class(na, klass);
}


/* allocate reference to NArray */
struct NARRAY*
 na_ref_alloc_struct(VALUE obj)
{
  int i;
  struct NARRAY *orig, *ary;

  GetNArray(obj,orig);

  if (orig->rank<=0)
    rb_raise(rb_eRuntimeError, "cannot create NArrayRefer of Empty NArray");

  ary        = ALLOC(struct NARRAY);
  ary->shape = ALLOC_N(int, orig->rank);
  ary->ptr   = orig->ptr;
  ary->rank  = orig->rank;
  ary->total = orig->total;
  ary->type  = orig->type;
  for (i=0; i<orig->rank; ++i)
    ary->shape[i] = orig->shape[i];
  ary->ref   = obj;

  return ary;
}

/* method:  self.refer */
static VALUE
 na_refer(VALUE self)
{
  return na_wrap_struct(na_ref_alloc_struct(self), self);
}

/* singleton method:  NArray.refer( other ) */
static VALUE
 na_s_refer(VALUE klass, VALUE self)
{
  return na_wrap_struct_class(na_ref_alloc_struct(self), klass);
}

/* method:  self.original */
static VALUE
 na_original(VALUE self)
{
  struct NARRAY *ary;

  GetNArray(self,ary);
  return ary->ref;
}


void
 na_clear_data(struct NARRAY *ary)
{
  if (NA_IsROBJ(ary))
    rb_mem_clear((VALUE*)(ary->ptr), ary->total);
  else
    MEMZERO(ary->ptr, char, na_sizeof[ary->type]*ary->total);
}


/* local function for new array creation */
static VALUE
 na_new2(int argc, VALUE *argv, int type, VALUE klass)
{
  int i, *shape;
  struct NARRAY *ary;
  VALUE v;

  if (argc == 0)
    rb_raise(rb_eArgError, "Argument required");

  shape = ALLOCA_N(int,argc);
  for (i=0; i<argc; ++i) shape[i]=NUM2INT(argv[i]);

  v = na_make_object(type,argc,shape,klass);
  GetNArray(v,ary);

  if (ary->type != NA_ROBJ)
    na_clear_data(ary);

  /* rb_obj_call_init(v, 0, 0); */
  return v;
}


/* Convert type arguments -> typecode */
int
 na_get_typecode(VALUE v)
{
  struct NARRAY *na;
  int i;

  if (v==rb_cFloat)   return NA_DFLOAT;
  if (v==rb_cInteger) return NA_LINT;
  if (v==cComplex)    return NA_DCOMPLEX;
  if (v==rb_cObject)  return NA_ROBJ;
  if (FIXNUM_P(v)) {
    i = NUM2INT(v);
    if (i<=NA_NONE || i>=NA_NTYPES)
      rb_raise(rb_eArgError, "Wrong type code");
    return i;
  }
  if (NA_IsNArray(v)) {
    GetNArray(v,na);
    return na->type;
  }
  if (TYPE(v)==T_STRING) {
    for (i=1; i<NA_NTYPES; ++i) {
      if ( !strncmp( RSTRING_PTR(v), na_typestring[i], RSTRING_LEN(v)) )
	return i;
    }
  }
  rb_raise(rb_eArgError, "Unrecognized NArray type");
  return 0;
}


/* class method: new(type, size1,size2,...,sizeN) */
static VALUE
 na_s_new(int argc, VALUE *argv, VALUE klass)
{
  if (argc == 0)
    rb_raise(rb_eArgError, "Argument required");
  return na_new2(argc-1, argv+1, na_get_typecode(argv[0]), klass);
}

/* class method: byte(size1,size2,...,sizeN) */
static VALUE
 na_s_new_byte(int argc, VALUE *argv, VALUE klass)
{ return na_new2(argc, argv, NA_BYTE, klass); }

/* class method: sint(size1,size2,...,sizeN) */
static VALUE
 na_s_new_sint(int argc, VALUE *argv, VALUE klass)
{ return na_new2(argc, argv, NA_SINT, klass); }

/* class method: int(size1,size2,...,sizeN) */
static VALUE
 na_s_new_int(int argc, VALUE *argv, VALUE klass)
{ return na_new2(argc, argv, NA_LINT, klass); }

/* class method: sfloat(size1,size2,...,sizeN) */
static VALUE
 na_s_new_sfloat(int argc, VALUE *argv, VALUE klass)
{ return na_new2(argc, argv, NA_SFLOAT, klass); }

/* class method: float(size1,size2,...,sizeN) */
static VALUE
 na_s_new_float(int argc, VALUE *argv, VALUE klass)
{ return na_new2(argc, argv, NA_DFLOAT, klass); }

/* class method: scomplex(size1,size2,...,sizeN) */
static VALUE
 na_s_new_scomplex(int argc, VALUE *argv, VALUE klass)
{ return na_new2(argc, argv, NA_SCOMPLEX, klass); }

/* class method: complex(size1,size2,...,sizeN) */
static VALUE
 na_s_new_complex(int argc, VALUE *argv, VALUE klass)
{ return na_new2(argc, argv, NA_DCOMPLEX, klass); }

/* class method: object(size1,size2,...,sizeN) */
static VALUE
 na_s_new_object(int argc, VALUE *argv, VALUE klass)
{ return na_new2(argc, argv, NA_ROBJ, klass); }



/* method: dup() */
VALUE
 na_clone(VALUE self)
{
  struct NARRAY *org, *cpy;

  GetNArray(self,org);
  cpy = na_alloc_struct(org->type,org->rank,org->shape);
  memcpy(cpy->ptr, org->ptr, na_sizeof[org->type] * org->total);
  return na_wrap_struct(cpy,self);
}


/* local function */
void
 na_copy_nary(struct NARRAY *dst, struct NARRAY *src)
{
  if (dst->total != src->total)
    rb_raise(rb_eRuntimeError, "src and dst array sizes mismatch");

  if (dst->type == src->type)
    memcpy(dst->ptr, src->ptr, src->total*na_sizeof[src->type]);
  else {
    SetFuncs[dst->type][src->type]( src->total,
				    dst->ptr, na_sizeof[dst->type],
				    src->ptr, na_sizeof[src->type] );
  }
}


/* method: to_type(type) */
static VALUE
 na_to_type(VALUE self, VALUE vtype)
{
  struct NARRAY *a1, *a2;
  VALUE v;

  GetNArray(self,a1);

  v = na_make_object(na_get_typecode(vtype), a1->rank, a1->shape,
		     CLASS_OF(self));
  GetNArray(v,a2);
  na_copy_nary(a2,a1);
  return v;
}


/* method: to_f() */
static VALUE
 na_to_float(VALUE self)
{
  struct NARRAY *a1, *a2;
  VALUE v;

  GetNArray(self,a1);

  v = na_make_object(na_upcast[NA_SFLOAT][a1->type], a1->rank, a1->shape,
		     CLASS_OF(self));
  GetNArray(v,a2);
  na_copy_nary(a2,a1);
  return v;
}


/* method: to_i() */
static VALUE
 na_to_integer(VALUE self)
{
  int type;
  struct NARRAY *a1, *a2;
  VALUE v;

  GetNArray(self,a1);
  if (!NA_IsINTEGER(a1))
    type = NA_LINT;
  else
    type = a1->type;
  v = na_make_object(type, a1->rank, a1->shape, CLASS_OF(self));
  GetNArray(v,a2);
  na_copy_nary(a2,a1);
  return v;
}


/* method: shape() -- returns an array of shape of each rank */
static VALUE
 na_shape(VALUE self)
{
  struct NARRAY *ary;
  VALUE *shape;
  int i;

  GetNArray(self,ary);
  shape = ALLOCA_N(VALUE,ary->rank);
  for (i = 0; i < ary->rank; ++i)
    shape[i] = INT2FIX(ary->shape[i]);
  return rb_ary_new4(ary->rank,shape);
}


/* method: rank() -- returns the rank of the array */
static VALUE
 na_rank(VALUE self)
{
  struct NARRAY *ary;
  GetNArray(self,ary);
  return INT2FIX(ary->rank);
}


/* method: size() -- returns the total number of elements */
static VALUE
 na_size(VALUE self)
{
  struct NARRAY *ary;
  GetNArray(self,ary);
  return INT2FIX(ary->total);
}


/* method: typecode -- returns the type of the array */
static VALUE
 na_typecode(VALUE self)
{
  struct NARRAY *ary;
  GetNArray(self,ary);
  return INT2FIX(ary->type);
}


/* method: element_size -- returns the element size of the array type */
static VALUE
 na_element_size(VALUE self)
{
  struct NARRAY *ary;
  GetNArray(self,ary);
  return INT2FIX(na_sizeof[ary->type]);
}


/* method: empty? -- returns true if empty array */
static VALUE
 na_is_empty(VALUE self)
{
  struct NARRAY *ary;
  GetNArray(self,ary);
  if (ary->total==0) return Qtrue;
  return Qfalse;
}


/* Binary copy of String => NArray */
static VALUE
 na_str_to_na(int argc, VALUE *argv, VALUE str)
{
  struct NARRAY *ary;
  VALUE v;
  int i, type, len=1, str_len, *shape, rank=argc-1;

  if (argc < 1)
    rb_raise(rb_eArgError, "Type and Size Arguments required");

  type = na_get_typecode(argv[0]);

  str_len = RSTRING_LEN(str);

  if (argc == 1) {
    rank  = 1;
    shape = ALLOCA_N(int,rank);
    if ( str_len % na_sizeof[type] != 0 )
      rb_raise(rb_eArgError, "string size mismatch");
    shape[0] = str_len / na_sizeof[type];
  }
  else {
    shape = ALLOCA_N(int,rank);
    for (i=0; i<rank; ++i)
      len *= shape[i] = NUM2INT(argv[i+1]);
    len *= na_sizeof[type];
    if ( len != str_len )
      rb_raise(rb_eArgError, "size mismatch");
  }

  v = na_make_object( type, rank, shape, cNArray );
  GetNArray(v,ary);
  memcpy( ary->ptr, RSTRING_PTR(str), ary->total*na_sizeof[type] );

  return v;
}


/* method: to_s -- convert the data contents to a binary string */
static VALUE
 na_to_s(VALUE self)
{
  struct NARRAY *ary;
  GetNArray(self,ary);
  if (NA_IsROBJ(ary))
    rb_raise(rb_eTypeError,"cannot convert object-type NArray");
  return rb_str_new(ary->ptr,ary->total*na_sizeof[ary->type]);
}


/* method: to_binary -- convert the data contents to a BYTE type NArray */
static VALUE
 na_to_binary(VALUE self)
{
  struct NARRAY *a1, *a2;
  int i, *shape, rank;
  VALUE v;

  GetNArray(self,a1);

  rank = a1->rank+1;
  shape = ALLOCA_N(int,rank);
  shape[0] = na_sizeof[a1->type];
  for (i=1; i<rank; ++i)
    shape[i] = a1->shape[i-1];

  v = na_make_object( NA_BYTE, rank, shape, cNArray );
  GetNArray(v,a2);
  MEMCPY(a2->ptr,a1->ptr,char,a2->total);

  return v;
}


/* method: to_type_as_binary(type) */
static VALUE
 na_to_type_as_binary(VALUE self, VALUE vtype)
{
  struct NARRAY *a1, *a2;
  int size, total, type;
  VALUE v;

  type = na_get_typecode(vtype);
  GetNArray(self,a1);

  size = a1->total * na_sizeof[a1->type];
  if ( size % na_sizeof[type] != 0 )
    rb_raise(rb_eRuntimeError, "bina1 size mismatch");
  total = size / na_sizeof[type];

  v = na_make_object( type, 1, &total, cNArray );
  GetNArray(v,a2);
  MEMCPY(a2->ptr,a1->ptr,char,size);

  return v;
}


static void
 na_to_string_binary(int n, char *p1, int i1, char *p2, int i2)
{
  for (; n>0; --n) {
    *(VALUE*)p1 = rb_str_new(p2,i2);
    p1+=i1; p2+=i2;
  }
}


/* method: to_string */
static VALUE
 na_to_string(VALUE self)
{
  VALUE v;
  struct NARRAY *a1, *a2;

  GetNArray(self,a1);

  if (a1->total==0)
    v = na_make_empty(NA_ROBJ, CLASS_OF(self));
  else
  if (a1->type==NA_BYTE) {
    if (a1->rank==1)
      return rb_str_new(a1->ptr,a1->shape[0]);
    v  = na_make_object(NA_ROBJ, a1->rank-1, a1->shape+1, cNArray);
    GetNArray(v,a2);
    na_to_string_binary( a2->total,
			 a2->ptr, sizeof(VALUE),
			 a1->ptr, a1->shape[0] );
  } else {
    v = na_make_object(NA_ROBJ, a1->rank, a1->shape, CLASS_OF(self));
    GetNArray(v,a2);
    ToStrFuncs[a1->type]( a2->total,
			  a2->ptr, sizeof(VALUE),
			  a1->ptr, na_sizeof[a1->type] );
  }
  return v;
}


/* singleton method:
   NArray.to_na( string, type, size1,size2,...,sizeN )
   NArray.to_na( array )
*/
static VALUE
 na_s_to_na(int argc, VALUE *argv, VALUE klass)
{
  if (argc < 1) {
    rb_raise(rb_eArgError, "Argument is required");
  }
  if (TYPE(argv[0]) == T_STRING) {
    return na_str_to_na(argc-1,argv+1,argv[0]);
  }
  if (argc > 1) {
    rb_raise(rb_eArgError, "Only one array argument must be provided");
  }
  if (TYPE(argv[0]) == T_ARRAY) {
    return na_ary_to_nary( argv[0], klass );
  }
  if (NA_IsNArray(argv[0])) {
    return argv[0];
  }
  rb_raise(rb_eTypeError, "Argument must be Array or String (or NArray)");
  return Qnil;
}


/* singleton method:
   NArray[object]
*/
static VALUE
 na_s_bracket(int argc, VALUE *argv, VALUE klass)
{
  VALUE v = rb_ary_new4(argc, argv);
  return na_ary_to_nary( v, klass );
}


/* method: coerce(other) */
static VALUE na_coerce(VALUE self, VALUE other)
{
  struct NARRAY *a1;

  GetNArray(self,a1);
  return rb_assoc_new( na_cast_object(other,a1->type), self );
}


/* method: inspect() -- returns the inspect of the array */
static VALUE
 na_inspect(VALUE self)
{
  VALUE str;
  struct NARRAY *ary;
  int i;
  char buf[256];
  const char *classname;
  const char *ref = "%s(ref).%s(%i";
  const char *org = "%s.%s(%i";

  GetNArray(self,ary);
  classname = rb_class2name(CLASS_OF(self));

  str = rb_str_new(0,0);
  if (ary->rank < 1) {
    sprintf(buf, "%s.%s(): []", classname, na_typestring[ary->type]);
    rb_str_cat(str,buf,strlen(buf));
  }
  else {
    sprintf(buf, (ary->ref==Qnil) ? org:ref,
	    classname, na_typestring[ary->type], ary->shape[0]);
    rb_str_cat(str,buf,strlen(buf));
    for (i=1; i<ary->rank; ++i) {
      sprintf(buf,",%i",ary->shape[i]);
      rb_str_cat(str,buf,strlen(buf));
    }
    rb_str_cat(str,")",1);
    rb_str_cat(str,": \n",3);
    rb_str_concat(str, na_make_inspect(self));
  }
  return str;
}


/* private function for reshape */
static void
 na_reshape(int argc, VALUE *argv, struct NARRAY *ary, VALUE self)
{
  int *shape, class_dim;
  int  i, total=1, unfixed=-1;
  VALUE klass;

  if (ary->total==0)
    rb_raise(rb_eRuntimeError, "cannot reshape empty array");

  klass = CLASS_OF(self);
  class_dim = NUM2INT(rb_const_get(klass, na_id_class_dim));

  if (argc == 0) {  /* trim ranks of size=1 */
    shape = ALLOCA_N(int,ary->rank+1);
    for (i=0; i<class_dim; ++i) shape[i]=0;
    for (   ; i<ary->rank; ++i) shape[i]=1;
    na_shrink_rank( self, class_dim, shape );
    if (ary->rank==0) ary->rank=1;
    return;
  }

  /* get shape from argument */
  shape = ALLOC_N(int,argc);
  for (i=0; i<argc; ++i)
    switch(TYPE(argv[i])) {
    case T_FIXNUM:
      total *= shape[i] = NUM2INT(argv[i]);
      break;
    case T_TRUE:
      unfixed = i;
      break;
    default:
      rb_raise(rb_eArgError,"illegal type");
    }

  if (unfixed>=0) {
    if (ary->total % total != 0)
      rb_raise(rb_eArgError, "Total size size must be divisor");
    shape[unfixed] = ary->total / total;
  }
  else if (total != ary->total)
    rb_raise(rb_eArgError, "Total size must be same");

  /* exchange */
  xfree(ary->shape);
  ary->shape = shape;
  ary->rank = argc;
}


/* method: reshape!(size1,size2,...,sizeN) */
static VALUE
 na_reshape_bang(int argc, VALUE *argv, VALUE self)
{
  struct NARRAY *ary;

  GetNArray(self,ary);
  na_reshape(argc, argv, ary, self);
  return self;
}


/* method: reshape(size1,size2,...,sizeN) */
static VALUE
 na_reshape_ref(int argc, VALUE *argv, VALUE self)
{
  struct NARRAY *ary;

  GetNArray(self,ary);
  ary = na_ref_alloc_struct(self);
  na_reshape(argc, argv, ary, self);
  return na_wrap_struct(ary,self);
}


/* method: flatten! */
static VALUE
 na_flatten_bang(VALUE self)
{
  struct NARRAY *ary;

  GetNArray(self,ary);
  if (ary->total==0 || ary->rank==0)
    rb_raise(rb_eRuntimeError, "cannot reshape empty array");
  ary->shape[0] = ary->total;
  ary->rank = 1;
  return self;
}


/* method: flatten */
static VALUE
 na_flatten_ref(VALUE self)
{
  return na_flatten_bang( na_wrap_struct( na_ref_alloc_struct(self), self ));
}


/* private function for newdim */
static void
 na_newdim(int argc, VALUE *argv, struct NARRAY *ary)
{
  int *shape, *count;
  int  i, j;

  if (argc==0)
    rb_raise(rb_eArgError, "Argument required");
  if (ary->total==0)
    rb_raise(rb_eRuntimeError, "cannot extend empty array");

  /* count new rank */
  count = ALLOCA_N(int,ary->rank+1);
  for (i=0; i<=ary->rank; ++i)
    count[i]=0;
  for (i=0; i<argc; ++i) {
    j = NUM2INT(argv[i]);
    if (j<0)	/* negative rank : -1=>append after last rank */
      j += ary->rank+1;
    if (j<0 || j>ary->rank)  /* range check */
      rb_raise(rb_eArgError, "rank out of range");
    ++count[j];
  }
  /* extend shape shape */
  shape = ALLOC_N(int,ary->rank+argc);
  for (j=i=0; i<ary->rank; ++i) {
    while (count[i]-->0) shape[j++] = 1;
    shape[j++] = ary->shape[i];
  }
  while (count[i]-->0) shape[j++] = 1;

  /* exchange shape */
  xfree(ary->shape);
  ary->shape = shape;
  ary->rank += argc;
}


/* method: newdim!(size1,size2,...,sizeN) */
static VALUE
 na_newdim_bang(int argc, VALUE *argv, VALUE self)
{
  struct NARRAY *ary;

  GetNArray(self,ary);
  na_newdim(argc, argv, ary);
  return self;
}


/* method: newdim(size1,size2,...,sizeN) */
VALUE
 na_newdim_ref(int argc, VALUE *argv, VALUE self)
{
  struct NARRAY *ary;

  GetNArray(self,ary);
  ary = na_ref_alloc_struct(self);
  na_newdim(argc, argv, ary);
  return na_wrap_struct(ary,self);
}


/* method: fill!(val) */
VALUE na_fill(VALUE self, volatile VALUE val)
{
  struct NARRAY *a1, *a2;

  GetNArray(self,a1);
  val = na_cast_unless_narray(val,a1->type);
  GetNArray(val,a2);

  if (a2->total != 1)
    rb_raise(rb_eArgError, "single-element argument required");

  SetFuncs[a1->type][a2->type]( a1->total,
				a1->ptr, na_sizeof[a1->type],
				a2->ptr, 0 );
  return self;
}


/* method: indgen!([start,[step]]) */
VALUE
 na_indgen(int argc, VALUE *argv, VALUE self)
{
  int start=0, step=1;
  struct NARRAY *ary;

  if (argc>0) {
    start = NUM2INT(argv[0]);
    if (argc==2)
      step = NUM2INT(argv[1]);
    else
      if (argc>2)
	rb_raise(rb_eArgError, "wrong # of arguments (%d for <= 2)", argc);
  }

  GetNArray(self,ary);
  IndGenFuncs[ary->type]( ary->total,
			  ary->ptr, na_sizeof[ary->type],
			  start, step );
  return self;
}


/* method:  where2
   idx_true, idx_false = narray.where2 */
static VALUE
 na_where2(volatile VALUE obj)
{
  VALUE v1, v0;
  int  n, i, n1, n0;
  char *c;
  int32_t *idx1, *idx0;
  struct NARRAY *ary, *a1, *a0; /* a1=true, a0=false */

  GetNArray(obj,ary);
  /* Convert to NA_BYTE by calling "obj.ne(0)", if needed */
  if(ary->type != NA_BYTE) {
    obj = rb_funcall(obj, na_id_ne, 1, INT2FIX(0));
    GetNArray(obj,ary);
  }
  n = ary->total;

  /* Count true */
  c = ary->ptr;
  n1 = 0;
  for (i=0; i<n; ++i)
    if (*(c++)) ++n1;

  n0 = n-n1;

  /* partially true and false */
  v1 = na_make_object( NA_LINT, 1, &n1, cNArray );
  GetNArray(v1,a1);
  idx1 = (int32_t*) a1->ptr;
  v0 = na_make_object( NA_LINT, 1, &n0, cNArray );
  GetNArray(v0,a0);
  idx0 = (int32_t*) a0->ptr;

  /* Get Indices */
  c = ary->ptr;
  for ( i=0; i<n; ++i ) {
    if (*(c++))
      *(idx1++) = i;
    else
      *(idx0++) = i;
  }

  return rb_assoc_new( v1, v0 );
}


/* method:  where
   idx_true = narray.where */
static VALUE
 na_where(VALUE self)
{
  return RARRAY_PTR( na_where2(self) )[0];
}


/* iterator: each() */
static VALUE
 na_each(VALUE obj)
{
  int i, sz;
  VALUE v;
  struct NARRAY *ary;
  char *p;
  void (*func)();

  if (rb_block_given_p()) {
    GetNArray(obj,ary);

    p  = ary->ptr;
    sz = na_sizeof[ary->type];
    func = SetFuncs[NA_ROBJ][ary->type];

    for ( i=ary->total; i-->0; ) {
      (*func)( 1, &v, 0, p, 0 );
      rb_yield(v);
      p += sz;
    }
    return Qnil;
  } else {
    return rb_funcall(obj, rb_intern("to_enum"), 0);
  }
}


/* iterator: collect() */
static VALUE
 na_collect(VALUE obj1)
{
  int i, sz;
  VALUE v, obj2;
  struct NARRAY *a1, *a2;
  char *p1, *p2;
  void (*get)(), (*set)();

  GetNArray(obj1,a1);
  obj2 = na_make_object(a1->type, a1->rank, a1->shape, CLASS_OF(obj1));
  GetNArray(obj2,a2);

  p1  = a1->ptr;
  p2  = a2->ptr;
  sz  = na_sizeof[a1->type];
  get = SetFuncs[NA_ROBJ][a1->type];
  set = SetFuncs[a1->type][NA_ROBJ];

  for ( i=a1->total; i-->0; ) {
    (*get)( 1, &v, 0, p1, 0 );
    v = rb_yield(v);
    (*set)( 1, p2, 0, &v, 0 );
    p1 += sz;
    p2 += sz;
  }
  return obj2;
}


/* iterator: collect!() */
static VALUE
 na_collect_bang(VALUE self)
{
  int i, sz;
  VALUE v;
  struct NARRAY *a1;
  char *p1;
  void (*get)(), (*set)();

  GetNArray(self,a1);

  p1  = a1->ptr;
  sz  = na_sizeof[a1->type];
  get = SetFuncs[NA_ROBJ][a1->type];
  set = SetFuncs[a1->type][NA_ROBJ];

  for ( i=a1->total; i-->0; ) {
    (*get)( 1, &v, 0, p1, 0 );
    v = rb_yield(v);
    (*set)( 1, p1, 0, &v, 0 );
    p1 += sz;
  }
  return self;
}


/* initialization of NArray Class */
void
 Init_narray()
{
    ID id_Complex = rb_intern("Complex");

    if (!rb_const_defined( rb_cObject, id_Complex)) {
	/* require Complex class */
	rb_require("complex");
    }
    cComplex = rb_const_get( rb_cObject, rb_intern("Complex") );

    /* define NArray class */
    cNArray = rb_define_class("NArray",rb_cObject);

    /* class methods */
    rb_define_singleton_method(cNArray,"new",na_s_new,-1);
    rb_define_singleton_method(cNArray,"byte",na_s_new_byte,-1);
    rb_define_singleton_method(cNArray,"sint",na_s_new_sint,-1);
    rb_define_singleton_method(cNArray,"lint",na_s_new_int,-1);
    rb_define_singleton_method(cNArray,"int", na_s_new_int,-1);
    rb_define_singleton_method(cNArray,"sfloat",na_s_new_sfloat,-1);
    rb_define_singleton_method(cNArray,"dfloat",na_s_new_float,-1);
    rb_define_singleton_method(cNArray,"float", na_s_new_float,-1);
    rb_define_singleton_method(cNArray,"scomplex",na_s_new_scomplex,-1);
    rb_define_singleton_method(cNArray,"dcomplex",na_s_new_complex,-1);
    rb_define_singleton_method(cNArray,"complex", na_s_new_complex,-1);
    rb_define_singleton_method(cNArray,"object",na_s_new_object,-1);

    rb_define_singleton_method(cNArray,"to_na",na_s_to_na,-1);
    rb_define_singleton_method(cNArray,"to_narray",na_s_to_na,-1);
    rb_define_singleton_method(cNArray,"[]",na_s_bracket,-1);

    /* methods */
    rb_define_method(cNArray, "shape", na_shape,0);
    rb_define_alias(cNArray,  "sizes","shape");
    rb_define_method(cNArray, "size", na_size,0);
    rb_define_alias(cNArray,  "total","size");
    rb_define_alias(cNArray,  "length","size");
    rb_define_method(cNArray, "rank", na_rank,0);
    rb_define_alias(cNArray,  "dim","rank");
    rb_define_alias(cNArray,  "dimension","rank");
    rb_define_method(cNArray, "typecode", na_typecode,0);
    rb_define_method(cNArray, "element_size", na_element_size,0);
    rb_define_method(cNArray, "empty?", na_is_empty,0);
    rb_define_method(cNArray, "clone", na_clone,0);
    rb_define_alias(cNArray,  "dup","clone");
    rb_define_method(cNArray, "inspect", na_inspect,0);
    rb_define_method(cNArray, "coerce", na_coerce,1);
    rb_define_method(cNArray, "reshape", na_reshape_ref,-1);
    rb_define_method(cNArray, "reshape!", na_reshape_bang,-1);
    rb_define_alias(cNArray,  "shape=","reshape!");
    rb_define_method(cNArray, "newdim", na_newdim_ref,-1);
    rb_define_alias(cNArray,  "newrank","newdim");
    rb_define_method(cNArray, "newdim!", na_newdim_bang,-1);
    rb_define_alias(cNArray,  "newdim=","newdim!");
    rb_define_alias(cNArray,  "newrank!","newdim!");
    rb_define_alias(cNArray,  "newrank=","newdim!");
    rb_define_method(cNArray, "flatten", na_flatten_ref,0);
    rb_define_method(cNArray, "flatten!", na_flatten_bang,0);
    rb_define_method(cNArray, "fill!", na_fill,1);
    rb_define_alias(cNArray,  "fill","fill!");
    rb_define_method(cNArray, "indgen!", na_indgen,-1);
    rb_define_alias(cNArray,  "indgen","indgen!");
    rb_define_method(cNArray, "where", na_where, 0);
    rb_define_method(cNArray, "where2", na_where2, 0);
    rb_define_method(cNArray, "each", na_each,0);
    rb_define_method(cNArray, "collect", na_collect,0);
    rb_define_method(cNArray, "collect!", na_collect_bang,0);
    rb_define_alias(cNArray, "map", "collect");
    rb_define_alias(cNArray, "map!", "collect!");
    rb_define_method(cNArray, "to_s", na_to_s, 0);
    rb_define_method(cNArray, "to_f", na_to_float, 0);
    rb_define_method(cNArray, "to_i", na_to_integer, 0);
    rb_define_method(cNArray, "to_type", na_to_type, 1);
    rb_define_method(cNArray, "to_binary", na_to_binary, 0);
    rb_define_method(cNArray, "to_type_as_binary", na_to_type_as_binary, 1);
    rb_define_method(cNArray, "to_string", na_to_string, 0);

    rb_define_const(cNArray, "NARRAY_VERSION", rb_str_new2(NARRAY_VERSION));
    rb_define_const(cNArray, "BYTE", INT2FIX(NA_BYTE));
    rb_define_const(cNArray, "SINT", INT2FIX(NA_SINT));
    rb_define_const(cNArray, "LINT", INT2FIX(NA_LINT));
    rb_define_const(cNArray, "INT",  INT2FIX(NA_LINT));
    rb_define_const(cNArray, "SFLOAT", INT2FIX(NA_SFLOAT));
    rb_define_const(cNArray, "DFLOAT", INT2FIX(NA_DFLOAT));
    rb_define_const(cNArray, "FLOAT",  INT2FIX(NA_DFLOAT));
    rb_define_const(cNArray, "SCOMPLEX", INT2FIX(NA_SCOMPLEX));
    rb_define_const(cNArray, "DCOMPLEX", INT2FIX(NA_DCOMPLEX));
    rb_define_const(cNArray, "COMPLEX",  INT2FIX(NA_DCOMPLEX));
    rb_define_const(cNArray, "ROBJ", INT2FIX(NA_ROBJ));
    rb_define_const(cNArray, "OBJECT", INT2FIX(NA_ROBJ));
    rb_define_const(cNArray, "NONE", INT2FIX(NA_NONE));
    rb_define_const(cNArray, "CLASS_DIMENSION", INT2FIX(0));
#ifdef WORDS_BIGENDIAN
    rb_define_const(cNArray, "ENDIAN",  INT2FIX(1));
#else
#ifdef DYNAMIC_ENDIAN	/* not supported yet */
    rb_define_const(cNArray, "ENDIAN",  INT2FIX(-1));
#else  /* LITTLE_ENDIAN */
    rb_define_const(cNArray, "ENDIAN",  INT2FIX(0));
#endif
#endif
    /* Reference */
    rb_define_singleton_method(cNArray, "refer", na_s_refer,1);
    rb_define_singleton_method(cNArray, "ref", na_s_refer,1);
    rb_define_method(cNArray, "refer", na_refer,0);
    rb_define_method(cNArray, "original", na_original,0);

    Init_na_array();
    Init_na_index();
    Init_nmath();
    Init_na_funcs();
    Init_na_random();

    cNArrayScalar = rb_define_class("NArrayScalar", cNArray);

    na_id_beg  	= rb_intern("begin");
    na_id_end  	= rb_intern("end");
    na_id_exclude_end	= rb_intern("exclude_end?");
    na_id_real 	= rb_intern("real");
    na_id_imag	= rb_intern("imag");
    na_id_new  	= rb_intern("new");
    na_id_to_i 	= rb_intern("to_i");
    na_id_usec 	= rb_intern("usec");
    na_id_now 	= rb_intern("now");
    na_id_compare = rb_intern("<=>");
    na_id_ne    = rb_intern("ne");
    na_id_and   = rb_intern("&&");
    na_id_or    = rb_intern("||");
    na_id_minus = rb_intern("-@");
    na_id_abs   = rb_intern("abs");
    na_id_power = rb_intern("**");
    na_id_add   = rb_intern("+");
    na_id_sbt   = rb_intern("-");
    na_id_mul   = rb_intern("*");
    na_id_div   = rb_intern("/");
    na_id_mod   = rb_intern("%");
    na_id_coerce_rev = rb_intern("coerce_rev");
    na_id_Complex = rb_intern("Complex");

    na_id_class_dim = rb_intern("CLASS_DIMENSION");

    Init_na_linalg();

    /* NArray extention script */
    rb_require("narray_ext.rb");
}
