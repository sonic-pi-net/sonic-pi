require "mknafunc"

fname = "na_op.c"
$> = open(fname,"w")

upcast_ary = $upcast.collect{|i| '  {'+i.join(", ")+'}'}.join(",\n")

print <<EOM
/*
  #{fname}
  Automatically generated code
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
/* isalpha(3) etc. */
#include <ctype.h>

const int na_upcast[NA_NTYPES][NA_NTYPES] = {
#{upcast_ary} };

const int na_no_cast[NA_NTYPES] =
 { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
const int na_cast_real[NA_NTYPES] =
 { 0, 1, 2, 3, 4, 5, 4, 5, 8 };
const int na_cast_comp[NA_NTYPES] =
 { 0, 6, 6, 6, 6, 7, 6, 7, 8 };
const int na_cast_round[NA_NTYPES] =
 { 0, 1, 2, 3, 3, 3, 6, 7, 8 };
const int na_cast_byte[NA_NTYPES] =
 { 0, 1, 1, 1, 1, 1, 1, 1, 1 };


static void TpErr(void) {
    rb_raise(rb_eTypeError,"illegal operation with this type");
}
static int TpErrI(void) {
    rb_raise(rb_eTypeError,"illegal operation with this type");
    return 0;
}
static void na_zerodiv() {
    rb_raise(rb_eZeroDivError, "divided by 0");
}

static int notnanF(float *n)
{
  return *n == *n;
}
static int notnanD(double *n)
{
  return *n == *n;
}
EOM


#
#  Set Fucs
#
data = [
  [/[O]/,/[O]/,        "*p1 = *p2;"],
  [/[O]/,/[BI]/,       "*p1 = INT2FIX(*p2);"],
  [/[O]/,/[L]/,        "*p1 = INT2NUM(*p2);"],
  [/[O]/,/[FD]/,       "*p1 = rb_float_new(*p2);"],
  [/[O]/,/[XC]/,       "*p1 = rb_complex_new(p2->r,p2->i);"],
  [/[BIL]/,/[O]/,      "*p1 = NUM2INT(*p2);"],
  [/[FD]/,/[O]/,       "*p1 = NUM2DBL(*p2);"],
  [/[XC]/,/[O]/,       "p1->r = NUM2REAL(*p2); p1->i = NUM2IMAG(*p2);"],
  [/[BILFD]/,/[BILFD]/,"*p1 = *p2;"],
  [/[BILFD]/,/[XC]/,   "*p1 = p2->r;"],
  [/[XC]/,/[BILFD]/,   "p1->r = *p2; p1->i = 0;"],
  [/[XC]/,/[XC]/,      "p1->r = p2->r; p1->i = p2->i;"] ]

$func_body = 
  "static void #name#CC(int n, char *p1, int i1, char *p2, int i2)
{
  for (; n; --n) {
    OPERATION
    p1+=i1; p2+=i2;
  }
}
"
mksetfuncs('Set','','',data)



#
#  Unary Funcs
#
$func_body = 
  "static void #name#C(int n, char *p1, int i1, char *p2, int i2)
{
  for (; n; --n) {
    OPERATION
    p1+=i1; p2+=i2;
  }
}
"


mkfuncs('Swp', $swap_types, $swap_types,
 [nil] +
 ["*p1 = *p2;"] + 
 ["na_size16_t x;  swap16(x,*p2);   *p1 = x;"] + 
 ["na_size32_t x;  swap32(x,*p2);   *p1 = x;"] + 
 ["na_size32_t x;  swap32(x,*p2);   *p1 = x;"] + 
 ["na_size64_t x;  swap64(x,*p2);   *p1 = x;"] + 
 ["na_size64_t x;  swap64c(x,*p2);  *p1 = x;"] + 
 ["na_size128_t x; swap128c(x,*p2); *p1 = x;"] + 
 ["*p1 = *p2;"]
)

print <<EOM

/* ------------------------- H2N --------------------------- */
#ifdef WORDS_BIGENDIAN

na_func_t H2NFuncs =
{ TpErr, SetBB, SetII, SetLL, SetFF, SetDD, SetXX, SetCC, SetOO };

na_func_t H2VFuncs =
{ TpErr, SetBB, SwpI, SwpL, SwpF, SwpD, SwpX, SwpC, SetOO };

#else
#ifdef DYNAMIC_ENDIAN  /* not supported yet */
#else  /* LITTLE ENDIAN */

na_func_t H2NFuncs =
{ TpErr, SetBB, SwpI, SwpL, SwpF, SwpD, SwpX, SwpC, SetOO };

na_func_t H2VFuncs =
{ TpErr, SetBB, SetII, SetLL, SetFF, SetDD, SetXX, SetCC, SetOO };

#endif
#endif
EOM

mkfuncs('Neg', $data_types, $data_types,
 [nil] +
 ["*p1 = -*p2;"]*5 + 
 ["p1->r = -p2->r;
    p1->i = -p2->i;"]*2 +
 ["*p1 = rb_funcall(*p2,na_id_minus,0);"]
)

mkfuncs('AddU', $data_types, $data_types,
 [nil] +
 ["*p1 += *p2;"]*5 + 
 ["p1->r += p2->r;
    p1->i += p2->i;"]*2 +
 ["*p1 = rb_funcall(*p1,'+',1,*p2);"]
)

mkfuncs('SbtU', $data_types, $data_types,
 [nil] +
 ["*p1 -= *p2;"]*5 + 
 ["p1->r -= p2->r;
    p1->i -= p2->i;"]*2 +
 ["*p1 = rb_funcall(*p1,'-',1,*p2);"]
)

mkfuncs('MulU', $data_types, $data_types,
 [nil] +
 ["*p1 *= *p2;"]*5 + 
 ["type1 x = *p1;
    p1->r = x.r*p2->r - x.i*p2->i;
    p1->i = x.r*p2->i + x.i*p2->r;"]*2 +
 ["*p1 = rb_funcall(*p1,'*',1,*p2);"]
)

mkfuncs('DivU', $data_types, $data_types,
 [nil] +
 ["if (*p2==0) {na_zerodiv();}
    *p1 /= *p2;"]*3 + 
 ["*p1 /= *p2;"]*2 + 
 ["type1 x = *p1;
    typef a = p2->r*p2->r + p2->i*p2->i;
    p1->r = (x.r*p2->r + x.i*p2->i)/a;
    p1->i = (x.i*p2->r - x.r*p2->i)/a;"]*2 +
 ["*p1 = rb_funcall(*p1,'/',1,*p2);"]
)

mkfuncs('ModU', $data_types, $data_types,
 [nil] +
 ["if (*p2==0) {na_zerodiv();}
    *p1 %= *p2;"]*3 +
 ["*p1 = fmod(*p1, *p2);"]*2 +
 [nil]*2 +
 ["*p1 = rb_funcall(*p1,'%',1,*p2);"]
)


# method: imag=
mkfuncs('ImgSet',$data_types,$real_types,
 [nil]*6 +
 ["p1->i = *p2;"]*2 +
 [nil]
)


mkfuncs('Floor',$int_types,$data_types,[nil] +
 ['copy']*3 + 
 ["*p1 = (typec)floor(*p2);"]*2 + 
 [nil]*3
)

mkfuncs('Ceil',$int_types,$data_types,[nil] +
 ['copy']*3 + 
 ["*p1 = (typec)ceil(*p2);"]*2 + 
 [nil]*3
)

mkfuncs('Round',$int_types,$data_types,[nil] +
 ['copy']*3 + 
# ["*p1 = floor(*p2+0.5);"]*2 + 
 ["if (*p2 >= 0) *p1 = (typec)floor(*p2+0.5);
     else *p1 = (typec)ceil(*p2-0.5);"]*2 + 
 [nil]*3
)

mkfuncs('Abs',$real_types,$data_types,[nil] +
 ["*p1 = *p2;"] + 
 ["*p1 = (*p2<0) ? -*p2 : *p2;"]*4 + 
 ["*p1 = (typec)hypot(p2->r, p2->i);"]*2 +
 ["*p1 = rb_funcall(*p2,na_id_abs,0);"]
)


mkfuncs('Real',$real_types,$data_types,[nil] +
 ['copy']*7 + 
 [nil]
)

mkfuncs('Imag',$real_types,$data_types,[nil] +
 ["*p1 = 0;"]*5 + 
 ["*p1 = p2->i;"]*2 +
 [nil]
)

mkfuncs('Angl',$real_types,$data_types,[nil] +
 [nil]*5 +
 ["*p1 = atan2(p2->i,p2->r);"]*2 +
 [nil]
)

mkfuncs('ImagMul',$comp_types,$data_types,[nil] +
 [nil]*3 +
 ["p1->r = 0; p1->i = *p2;"]*2 + 
 ["p1->r = -p2->i; p1->i = p2->r;"]*2 +
 [nil]
)

mkfuncs('Conj',$data_types,$data_types,[nil] +
 ['copy']*5 + 
 ["p1->r = p2->r; p1->i = -p2->i;"]*2 +
 [nil]
)

mkfuncs('Not', [$data_types[1]]*9, $data_types,
 [nil] +
 ["*p1 = (*p2==0) ? 1:0;"]*5 +
 ["*p1 = (p2->r==0 && p2->i==0) ? 1:0;"]*2 +
 ["*p1 = RTEST(*p2) ? 0:1;"]
)

mkfuncs('BRv', $data_types, $data_types, [nil] +
 ["*p1 = ~(*p2);"]*3 +
 [nil]*4 +
 ["*p1 = rb_funcall(*p2,'~',0);"]
)

mkfuncs('Min', $data_types, $data_types, [nil] +
 ["if (*p1>*p2) *p1=*p2;"]*3 +
 ["if (notnan#C((type1*)p2) && *p1>*p2) *p1=*p2;"]*2 +
 [nil]*2 +
 ["if (FIX2INT(rb_funcall(*p1,na_id_compare,1,*p2))>0) *p1=*p2;"]
)

mkfuncs('Max', $data_types, $data_types, [nil] +
 ["if (*p1<*p2) *p1=*p2;"]*3 +
 ["if (notnan#C((type1*)p2) && *p1<*p2) *p1=*p2;"]*2 +
 [nil]*2 +
 ["if (FIX2INT(rb_funcall(*p1,na_id_compare,1,*p2))<0) *p1=*p2;"]
)


mksortfuncs('Sort', $data_types, $data_types, [nil] +
 ["
{ if (*p1 > *p2) return 1;
  if (*p1 < *p2) return -1;
  return 0; }"]*5 +
 [nil]*2 +
 ["
{ VALUE r = rb_funcall(*p1, na_id_compare, 1, *p2);
  return NUM2INT(r); }"]
)

mksortfuncs('SortIdx', $data_types, $data_types, [nil] +
 ["
{ if (**p1 > **p2) return 1;
  if (**p1 < **p2) return -1;
  return 0; }"]*5 +
 [nil]*2 +
 ["
{ VALUE r = rb_funcall(**p1, na_id_compare, 1, **p2);
  return NUM2INT(r); }"]
)

# indgen
$func_body = 
  "static void #name#C(int n, char *p1, int i1, int p2, int i2)
{
  for (; n; --n) {
    OPERATION
    p1+=i1; p2+=i2;
  }
}
"
mkfuncs('IndGen',$data_types,[$data_types[3]]*8,
 [nil] +
 ["*p1 = (typef)p2;"]*5 +
 ["p1->r = (typef)p2;
   p1->i = 0;"]*2 +
 ["*p1 = INT2FIX(p2);"]
)



$func_body = 
"static void #name#C(int n, char *p1, int i1, char *p2, int i2)
{
  OPERATION
}
"
mkfuncs('ToStr',['']+[$data_types[8]]*8,$data_types,
 [nil] +
 ["char buf[22];
  for (; n; --n) {
    sprintf(buf,\"%i\",(int)*p2);
    *p1 = rb_str_new2(buf);
    p1+=i1; p2+=i2;
  }"]*3 +
 ["char buf[24];
  for (; n; --n) {
    sprintf(buf,\"%.5g\",(double)*p2);
    *p1 = rb_str_new2(buf);
    p1+=i1; p2+=i2;
  }"] +
 ["char buf[24];
  for (; n; --n) {
    sprintf(buf,\"%.8g\",(double)*p2);
    *p1 = rb_str_new2(buf);
    p1+=i1; p2+=i2;
  }"] +
 ["char buf[50];
  for (; n; --n) {
    sprintf(buf,\"%.5g%+.5gi\",(double)p2->r,(double)p2->i);
    *p1 = rb_str_new2(buf);
    p1+=i1; p2+=i2;
  }"] +
 ["char buf[50];
  for (; n; --n) {
    sprintf(buf,\"%.8g%+.8gi\",(double)p2->r,(double)p2->i);
    *p1 = rb_str_new2(buf);
    p1+=i1; p2+=i2;
  }"] +
 ["for (; n; --n) {
    *p1 = rb_obj_as_string(*p2);
    p1+=i1; p2+=i2;
  }"]
)


print <<EOM

/* from numeric.c */
static void na_str_append_fp(char *buf)
{
  if (buf[0]=='-' || buf[0]=='+') ++buf;
  if (ISALPHA(buf[0])) return; /* NaN or Inf */
  if (strchr(buf, '.') == 0) {
      int   len = strlen(buf);
      char *ind = strchr(buf, 'e');
      if (ind) {
          memmove(ind+2, ind, len-(ind-buf)+1);
          ind[0] = '.';
	  ind[1] = '0';
      } else {
          strcat(buf, ".0");
      }
  }
}
EOM

$func_body = 
"static void #name#C(char *p1, char *p2)
{
  OPERATION
}
"
mkfuncs('Insp',['']+[$data_types[8]]*8,$data_types,
 [nil] +
 ["char buf[22];
  sprintf(buf,\"%i\",(int)*p2);
  *p1 = rb_str_new2(buf);"]*3 +
 ["char buf[24];
  sprintf(buf,\"%g\",(double)*p2);
  na_str_append_fp(buf);
  *p1 = rb_str_new2(buf);"] +
 ["char buf[24];
  sprintf(buf,\"%g\",(double)*p2);
  na_str_append_fp(buf);
  *p1 = rb_str_new2(buf);"] +
 ["char buf[50], *b;
  sprintf(buf,\"%g\",(double)p2->r);
  na_str_append_fp(buf);
  b = buf+strlen(buf);
  sprintf(b,\"%+g\",(double)p2->i);
  na_str_append_fp(b);
  strcat(buf,\"i\");
  *p1 = rb_str_new2(buf);"] +
 ["char buf[50], *b;
  sprintf(buf,\"%g\",(double)p2->r);
  na_str_append_fp(buf);
  b = buf+strlen(buf);
  sprintf(b,\"%+g\",(double)p2->i);
  na_str_append_fp(b);
  strcat(buf,\"i\");
  *p1 = rb_str_new2(buf);"] +
 ["*p1 = rb_inspect(*p2);"]
)



#
#   Binary Funcs
#

=begin
# Optimize experiment
$func_body = 
  "static void #name#C(int n, char *p1, int i1, char *p2, int i2, char *p3, int i3)
{
  int i;
  if (i1==sizeof(type1) && i2==sizeof(type1) && i3==sizeof(type1)) {
    type1 *a1=p1, *a2=p2, *a3=p3;
    for (i=0; n; --n,++i) {
      *a1 = *a2 * *a3; +++a1;++a2;++a3;
    }
  } else
    for (; n; --n) {
      OPERATION
      p1+=i1; p2+=i2; p3+=i3;
    }
}
"
mkfuncs('MulB', $data_types, $data_types,
 [nil] +
 ["*p1 = *p2 * *p3;"]*5 + [nil]*3
)
=end

$func_body = 
  "static void #name#C(int n, char *p1, int i1, char *p2, int i2, char *p3, int i3)
{
  for (; n; --n) {
    OPERATION
    p1+=i1; p2+=i2; p3+=i3;
  }
}
"

mkfuncs('AddB', $data_types, $data_types,
 [nil] +
 ["*p1 = *p2 + *p3;"]*5 + 
 ["p1->r = p2->r + p3->r;
    p1->i = p2->i + p3->i;"]*2 +
 ["*p1 = rb_funcall(*p2,'+',1,*p3);"]
)

mkfuncs('SbtB', $data_types, $data_types,
 [nil] +
 ["*p1 = *p2 - *p3;"]*5 + 
 ["p1->r = p2->r - p3->r;
    p1->i = p2->i - p3->i;"]*2 +
 ["*p1 = rb_funcall(*p2,'-',1,*p3);"]
)

mkfuncs('MulB', $data_types, $data_types,
 [nil] +
 ["*p1 = *p2 * *p3;"]*5 + 
 ["type1 x = *p2;
    p1->r = x.r*p3->r - x.i*p3->i;
    p1->i = x.r*p3->i + x.i*p3->r;"]*2 +
 ["*p1 = rb_funcall(*p2,'*',1,*p3);"]
)

mkfuncs('DivB', $data_types, $data_types,
 [nil] +
 ["if (*p3==0) {na_zerodiv();};
    *p1 = *p2 / *p3;"]*3 +
 ["*p1 = *p2 / *p3;"]*2 +
 ["type1 x = *p2;
    typef a = p3->r*p3->r + p3->i*p3->i;
    p1->r = (x.r*p3->r + x.i*p3->i)/a;
    p1->i = (x.i*p3->r - x.r*p3->i)/a;"]*2 +
 ["*p1 = rb_funcall(*p2,'/',1,*p3);"]
)

mkfuncs('ModB', $data_types, $data_types,
 [nil] +
 ["if (*p3==0) {na_zerodiv();};
    *p1 = *p2 % *p3;"]*3 +
 ["*p1 = fmod(*p2, *p3);"]*2 + 
 [nil]*2 +
 ["*p1 = rb_funcall(*p2,'%',1,*p3);"]
)


mkfuncs('MulAdd', $data_types, $data_types,
 [nil] +
 ["*p1 += *p2 * *p3;"]*5 + 
 ["type1 x = *p2;
    p1->r += x.r*p3->r - x.i*p3->i;
    p1->i += x.r*p3->i + x.i*p3->r;"]*2 +
 ["*p1 = rb_funcall(*p1,'+',1,
    rb_funcall(*p2,'*',1,*p3));"]
)

mkfuncs('MulSbt', $data_types, $data_types,
 [nil] +
 ["*p1 -= *p2 * *p3;"]*5 + 
 ["type1 x = *p2;
    p1->r -= x.r*p3->r - x.i*p3->i;
    p1->i -= x.r*p3->i + x.i*p3->r;"]*2 +
 ["*p1 = rb_funcall(*p1,'-',1,
    rb_funcall(*p2,'*',1,*p3));"]
)


#
#   Bit operator
#

mkfuncs('BAn', $data_types, $data_types,
 [nil] +
 ["*p1 = *p2 & *p3;"]*3 + 
 [nil]*4 +
 ["*p1 = rb_funcall(*p2,'&',1,*p3);"]
)

mkfuncs('BOr', $data_types, $data_types,
 [nil] +
 ["*p1 = *p2 | *p3;"]*3 + 
 [nil]*4 +
 ["*p1 = rb_funcall(*p2,'|',1,*p3);"]
)

mkfuncs('BXo', $data_types, $data_types,
 [nil] +
 ["*p1 = *p2 ^ *p3;"]*3 + 
 [nil]*4 +
 ["*p1 = rb_funcall(*p2,'^',1,*p3);"]
)


#
#   Comparison
#

mkfuncs('Eql', [$data_types[1]]*9, $data_types,
 [nil] +
 ["*p1 = (*p2==*p3) ? 1:0;"]*5 +
 ["*p1 = (p2->r==p3->r) && (p2->i==p3->i) ? 1:0;"]*2 +
 ["*p1 = RTEST(rb_equal(*p2, *p3)) ? 1:0;"]
)

mkfuncs('Cmp', [$data_types[1]]*9, $data_types,
 [nil] +
 ["if (*p2>*p3) *p1=1;
    else if (*p2<*p3) *p1=2;
    else *p1=0;"]*5 +
 [nil]*2 +
 ["int v = NUM2INT(rb_funcall(*p2,na_id_compare,1,*p3));
    if (v>0) *p1=1; else if (v<0) *p1=2; else *p1=0;"]
)

mkfuncs('And', [$data_types[1]]*9, $data_types,
 [nil] +
 ["*p1 = (*p2!=0 && *p3!=0) ? 1:0;"]*5 +
 ["*p1 = ((p2->r!=0||p2->i!=0) && (p3->r!=0||p3->i!=0)) ? 1:0;"]*2 +
 ["*p1 = (RTEST(*p2) && RTEST(*p3)) ? 1:0;"]
)

mkfuncs('Or_', [$data_types[1]]*9, $data_types,
 [nil] +
 ["*p1 = (*p2!=0 || *p3!=0) ? 1:0;"]*5 +
 ["*p1 = ((p2->r!=0||p2->i!=0) || (p3->r!=0||p3->i!=0)) ? 1:0;"]*2 +
 ["*p1 = (RTEST(*p2) || RTEST(*p3)) ? 1:0;"]
)

mkfuncs('Xor', [$data_types[1]]*9, $data_types,
 [nil] +
 ["*p1 = ((*p2!=0) == (*p3!=0)) ? 0:1;"]*5 +
 ["*p1 = ((p2->r!=0||p2->i!=0) == (p3->r!=0||p3->i!=0)) ? 0:1;"]*2 +
 ["*p1 = (RTEST(*p2) == RTEST(*p3)) ? 0:1;"]
)


#
#   Atan2
#

mkfuncs('atan2', $data_types, $data_types,
 [nil]*4 +
 ["*p1 = atan2(*p2, *p3);"]*2 +
 [nil]*3
)


#
#   Mask
#
$func_body = 
  "static void #name#C(int n, char *p1, int i1, char *p2, int i2, char *p3, int i3)
{
  for (; n; --n) {
    OPERATION
  }
}
"
mkfuncs('RefMask',$data_types,$data_types,
 [nil] +
 ["if (*(u_int8_t*)p3) { *p1=*p2; p1+=i1; }
    p3+=i3; p2+=i2;"]*8
)

mkfuncs('SetMask',$data_types,$data_types,
 [nil] +
 ["if (*(u_int8_t*)p3) { *p1=*p2; p2+=i2; }
    p3+=i3; p1+=i1;"]*8
)
