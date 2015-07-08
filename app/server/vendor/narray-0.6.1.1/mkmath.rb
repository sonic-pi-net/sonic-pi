require "mknafunc"

# File name
fname = "na_math.c"
$> = open(fname,"w")

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

#ifndef M_LOG2E
#define M_LOG2E         1.4426950408889634074
#endif
#ifndef M_LOG10E
#define M_LOG10E        0.43429448190325182765
#endif

VALUE rb_mNMath;

static void TpErr(void) {
    rb_raise(rb_eTypeError,"illegal operation with this type");
}

#if 0
void sincos(double x, double *s, double *c)
{
  *s=sin(x); *c=cos(x);
}

#ifndef HAVE_ACOSH
static double rb_log1p (const double x)
{
  double y;
  y = 1+x;

  if (y==1)
     return x;
  else
     return log(y)*(x/(y-1));
}

static double zero=0;

static double acosh(double x)
{
   /* acosh(x) = log(x+sqrt(x*x-1)) */
   if (x>2) {
      return log(2*x-1/(sqrt(x*x-1)+x));
   } else if (x>=1) {
      x-=1;
      return rb_log1p(x+sqrt(2*x+x*x));
   }
   return zero/(x-x); /* x<1: NaN */
}

static double asinh(double x)
{
   double a, x2;
   int neg;

   /* asinh(x) = log(x+sqrt(x*x+1)) */
   neg = x<0;
   if (neg) {x=-x;}
   x2 = x*x;

   if (x>2) {
      a = log(2*x+1/(x+sqrt(x2+1)));
   } else {
      a = rb_log1p(x+x2/(1+sqrt(x2+1)));
   }
   if (neg) {a=-a;}
   return a;
}

static double atanh(double x)
{
   double a, x2;
   int neg;

   /* atanh(x) = 0.5*log((1+x)/(1-x)) */
   neg = x<0;
   if (neg) {x=-x;}
   x2 = x*2;

   if (x<0.5) {
      a = 0.5*rb_log1p(x2+x2*x/(1-x));
   } else if (x<1) {
      a = 0.5*rb_log1p(x2/(1-x));
   } else if (x==1) {
      a = 1/zero;        /* Infinity */
   } else {
      return zero/(x-x); /* x>1: NaN */
   }
   if (neg) {a=-a;}
   return a;
}
#endif
#endif

static void squareX(scomplex *x) {
  float r=x->r;
  x->r = r*r - x->i*x->i;
  x->i = 2*r*x->i;
}

static void squareC(dcomplex *x) {
  double r=x->r;
  x->r = r*r - x->i*x->i;
  x->i = 2*r*x->i;
}


static void mulX(scomplex *x, scomplex *y) {
  scomplex z=*x;
  x->r = z.r*y->r - z.i*y->i;
  x->i = z.r*y->i + z.i*y->r;
}

static void mulC(dcomplex *x, dcomplex *y) {
  dcomplex z=*x;
  x->r = z.r*y->r - z.i*y->i;
  x->i = z.r*y->i + z.i*y->r;
}


static void divX(scomplex *p1, scomplex *p2) {
  scomplex x = *p1;
  float    a = p2->r*p2->r + p2->i*p2->i;
  p1->r = (x.r*p2->r + x.i*p2->i)/a;
  p1->i = (x.i*p2->r - x.r*p2->i)/a;
}

static void divC(dcomplex *p1, dcomplex *p2) {
  dcomplex x = *p1;
  double   a = p2->r*p2->r + p2->i*p2->i;
  p1->r = (x.r*p2->r + x.i*p2->i)/a;
  p1->i = (x.i*p2->r - x.r*p2->i)/a;
}


static scomplex recipX(scomplex *z)
{
  scomplex r;
  float    n;

  if ( (z->r<0 ? -z->r:z->r) > (z->i<0 ? -z->i:z->i) ) {
    r.i  = z->i/z->r;
    n    = (1+r.i*r.i)*z->r;
    r.r  = 1/n;
    r.i /= -n;
  } else {
    r.r  = z->r/z->i;
    n    = (1+r.r*r.r)*z->i;
    r.r /= n;
    r.i  = -1/n;
  }
  return r;
}

static dcomplex recipC(dcomplex *z)
{
  dcomplex r;
  double   n;

  if ( (z->r<0 ? -z->r:z->r) > (z->i<0 ? -z->i:z->i) ) {
    r.i  = z->i/z->r;
    n    = (1+r.i*r.i)*z->r;
    r.r  = 1/n;
    r.i /= -n;
  } else {
    r.r  = z->r/z->i;
    n    = (1+r.r*r.r)*z->i;
    r.r /= n;
    r.i  = -1/n;
  }
  return r;
}


static int powInt(int x, int p)
{
  int r=1;

  switch(p) {
  case 2: return x*x;
  case 3: return x*x*x;
  case 1: return x;
  case 0: return 1;
  }
  if (p<0)  return 0;
  /* if(p>3) */	
  while (p) {
    if ( (p%2) == 1 ) r *= x;
    x *= x;
    p /= 2;
  }
  return r;
}


static float powFi(float x, int p)
{
  float r=1;

  switch(p) {
  case 2: return x*x;
  case 3: return x*x*x;
  case 1: return x;
  case 0: return 1;
  }
  if (p<0)  return 1/powFi(x,-p);
  /* if(p>3) */	
  while (p) {
    if ( (p%2) == 1 ) r *= x;
    x *= x;
    p /= 2;
  }
  return r;
}


static double powDi(double x, int p)
{
  double r=1;

  switch(p) {
  case 2: return x*x;
  case 3: return x*x*x;
  case 1: return x;
  case 0: return 1;
  }
  if (p<0)  return 1/powDi(x,-p);
  /* if(p>3) */	
  while (p) {
    if ( (p%2) == 1 ) r *= x;
    x *= x;
    p /= 2;
  }
  return r;
}


static scomplex powXi(scomplex *x, int p)
{
  scomplex y=*x, r={1,0};

  if (p==2) { squareX(&y); return y; }
  if (p==1) { return y; }
  if (p==0) { return r; }
  if (p<0) {
    y = powXi(x,-p);
    return recipX(&y);
  }
  /* if (p>2) */
  while (p) {
    if ( (p%2) == 1 ) mulX(&r,&y);
    squareX(&y);
    p /= 2;
  }
  return r;
}

static dcomplex powCi(dcomplex *x, int p)
{
  dcomplex y=*x, r={1,0};

  if (p==2) { squareC(&y); return y; }
  if (p==1) { return y; }
  if (p==0) { return r; }
  if (p<0) {
    y = powCi(x,-p);
    return recipC(&y);
  }
  /* if (p>2) */
  while (p) {
    if ( (p%2) == 1 ) mulC(&r,&y);
    squareC(&y);
    p /= 2;
  }
  return r;
}


EOM

data = [
['sqrt',
[nil]*4 +
["{ *p1 = sqrt(*p2); }"]*2 +
["{
  typer xr=p2->r/2, xi=p2->i/2, r=hypot(xr,xi);
  if (xr>0) {
    p1->r = sqrt(r+xr);
    p1->i = xi/p1->r;
  } else if ( (r-=xr) ) {
    p1->i = (xi>=0) ? sqrt(r):-sqrt(r);
    p1->r = xi/p1->i;
  } else {
    p1->r = p1->i = 0;
  }
}"]*2 +
[nil] ],

['sin',
[nil]*4 +
["{ *p1 = sin(*p2); }"]*2 +
["{
  p1->r = sin(p2->r)*cosh(p2->i);
  p1->i = cos(p2->r)*sinh(p2->i); }"]*2 +
[nil] ],

['cos',
[nil]*4 +
["{ *p1 = cos(*p2); }"]*2 +
["{
  p1->r = cos(p2->r)*cosh(p2->i);
  p1->i = -sin(p2->r)*sinh(p2->i); }"]*2 +
[nil] ],

['tan',
[nil]*4 +
["{ *p1 = tan(*p2); }"]*2 +
["{
  typer d, th;
  p1->i = th = tanh(2*p2->i);
  p1->r = sqrt(1-th*th); /* sech */
  d  = 1 + cos(2*p2->r) * p1->r;
  p1->r *= sin(2*p2->r)/d;
  p1->i /= d;
}"]*2 +
[nil] ],

['sinh',
[nil]*4 +
["{ *p1 = sinh(*p2); }"]*2 +
["{
  p1->r = sinh(p2->r)*cos(p2->i);
  p1->i = cosh(p2->r)*sin(p2->i);
}"]*2 +
[nil] ],

['cosh',
[nil]*4 +
["{ *p1 = cosh(*p2); }"]*2 +
["{
  p1->r = cosh(p2->r)*cos(p2->i);
  p1->i = sinh(p2->r)*sin(p2->i);
}"]*2 +
[nil] ],

['tanh',
[nil]*4 +
["{ *p1 = tanh(*p2); }"]*2 +
["{
  typer d, th;
  p1->r = th = tanh(2*p2->r);
  p1->i = sqrt(1-th*th); /* sech */
  d  = 1 + cos(2*p2->i) * p1->i;
  p1->r /= d;
  p1->i *= sin(2*p2->i)/d;
}"]*2 +
[nil] ],

['exp',
[nil]*4 +
["{ *p1 = exp(*p2); }"]*2 +
["{
  typer a = exp(p2->r);
  p1->r = a*cos(p2->i);
  p1->i = a*sin(p2->i);
}"]*2 +
[nil] ],

['log',
[nil]*4 +
["{ *p1 = log(*p2); }"]*2 +
["{
  typed x = *p2;
  p1->r = log(hypot(x.r, x.i));
  p1->i = atan2(x.i, x.r);
}"]*2 +
[nil] ],

['log10',
[nil]*4 +
["{ *p1 = log10(*p2); }"]*2 +
["{
  log#code(p1,p2);
  p1->r *= (typer)M_LOG10E;
  p1->i *= (typer)M_LOG10E;
}"]*2 +
[nil] ],


['log2',
[nil]*4 +
["{ *p1 = log(*p2)*M_LOG2E; }"]*2 +
["{
  log#code(p1,p2);
  p1->r *= (typer)M_LOG2E;
  p1->i *= (typer)M_LOG2E;
}"]*2 +
[nil] ],


['asin',
[nil]*4 +
["{ *p1 = asin(*p2); }"]*2 +
# -i * log( sqrt(1-x**2) + x*i )
["{
  typed x = *p2;
  square#code(&x);
  x.r = 1 - x.r;
  x.i =   - x.i;
  sqrt#code(&x,&x);
  x.r -= p2->i;
  x.i += p2->r;
  log#code(&x,&x);
  p1->r =  x.i;
  p1->i = -x.r;
}"]*2 +
[nil]*1 ],

['asinh',
[nil]*4 +
["{ *p1 = asinh(*p2); }"]*2 +
# log(sqrt(x**2+1)+x)
["{
  typed x = *p2;
  square#code(&x);
  x.r += 1;
  sqrt#code(&x,&x);
  x.r += p2->r;
  x.i += p2->i;
  log#code(p1,&x);
}"]*2 +
[nil]*1 ],

['acos',
[nil]*4 +
["{ *p1 = acos(*p2); }"]*2 +
# -i * log( sqrt(1-x**2)*i + x )
["{
  typed x = *p2;
  typer tmp;
  square#code(&x);
  x.r = 1 - x.r;
  x.i =   - x.i;
  sqrt#code(&x,&x);
  tmp =  x.r + p2->i;
  x.r = -x.i + p2->r;
  x.i = tmp;
  log#code(&x,&x);
  p1->r =  x.i;
  p1->i = -x.r;
}"]*2 +
[nil]*1 ],

['acosh',
[nil]*4 +
["{ *p1 = acosh(*p2); }"]*2 +
# log(x+sqrt(x**2-1))
["{
  typed x = *p2;
  square#code(&x);
  x.r -= 1;
  sqrt#code(&x,&x);
  x.r += p2->r;
  x.i += p2->i;
  log#code(p1,&x);
}"]*2 +
[nil]*1 ],

['atan',
[nil]*4 +
["{ *p1 = atan(*p2); }"]*2 +
# i/2 * log((i+x)/(i-x))
["{
  typed x,y;
  x.r=-p2->r; x.i=1-p2->i;
  y.r= p2->r; y.i=1+p2->i;
  div#code((void*)&y,(void*)&x);
  log#code((void*)&x,(void*)&y);
  p1->r = -x.i/2;
  p1->i =  x.r/2;
}"]*2 +
[nil]*1 ],

['atanh',
[nil]*4 +
["{ *p1 = atanh(*p2); }"]*2 +
# 1/2 * log((1+x)/(1-x))
["{
  typed x,y;
  x.r=1-p2->r; x.i=-p2->i;
  y.r=1+p2->r; y.i= p2->i;
  div#code((void*)&y,(void*)&x);
  log#code((void*)&x,(void*)&y);
  p1->r = x.r/2;
  p1->i = x.i/2;
}"]*2 +
[nil]*1 ] ]



def mkmathfuncs(bsname,func)

  print "
/* ------------------------- #{bsname} --------------------------- */\n"
  c  = $type_codes
  tr = $real_types
  td = $data_types
  name = bsname

  # Function Definition
  head = "static void #{name}#code(void *p1, void *p2)"
  for i in 0...c.size
    if func[i] != nil && func[i]=~/^\{/
      f = func[i].
	gsub(/p1->/,"((#{td[i]}*)p1)->").
	gsub(/p2->/,"((#{td[i]}*)p2)->").
	gsub(/\*p1/,"*(#{td[i]}*)p1").
	gsub(/\*p2/,"*(#{td[i]}*)p2").
	gsub(/typer/, tr[i]).
	gsub(/typed/, td[i])
      puts( (head+f).gsub(/#code/,c[i]) )
    end
  end

  # Function Array

  print "\nna_mathfunc_t #{name}Funcs =\n{ "
  m = []
  for i in 0...c.size
    if func[i] == nil
      m += ['TpErr']
    elsif func[i]=='copy'
      m += ['Set'+c[i]*2]
    elsif !( func[i] =~ /^\{/ )
      m += [func[i]]
    else
      m += [name+c[i]]
    end
  end
  print m.join(", ")+" };\n"

end


# Function Definitions
for i in data
  mkmathfuncs( i[0], i[1] )
end


#
#  Recip
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
mkfuncs('Rcp', $data_types, $data_types,
 [nil] +
 ["*p1 = 1/(*p2);"]*5 + 
 ["*p1 = recip#C((type1*)p2);"]*2 +
 ["*p1 = rb_funcall(INT2FIX(1),na_id_div,1,*p2);"]
)


#
#   Power
#
def mkpowfuncs(name,funcs)

  print "
/* ------------------------- #{name} --------------------------- */\n"
  c  = $type_codes
  n  = $type_codes.size
  td = $data_types
  tr = $real_types

  # Function Definition

  for i in 0...n
    for j in 0...n
      funcs.each do |k|
	if c[i]=~k[0] && c[j]=~k[1]
	  tu = $data_types[$upcast[i][j]]
	  f = k[2].
	    gsub(/p1->/,"((#{tu}*)p1)->").
	    gsub(/p2->/,"((#{td[i]}*)p2)->").
	    gsub(/p3->/,"((#{td[j]}*)p3)->").
	    gsub(/\*p1/,"*(#{tu}*)p1").
	    gsub(/\*p2/,"*(#{td[i]}*)p2").
	    gsub(/\*p3/,"*(#{td[j]}*)p3").
	    gsub(/typed/,td[i]).
            gsub(/typef/,tr[i])
	  puts $func_body.
	    gsub(/#name/,name).
	    sub(/OPERATION/,f).
	    gsub(/#CC/,c[i]+c[j]).
	    gsub(/#C/, c[i])
	end
      end
    end
  end

  # function pointer array
  print "\nna_setfunc_t "+name+"Funcs = {\n"
  m = []
  for i in 0...n
    l = []
    for j in 0...n
      f = true
      for k in funcs
	if c[i]=~k[0] && c[j]=~k[1]
	  l += [name+c[i]+c[j]]
	  f = false
	  break
	end
      end
      if f
	l += ['TpErr']
      end
    end
    m += ['  { '+l.join(', ')+' }']
  end
  print m.join(",\n")+"\n};\n"

end

$func_body = 
"static void #name#CC(int n, char *p1, int i1, char *p2, int i2, char *p3, int i3)
{
  for (; n; --n) {
    OPERATION
    p1+=i1; p2+=i2; p3+=i3;
  }
}
"
mkpowfuncs('Pow',
[
[/[O]/,/[O]/,     "*p1 = rb_funcall(*p2,na_id_power,1,*p3);"],
[/[BIL]/,/[BIL]/, "*p1 = powInt(*p2,*p3);"],
[/[FD]/,/[BIL]/,  "*p1 = pow#Ci(*p2,*p3);"],
[/[BILFD]/,/[FD]/,"*p1 = pow(*p2,*p3);"],
[/[XC]/,/[BIL]/,  "*p1 = pow#Ci((typed*)p2,*p3);"],
[/[XC]/,/[FD]/,
   "typed r;
    if (*p3==0)
    { p1->r=1; p1->i=0; } else
    if (p2->r==0 && p2->i==0 && *p3>0)
    { p1->r=0; p1->i=0; } else {
    log#C(&r, p2);
    r.r *= *p3;
    r.i *= *p3;
    exp#C(p1, &r); }"],
[/[XC]/,/[XC]/,
   "typed l, r;
    if (p3->r==0 && p3->i==0)
    { p1->r=1; p1->i=0; } else
    if (p2->r==0 && p2->i==0 && p3->r>0 && p3->i==0)
    { p1->r=0; p1->i=0; } else {
    log#C(&l, p2);
    r.r = p3->r * l.r - p3->i * l.i;
    r.i = p3->r * l.i + p3->i * l.r;
    exp#C(p1, &r); }"]
])


# Execution
print <<EOM


/* ------------------------- Execution -------------------------- */

static void
 na_exec_math(struct NARRAY *a1, struct NARRAY *a2, void (*func)())
{
  int  i, s1, s2;
  char *p1, *p2;

  s1 = na_sizeof[a1->type];
  s2 = na_sizeof[a2->type];
  p1 = a1->ptr;
  p2 = a2->ptr;
  for (i=a1->total; i ; i--) {
    (*func)( p1, p2 );
    p1 += s1;
    p2 += s2;
  }
}


static VALUE
 na_math_func(volatile VALUE self, na_mathfunc_t funcs)
{
  struct NARRAY *a1, *a2;
  VALUE ans;

  if (TYPE(self) == T_ARRAY) {
    self = na_ary_to_nary(self,cNArray);
  } else
  if (!IsNArray(self)) {
    self = na_make_scalar(self,na_object_type(self));
  }

  GetNArray(self,a2);
  if (NA_IsINTEGER(a2)) {
    self = na_upcast_type(self,NA_DFLOAT);
    GetNArray(self,a2);
  }
  ans = na_make_object(a2->type, a2->rank, a2->shape, CLASS_OF(self));
  GetNArray(ans,a1);

  na_exec_math(a1, a2, funcs[a2->type]);

  if (CLASS_OF(self) == cNArrayScalar)
    SetFuncs[NA_ROBJ][a1->type](1,&ans,0,a1->ptr,0);    

  return ans;
}
EOM


# Module Methods
print <<EOM

/* ------------------------- Module Methods -------------------------- */
EOM
for i in data
  bsname=i[0]
  name=bsname
  print <<EOM

/*
 *  call-seq:
 *     NMath.#{name}(arg)  -> narray
 */
static VALUE na_math_#{bsname}(VALUE obj, VALUE x)
{ return na_math_func(x,#{name}Funcs); }
EOM
end



# Initializer
print <<EOM


/* Initialization of NMath module */
void Init_nmath(void)
{
  /* define ExtMath module */
  rb_mNMath = rb_define_module("NMath");

  /* methods */
EOM

for i in data
  print "  rb_define_module_function(rb_mNMath,\"#{i[0]}\",na_math_#{i[0]},1);\n"
end

print <<EOM
}
EOM
