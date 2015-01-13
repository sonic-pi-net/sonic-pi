$type_codes = %w(n B I L F D X C O)
$data_types = 
 %w(none u_int8_t int16_t int32_t float double scomplex dcomplex VALUE)
$real_types =
 %w(none u_int8_t int16_t int32_t float double float double VALUE)
$int_types  = 
 %w(none u_int8_t int16_t int32_t int32_t int32_t scomplex dcomplex VALUE)
$comp_types =
 %w(none scomplex scomplex scomplex scomplex dcomplex scomplex dcomplex VALUE)
$swap_types  = 
 %w(none u_int8_t na_size16_t na_size32_t na_size32_t na_size64_t na_size64_t na_size128_t VALUE)
$upcast = [
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [ 0, 1, 2, 3, 4, 5, 6, 7, 8],
  [ 0, 2, 2, 3, 4, 5, 6, 7, 8],
  [ 0, 3, 3, 3, 4, 5, 6, 7, 8],
  [ 0, 4, 4, 4, 4, 5, 6, 7, 8],
  [ 0, 5, 5, 5, 5, 5, 7, 7, 8],
  [ 0, 6, 6, 6, 6, 7, 6, 7, 8],
  [ 0, 7, 7, 7, 7, 7, 7, 7, 8],
  [ 0, 8, 8, 8, 8, 8, 8, 8, 8] ]
$data_obj = [
  [/[O]/,/[O]/,  "
    *p1 = rb_funcall(*p1,#id,1,*p2);"],
  [/[O]/,/[BIL]/,"
    *p1 = rb_funcall(*p1,#id,1,INT2FIX(*p2));"],
  [/[O]/,/[FD]/, "
    *p1 = rb_funcall(*p1,#id,1,rb_float_new(*p2));"],
  [/[O]/,/[XC]/, "
    *p1 = rb_funcall(*p1,#id,1,rb_complex_new(p2->r,p2->i));"],
  [/[BIL]/,/[O]/,"
    *p1 = NUM2INT(rb_funcall(INT2FIX(*p1),#id,1,*p2));"],
  [/[FD]/,/[O]/, "
    *p1 = NUM2DBL(rb_funcall(rb_float_new(*p1),#id,1,*p2));"],
  [/[XC]/,/[O]/, "VALUE v=rb_funcall(rb_complex_new(p1->r,p1->i),#id,1,*p2);
    p1->r = NUM2REAL(v); p1->i = NUM2IMAG(v);"] ]


def mksetfuncs(name,op,id,funcs)

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
	  #if i==j
	  #  f = "memcpy(p1,p1,sizeof(typed));"
	  #else
	    f = k[2]
	  #end
	  f = f.
	    gsub(/p1->/,"((#{td[i]}*)p1)->").
	    gsub(/p2->/,"((#{td[j]}*)p2)->").
	    gsub(/\*p1/,"*(#{td[i]}*)p1").
	    gsub(/\*p2/,"*(#{td[j]}*)p2").
	    gsub(/ = /," = (#{tr[i]})").
            gsub(/#id/,id).
            gsub(/#op/,op).
	    gsub(/typed/,td[i]).
            gsub(/typef/,tr[i])
	  puts $func_body.
	    gsub(/#name/,name).
	    sub(/OPERATION/,f).
	    gsub(/#CC/,c[i]+c[j])
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



def mkfuncs(name,t1,t2,func)

  print "
/* ------------------------- #{name} --------------------------- */\n"
  c  = $type_codes
  td = $data_types
  tr = $real_types

  for i in 0...c.size
    if func[i] != nil && func[i] != "copy"
      f = func[i].
	gsub(/p1->/,"((#{t1[i]}*)p1)->").
	gsub(/p2->/,"((#{t2[i]}*)p2)->").
	gsub(/p3->/,"((#{t2[i]}*)p3)->").
	gsub(/\*p1/,"*(#{t1[i]}*)p1").
	gsub(/\*p2/,"*(#{t2[i]}*)p2").
	gsub(/\*p3/,"*(#{t2[i]}*)p3").
	gsub(/type1/,td[i]).
	gsub(/typec/,t1[i]).
	gsub(/typef/,tr[i])
      puts $func_body.
	gsub(/#name/,name).
	sub(/OPERATION/,f).
	gsub(/#C/,c[i])
    end
  end

  # Function Array

  print "\nna_func_t #{name}Funcs =\n{ "
  m = []
  for i in 0...c.size
    if func[i] == nil
      m += ['TpErr']
    elsif func[i]=='copy'
      m += ['Set'+c[$data_types.index(t1[i])]+c[i]]
    else
      m += [name+c[i]]
    end
  end
  print m.join(", ")+" };\n"

end



def mksortfuncs(bsname,t1,t2,func)

  print "
/* ------------------------- #{bsname} --------------------------- */\n"
  c  = $type_codes
  tf = $real_types
  name = bsname

  # Function Definition
  head = "static int #{name}#code(const void *p1, const void *p2)"
  for i in 0...c.size
    if func[i] != nil && func[i]=~/^\{/
      f = func[i].
	gsub(/p1->/,"((#{t1[i]}*)p1)->").
	gsub(/p2->/,"((#{t2[i]}*)p2)->").
	gsub(/\*\*p1/,"**(#{t1[i]}**)p1").
	gsub(/\*\*p2/,"**(#{t2[i]}**)p2").
	gsub(/\*p1/,"*(#{t1[i]}*)p1").
	gsub(/\*p2/,"*(#{t2[i]}*)p2").
	gsub(/typef/,tf[i])
      puts( (head+f).gsub(/#code/,c[i]) )
    end
  end

  # Function Array

  print "\nna_sortfunc_t #{name}Funcs =\n{ "
  m = []
  for i in 0...c.size
    if func[i] == nil
      m += ['(int (*)(const void *, const void *))TpErrI']
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

