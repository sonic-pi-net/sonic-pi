#! /bin/octave -qf

arg_list = argv();
TYPE   = arg_list{1};
OP     = arg_list{2};
ARRSZ  = str2num(arg_list{3});
REPEAT = str2num(arg_list{4});
n = ARRSZ;

switch(TYPE)
  case "float"
    a = linspace(0,n-1,n);
    b = linspace(0,n-1,n);
  case "int"
    a = int32(linspace(0,n-1,n));
    b = int32(linspace(0,n-1,n));
  case "complex"
    a = complex(linspace(0,n-1,n));
    b = complex(linspace(0,n-1,n));
  case "float_cross"
    a = linspace(0,n-1,n)';
    b = linspace(0,n-1,n);
  case "float_matrix"
    a = linspace(0,n*n-1,n*n);
    a = rem(a, n+1) + 1;
    a = reshape(a,n,n);
    b = linspace(0,n*n-1,n*n);
    b = rem(b, n-1) + 1;
    b = reshape(b,n,n);
  case "float_solve"
    a = linspace(0,n*n-1,n*n);
    a = rem(a, n+1) + 1;
    a = reshape(a,n,n);
    b = reshape(linspace(1,n*n,n*n),n,n);
endswitch

[t1, u1, s1] = cputime ();
switch(OP)
  case "add"
    for i = 1:REPEAT
      c = a + b;
    endfor
  case "mul"
    for i = 1:REPEAT
      c = a .* b;
    endfor
  case "matmul"
    for i = 1:REPEAT
      c = a * b;
    endfor
    #size(c)
  case "solve"
    for i = 1:REPEAT
      c = a \ b;
    endfor
    #size(c)
endswitch
[t2, u2, s2] = cputime ();

printf ("Octave type=%s size=%d op=%s repeat=%d  Time: %.2f sec", 
	TYPE,ARRSZ,OP,REPEAT,u2 - u1);
