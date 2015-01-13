#  Numerical Array Extention for Ruby
#    (C) Copyright 2000-2008 by Masahiro TANAKA
#
#  This program is free software.
#  You can distribute/modify this program
#  under the same terms as Ruby itself.
#  NO WARRANTY.
#
class NArray

  def self.cast(array,type=nil)
    case array
    when NArray
    when Array
      array = NArray.to_na(array)
    else
      raise ArgumentError, "1st argument must be NArray or Array"
    end
    type = array.typecode if type.nil?
    shape = array.shape
    na = self.new(type,*shape)
    na[] = array
    na
  end

  def integer?
    self.typecode==NArray::BYTE ||
    self.typecode==NArray::SINT ||
    self.typecode==NArray::LINT
  end
  def complex?
    self.typecode==NArray::DCOMPLEX ||
    self.typecode==NArray::SCOMPLEX
  end

  def all?
    where.size == size
  end

  def any?
    where.size > 0
  end

  def none?
    where.size == 0
  end

  def ==(other)
    other.kind_of?(NArray) &&
      shape == other.shape && 
      eq(other).all?
  end

  def eql?(other)
    self.class == other.class &&
      typecode == other.typecode &&
      shape == other.shape &&
      case typecode
      when NArray::OBJECT
	to_a.eql? other.to_a
      else
	to_s.eql? other.to_s
      end
  end

  def hash
    case typecode
    when NArray::OBJECT
      [self.class, to_a].hash
    else
      [self.class, typecode, shape, to_s].hash
    end
  end

  def rank_total(*ranks)
    if ranks.size>0
      idx = []
      ranks.each{|i| idx.push(*i)}
      # ranks is expected to be, e.g., [1, 3..5, 7]
      a = self.shape
      n = 1
      idx.each{|i| n *= a[i]}
      n
    else
      self.total
    end
  end

  # delete rows/columns
  def delete_at(*args)
    if args.size > self.rank
      raise ArgumentError, "too many arguments"
    end
    shp = self.shape
    ind = []
    self.rank.times do |i|
      n = shp[i]
      case a=args[i]
      when Integer
        a = n+a if a<0
        raise IndexError, "index(%d) out of range"%[a] if a<0
        x = [0...a,a+1...n]
      when Range
        b = a.first
        b = n+b if b<0
        raise IndexError, "index(%s) out of range"%[a] if b<0
        e = a.last
        e = n+e if e<0
        e -= 1 if a.exclude_end?
        raise IndexError, "index(%s) out of range"%[a] if e<0
        x = [0...b,e+1...n]
      when Array
        x = (0...n).to_a
        x -= a.map do |j|
          raise IndexError, "contains non-integer" unless Integer===j
          (j<0) ? n+j : j
        end
      else
        if a
          raise ArgumentError, "invalid argument"
        else
          x = true
        end
      end
      ind << x
    end
    self[*ind]
  end

# Statistics
  def mean(*ranks)
    if integer?
      a = self.to_type(NArray::DFLOAT)
    else
      a = self
    end
    a = NArray.ref(a)
    a.sum(*ranks) / (rank_total(*ranks))
  end

  def stddev(*ranks)
    if integer?
      a = self.to_type(NArray::DFLOAT)
    else
      a = self
    end
    a = NArray.ref(a)
    n = rank_total(*ranks)
    if complex?
      NMath::sqrt( (( a-a.accum(*ranks).div!(n) ).abs**2).sum(*ranks)/(n-1) )
    else
      NMath::sqrt( (( a-a.accum(*ranks).div!(n) )**2).sum(*ranks)/(n-1) )
    end
  end

  def rms(*ranks)
    if integer?
      a = self.to_type(NArray::DFLOAT)
    else
      a = self
    end
    a = NArray.ref(a)
    n = rank_total(*ranks)
    if complex?
      NMath::sqrt( (a.abs**2).sum(*ranks)/n )
    else
      NMath::sqrt( (a**2).sum(*ranks)/n )
    end
  end

  def rmsdev(*ranks)
    if integer?
      a = self.to_type(NArray::DFLOAT)
    else
      a = self
    end
    a = NArray.ref(a)
    n = rank_total(*ranks)
    if complex?
      NMath::sqrt( (( a-a.accum(*ranks).div!(n) ).abs**2).sum(*ranks)/n )
    else
      NMath::sqrt( (( a-a.accum(*ranks).div!(n) )**2).sum(*ranks)/n )
    end
  end

  def median(rank=nil)
    shape = self.shape
    rank = shape.size-1 if rank==nil
    s = sort(rank).reshape!(true,*shape[rank+1..-1])
    n = s.shape[0]
    if n%2==1
      s[n/2,false]
    else
      s[n/2-1..n/2,false].sum(0)/2
    end
  end


# Normal distributed random number;  valid for floating point types
  def randomn
    size = self.size
    case type = self.typecode
    when COMPLEX; type=FLOAT
    when SCOMPLEX; type=SFLOAT
    when FLOAT
    when SFLOAT
    else
      raise TypeError, "NArray type must be (S)FLOAT or (S)COMPLEX."
    end
    rr = NArray.new(type,size)
    xx = NArray.new(type,size)
    i = 0
    while i < size
      n = size-i
      m = ((n+Math::sqrt(n))*1.27).to_i
      x = NArray.new(type,m).random!(1) * 2 - 1
      y = NArray.new(type,m).random!(1) * 2 - 1
      r = x**2 + y**2
      idx = (r<1).where
      idx = idx[0...n] if idx.size > n
      if idx.size>0
	rr[i] = r[idx]
	xx[i] = x[idx]
	i += idx.size
      end
    end
    # Box-Muller transform
    rr = ( xx * NMath::sqrt( -2 * NMath::log(rr) / rr ) )
    # finish
    rr.reshape!(*self.shape) if self.rank > 1
    rr = rr.to_type(self.typecode) if type!=self.typecode
    if RUBY_VERSION < "1.8.0"
      self.type.refer(rr)
    else
      self.class.refer(rr)
    end
  end
  #alias randomn! randomn

  def randomn!
    self[]= randomn
    self
  end

  def reverse(*ranks)
    if self.rank==0
      return self.dup
    elsif ranks.size==0
      idx = (0...self.rank).map{-1..0}
    else
      idx = [true]*self.rank
      ranks.each do |i|
        if !i.kind_of?(Integer)
          raise ArgumentError, "Argument must be Integer"
        end
        if i >= self.rank
          raise ArgumentError, "dimension(%s) out of range"%[i]
        end
        idx[i] = -1..0
      end
    end
    self[*idx]
  end

  def rot90(n_times=1)
    if self.rank < 2
      raise "must be >= 2 dimensional array"
    end
    case n_times%4
    when 0
      self.dup
    when 1
      self.transpose(1,0).reverse(0)
    when 2
      self.reverse(0,1)
    when 3
      self.transpose(1,0).reverse(1)
    end
  end

  #SFloatOne = NArray.sfloat(1).fill!(1)
end


module NMath
  PI = Math::PI
  E = Math::E

  def recip x
    1/x.to_f
  end

# Trigonometric function
  def csc x
    1/sin(x)
  end
  def csch x
    1/sinh(x)
  end
  def acsc x
    asin(1/x.to_f)
  end
  def acsch x
    asinh(1/x.to_f)
  end

  def sec x
    1/cos(x)
  end
  def sech x
    1/cosh(x)
  end
  def asec x
    acos(1/x.to_f)
  end
  def asech x
    acosh(1/x.to_f)
  end

  def cot x
    1/tan(x)
  end
  def coth x
    1/tanh(x)
  end
  def acot x
    atan(1/x.to_f)
  end
  def acoth x
    atanh(1/x.to_f)
  end

# Statistics
  def covariance(x,y,*ranks)
    x = NArray.to_na(x) unless x.kind_of?(NArray)
    x = x.to_type(NArray::DFLOAT) if x.integer?
    y = NArray.to_na(y) unless y.kind_of?(NArray)
    y = y.to_type(NArray::DFLOAT) if y.integer?
    n = x.rank_total(*ranks)
    xm = x.accum(*ranks).div!(n)
    ym = y.accum(*ranks).div!(n)
    ((x-xm)*(y-ym)).sum(*ranks) / (n-1)
  end

  module_function :recip
  module_function :csc,:sec,:cot,:csch,:sech,:coth
  module_function :acsc,:asec,:acot,:acsch,:asech,:acoth
  module_function :covariance
end


module FFTW
  def convol(a1,a2)
    n1x,n1y = a1.shape
    n2x,n2y = a2.shape
    raise "arrays must have same shape" if n1x!=n2x || n1y!=n2y
    (FFTW.fftw( FFTW.fftw(a1,-1) * FFTW.fftw(a2,-1), 1).real) / (n1x*n1y)
  end
  module_function :convol
end

require 'nmatrix'
