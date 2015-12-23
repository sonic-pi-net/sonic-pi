#  Numerical Array Extention for Ruby
#    (C) Copyright 2000-2003 by Masahiro TANAKA
#

#
# ------ NMatrix ------
#
class NMatrix < NArray
  CLASS_DIMENSION = 2

  def +(other)
    case other
    when NMatrix
      return super(NArray.refer(other))
    when NArray
      unless other.instance_of?(NArray)
        return other.coerce_rev( self, :+ )
      end
    end
    raise TypeError,"Illegal operation: NMatrix + %s" % other.class
  end

  def -(other)
    case other
    when NMatrix
      return super(NArray.refer(other))
    when NArray
      unless other.instance_of?(NArray)
        return other.coerce_rev( self, :- )
      end
    end
    raise TypeError,"Illegal operation: NMatrix - %s" % other.class
  end

  def *(other)
    case other
    when NMatrix
      NMatrix.mul_add( NArray.refer(self).newdim!(0),other.newdim(2), 1 )
      #NMatrix.mul_add( NArray.refer(self).newdim!(0),
      #		       other.transpose(1,0).newdim!(2), 0 )
    when NVector
      NVector.mul_add( NArray.refer(self), other.newdim(1), 0 )
    when NArray
      if other.instance_of?(NArray)
	NMatrix.mul( NArray.refer(self), other.newdim(0,0) )
      else
	other.coerce_rev( self, :* )
      end
    when Numeric
      super
      #NMatrix.mul( NArray.refer(self), other )
    when Array
      NMatrix.mul( self, NArray[*other].newdim!(0,0) )
    else
      raise TypeError,"Illegal operation: NMatrix * %s" % other.class
    end
  end

  def /(other)
    case other
    when NMatrix
      other.lu.solve(self)
    when NVector
      raise TypeError,"Illegal operation: NMatrix / %s" % other.class
    when NArray
      if other.instance_of?(NArray)
	NMatrix.div( NArray.refer(self), other.newdim(0,0) )
      else
	other.coerce_rev( self, :/ )
      end
    when Numeric
      NMatrix.div( NArray.refer(self), other )
    when Array
      NMatrix.div( self, NArray[*other].newdim!(0,0) )
    else
      raise TypeError,"Illegal operation: NMatrix / %s" % other.class
    end
  end

  def **(n)
    case n
    when Integer
      if n==0
	return 1.0
      elsif n<0
	m = self.inverse
	n = -n
      else
	m = self
      end
      (2..n).each{ m *= self }
      m
    else
      raise TypeError,"Illegal operation: NMatrix ** %s" % n.class
    end
  end

  def coerce_rev(other,id)
    case id
    when :*
	if other.instance_of?(NArray)
	  return NMatrix.mul( other.newdim(0,0), self )
	end
	if other.instance_of?(NArrayScalar)
	  return NMatrix.mul( other.newdim(0), self )
	end
    when :/
	if other.instance_of?(NArray)
	  return NMatrix.mul( other.newdim(0,0), self.inverse )
	end
	if other.instance_of?(NArrayScalar)
	  return NMatrix.mul( other.newdim(0), self.inverse )
	end
    end
    raise TypeError,"Illegal operation: %s %s NMatrix" %
      [other.class, id.id2name]
  end

  def inverse
    self.lu.solve( NMatrix.new(self.typecode, *self.shape).fill!(0).unit )
  end

  def transpose(*arg)
    if arg.size==0
      super(1,0)
    else
      super
    end
  end

  def diagonal!(val=1)
    shp = self.shape
    idx = NArray.int(shp[0..1].min).indgen! * (shp[0]+1)
    ref = reshape(shp[0]*shp[1],true)
    if val.kind_of?(Numeric)
      ref[idx,true] = val
    else
      val = NArray.to_na(val)
      raise ArgumentError, "must be 1-d array" if val.dim!=1
      ref[idx,true] = val.newdim!(-1)
    end
    self
  end

  def diagonal(val)
    self.dup.diagonal!(val)
  end

  def unit
    diagonal!
  end
  alias identity unit
  alias I unit

end # class NMatrix


#
# ------ NVector ------
#
class NVector < NArray
  CLASS_DIMENSION = 1

  def +(other)
    case other
    when NVector
      return super(NArray.refer(other))
    when NArray
      unless other.instance_of?(NArray)
        return other.coerce_rev( self, :+ )
      end
    end
    raise TypeError,"Illegal operation: NVector + %s" % other.class
  end

  def -(other)
    case other
    when NVector
      return super(NArray.refer(other))
    when NArray
      unless other.instance_of?(NArray)
        return other.coerce_rev( self, :- )
      end
    end
    raise TypeError,"Illegal operation: NVector - %s" % other.class
  end

  def *(other)
    case other
    when NMatrix
      NVector.mul_add( NArray.refer(self).newdim!(0), other, 1 )
    when NVector
      NArray.mul_add( NArray.refer(self), other, 0 ) # inner product
    when NArray
      if other.instance_of?(NArray)
	NVector.mul( NArray.refer(self), other.newdim(0) )
      else
	other.coerce_rev( self, :* )
      end
    when Numeric
      NVector.mul( NArray.refer(self), other )
    else
      raise TypeError,"Illegal operation: NVector * %s" % other.class
    end
  end

  def /(other)
    case other
    when NMatrix
      other.lu.solve(self)
    when NVector
      raise TypeError,"Illegal operation: NVector / %s" % other.class
    when NArray
      if other.instance_of?(NArray)
	NVector.div( NArray.refer(self), other.newdim(0) )
      else
	other.coerce_rev( self, :/ )
      end
    when Numeric
      NVector.div( NArray.refer(self), other )
    else
      raise TypeError,"Illegal operation: NVector / %s" % other.class
    end
  end

  def **(n)
    if n==2
      self*self
    else
      raise ArgumentError,"Only v**2 is implemented"
    end
  end

  def coerce_rev(other,id)
    case id
    when :*
	if other.instance_of?(NArray)
	  return NVector.mul( other.newdim(0), self )
	end
	if other.instance_of?(NArrayScalar)
	  return NVector.mul( other, self )
	end
    end
    raise TypeError,"Illegal operation: %s %s NVector" %
      [other.class, id.id2name]
  end

end # class NVector
