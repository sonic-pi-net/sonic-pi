module SimpleCov
  version = "0.12.0"

  def version.to_a
    split(".").map(&:to_i)
  end

  def version.major
    to_a[0]
  end

  def version.minor
    to_a[1]
  end

  def version.patch
    to_a[2]
  end

  def version.pre
    to_a[3]
  end

  VERSION = version
end
