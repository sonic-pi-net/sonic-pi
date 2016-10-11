class UntestedClass
  def initialize(yogurts)
    @yogurts = yogurts
  end

  def power_level
    @yogurts.map do |yo|
      yo.experience_points**2
    end.reduce(0, &:+)
  end
end
