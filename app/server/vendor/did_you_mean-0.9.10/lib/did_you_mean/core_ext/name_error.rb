class NameError
  attr_reader :frame_binding

  IGNORED_CALLERS = [
    /( |`)missing_name'/,
    /( |`)safe_constantize'/
  ].freeze
  private_constant :IGNORED_CALLERS

  def to_s_with_did_you_mean
    msg = original_message.dup
    bt  = caller.first(6)

    msg << did_you_mean?.to_s if IGNORED_CALLERS.all? {|ignored| bt.grep(ignored).empty? }
    msg
  rescue
    original_message
  end

  alias original_message to_s
  alias             to_s to_s_with_did_you_mean

  def did_you_mean?
    finder.did_you_mean?
  end

  def finder
    @finder ||= DidYouMean.finders[self.class.to_s].new(self)
  end
end
