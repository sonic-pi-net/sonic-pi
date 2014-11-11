class NameError
  attr_reader :frame_binding

  begin
    require "active_support/core_ext/name_error"

    def missing_name
      if /undefined local variable or method/ !~ original_message
        $1 if /((::)?([A-Z]\w*)(::[A-Z]\w*)*)$/ =~ original_message
      end
    end if method_defined?(:missing_name)
  rescue LoadError; end

  def to_s_with_did_you_mean
    original_message + did_you_mean?.to_s rescue original_message
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
