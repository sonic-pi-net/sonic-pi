##
# @api public
#
# This monkeypatch extends Ruby 1.8 {String#%} with an ability
# to replace named capture groups, i.e.
# `"foo: %{bar}" % { :bar => 10 } # => "foo: 10"`.
#
class String
  alias original_percent %

  def %(arg, *args)
    if arg.is_a?(Hash)
      gsub(/%\{(\w+)\}/) do
        arg[$1.to_sym]
      end
    else
      original_percent(arg, *args)
    end
  end
end
