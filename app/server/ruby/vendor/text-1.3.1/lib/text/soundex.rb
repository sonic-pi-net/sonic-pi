#
# Ruby implementation of the Soundex algorithm,
# as described by Knuth in volume 3 of The Art of Computer Programming.
#
# Author: Michael Neumann (neumann@s-direktnet.de)
#

module Text # :nodoc:
module Soundex

  def soundex(str_or_arr)
    case str_or_arr
    when String
      soundex_str(str_or_arr)
    when Array
      str_or_arr.collect{|ele| soundex_str(ele)}
    else
      nil
    end
  end
  module_function :soundex

  private

  #
  # returns nil if the value couldn't be calculated (empty-string, wrong-character)
  # do not change the parameter "str"
  #
  def soundex_str(str)
    str = str.upcase.gsub(/[^A-Z]/, "")
    return nil if str.empty?

    last_code = get_code(str[0,1])
    soundex_code = str[0,1]

    for index in 1...(str.size) do
      return soundex_code if soundex_code.size == 4

      code = get_code(str[index,1])

      if code == "0" then
        last_code = nil
      elsif code != last_code then
        soundex_code += code
        last_code = code
      end
    end # for

    return soundex_code.ljust(4, "0")
  end
  module_function :soundex_str

  def get_code(char)
    char.tr! "AEIOUYWHBPFVCSKGJQXZDTLMNR", "00000000111122222222334556"
  end
  module_function :get_code

end # module Soundex
end # module Text
