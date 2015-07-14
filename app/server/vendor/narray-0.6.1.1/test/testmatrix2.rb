require 'narray'
#require 'irb/xmp'
# xmp :: http://www.ruby-lang.org/en/raa-list.rhtml?name=xmp
def xp(s)
  begin
    puts s+" #=>"
    p eval(s)
  rescue
    puts $!
  end
  puts
end

$m1 = NMatrix.float(2,2).indgen!
$m2 = NMatrix[[0,1.2],[1.5,0]]

$v1 = NVector[0.5,1.5]
$v2 = NVector.float(2,2).indgen!

$a  = NArray.float(2,2).indgen!

xp '$m1'
xp '$m1.inverse'
xp '$m2'
xp '$m1*$m2'
xp '$m2*$m1'
xp '$m1+$m2'
xp '3.14*$m1'
xp '$m2*1.25'
xp '$v1'
xp '$v2'
xp '1.25*$v1'
xp 'NMath.sqrt($v2**2)'
xp '$v1*$v2'
xp '$m1*$v1'
xp '$v2*$m2'
xp '$m1.diagonal([98,99])'
xp 'NMatrix.float(4,3).unit'

puts "\n=== following will fail ...\n"
xp '$m1+$v1'
xp '$m1+1'
