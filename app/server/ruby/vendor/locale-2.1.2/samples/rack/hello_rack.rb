require 'rubygems'
require 'rack'
require 'locale_rack'
 
class HelloRackApp
  include Locale::Rack

  def call(env)
    req = Rack::Request.new(env)
    init_locale(env, req)
    str = "Language tag candidates of your request order by the priority:\n\n"
    str += Locale.candidates(:type => :rfc).map{|v| v.inspect + "\n"}.join
    [200, {"Content-Type" => "text/plain", "Content-Length" => str.length.to_s}, [str]]
  end
end

