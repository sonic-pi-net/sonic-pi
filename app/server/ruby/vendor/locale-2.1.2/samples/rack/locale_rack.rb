 require 'locale'
 Locale.init(:driver => :cgi)
 
 module Locale::Rack 
    def init_locale(env, req)
      Locale.set_request([req["lang"]], [req.cookies["lang"]],
                         env["HTTP_ACCEPT_LANGUAGE"], 
                         env["HTTP_ACCEPT_CHARSET"])     
    end
 end

