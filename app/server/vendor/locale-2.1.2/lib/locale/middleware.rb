# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
#
# License: Ruby's or LGPL
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "locale"

module Locale
  class Middleware
    def initialize(application, options={})
      @application = application
      @options = options
      Locale.init(:driver => :cgi)
    end

    def call(environment)
      request = Rack::Request.new(environment)
      Locale.set_request([request["lang"]],
                         [request.cookies["lang"]],
                         environment["HTTP_ACCEPT_LANGUAGE"],
                         environment["HTTP_ACCEPT_CHARSET"])
      @application.call(environment)
    end
  end
end

