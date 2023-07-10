# encoding: UTF-8
# frozen_string_literal: true

# This file contains data derived from the IANA Time Zone Database
# (https://www.iana.org/time-zones).

module TZInfo
  module Data
    module Definitions
      module America
        module Argentina
          module Buenos_Aires
            include Format2::TimezoneDefinition

            timezone 'America/Argentina/Buenos_Aires' do |tz|
              tz.offset :o0, -14028, 0, 'LMT'
              tz.offset :o1, -15408, 0, 'CMT'
              tz.offset :o2, -14400, 0, '-04'
              tz.offset :o3, -14400, 3600, '-03'
              tz.offset :o4, -10800, 0, '-03'
              tz.offset :o5, -10800, 3600, '-02'

              tz.transition :o1, -2372097972
              tz.transition :o2, -1567453392
              tz.transition :o3, -1233432000
              tz.transition :o2, -1222981200
              tz.transition :o3, -1205956800
              tz.transition :o2, -1194037200
              tz.transition :o3, -1172865600
              tz.transition :o2, -1162501200
              tz.transition :o3, -1141329600
              tz.transition :o2, -1130965200
              tz.transition :o3, -1109793600
              tz.transition :o2, -1099429200
              tz.transition :o3, -1078257600
              tz.transition :o2, -1067806800
              tz.transition :o3, -1046635200
              tz.transition :o2, -1036270800
              tz.transition :o3, -1015099200
              tz.transition :o2, -1004734800
              tz.transition :o3, -983563200
              tz.transition :o2, -973198800
              tz.transition :o3, -952027200
              tz.transition :o2, -941576400
              tz.transition :o3, -931032000
              tz.transition :o2, -900882000
              tz.transition :o3, -890337600
              tz.transition :o2, -833749200
              tz.transition :o3, -827265600
              tz.transition :o2, -752274000
              tz.transition :o3, -733780800
              tz.transition :o2, -197326800
              tz.transition :o3, -190843200
              tz.transition :o2, -184194000
              tz.transition :o3, -164491200
              tz.transition :o2, -152658000
              tz.transition :o3, -132955200
              tz.transition :o2, -121122000
              tz.transition :o3, -101419200
              tz.transition :o2, -86821200
              tz.transition :o3, -71092800
              tz.transition :o2, -54766800
              tz.transition :o3, -39038400
              tz.transition :o2, -23317200
              tz.transition :o4, -7588800
              tz.transition :o5, 128142000
              tz.transition :o4, 136605600
              tz.transition :o5, 596948400
              tz.transition :o4, 605066400
              tz.transition :o5, 624423600
              tz.transition :o4, 636516000
              tz.transition :o5, 656478000
              tz.transition :o4, 667965600
              tz.transition :o5, 687927600
              tz.transition :o4, 699415200
              tz.transition :o5, 719377200
              tz.transition :o4, 731469600
              tz.transition :o3, 938919600
              tz.transition :o4, 952052400
              tz.transition :o5, 1198983600
              tz.transition :o4, 1205632800
              tz.transition :o5, 1224385200
              tz.transition :o4, 1237082400
            end
          end
        end
      end
    end
  end
end
