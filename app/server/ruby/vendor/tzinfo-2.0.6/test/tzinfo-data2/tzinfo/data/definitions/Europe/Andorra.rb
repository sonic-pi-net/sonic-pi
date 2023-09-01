# encoding: UTF-8
# frozen_string_literal: true

# This file contains data derived from the IANA Time Zone Database
# (https://www.iana.org/time-zones).

module TZInfo
  module Data
    module Definitions
      module Europe
        module Andorra
          include Format2::TimezoneDefinition

          timezone 'Europe/Andorra' do |tz|
            tz.offset :o0, 364, 0, 'LMT'
            tz.offset :o1, 0, 0, 'WET'
            tz.offset :o2, 3600, 0, 'CET'
            tz.offset :o3, 3600, 3600, 'CEST'

            tz.transition :o1, -2177453164
            tz.transition :o2, -733881600
            tz.transition :o3, 481078800
            tz.transition :o2, 496803600
            tz.transition :o3, 512528400
            tz.transition :o2, 528253200
            tz.transition :o3, 543978000
            tz.transition :o2, 559702800
            tz.transition :o3, 575427600
            tz.transition :o2, 591152400
            tz.transition :o3, 606877200
            tz.transition :o2, 622602000
            tz.transition :o3, 638326800
            tz.transition :o2, 654656400
            tz.transition :o3, 670381200
            tz.transition :o2, 686106000
            tz.transition :o3, 701830800
            tz.transition :o2, 717555600
            tz.transition :o3, 733280400
            tz.transition :o2, 749005200
            tz.transition :o3, 764730000
            tz.transition :o2, 780454800
            tz.transition :o3, 796179600
            tz.transition :o2, 811904400
            tz.transition :o3, 828234000
            tz.transition :o2, 846378000
            tz.transition :o3, 859683600
            tz.transition :o2, 877827600
            tz.transition :o3, 891133200
            tz.transition :o2, 909277200
            tz.transition :o3, 922582800
            tz.transition :o2, 941331600
            tz.transition :o3, 954032400
            tz.transition :o2, 972781200
            tz.transition :o3, 985482000
            tz.transition :o2, 1004230800
            tz.transition :o3, 1017536400
            tz.transition :o2, 1035680400
            tz.transition :o3, 1048986000
            tz.transition :o2, 1067130000
            tz.transition :o3, 1080435600
            tz.transition :o2, 1099184400
            tz.transition :o3, 1111885200
            tz.transition :o2, 1130634000
            tz.transition :o3, 1143334800
            tz.transition :o2, 1162083600
            tz.transition :o3, 1174784400
            tz.transition :o2, 1193533200
            tz.transition :o3, 1206838800
            tz.transition :o2, 1224982800
            tz.transition :o3, 1238288400
            tz.transition :o2, 1256432400
            tz.transition :o3, 1269738000
            tz.transition :o2, 1288486800
            tz.transition :o3, 1301187600
            tz.transition :o2, 1319936400
            tz.transition :o3, 1332637200
            tz.transition :o2, 1351386000
            tz.transition :o3, 1364691600
            tz.transition :o2, 1382835600
            tz.transition :o3, 1396141200
            tz.transition :o2, 1414285200
            tz.transition :o3, 1427590800
            tz.transition :o2, 1445734800
            tz.transition :o3, 1459040400
            tz.transition :o2, 1477789200
            tz.transition :o3, 1490490000
            tz.transition :o2, 1509238800
            tz.transition :o3, 1521939600
            tz.transition :o2, 1540688400
            tz.transition :o3, 1553994000
            tz.transition :o2, 1572138000
            tz.transition :o3, 1585443600
            tz.transition :o2, 1603587600
            tz.transition :o3, 1616893200
            tz.transition :o2, 1635642000
            tz.transition :o3, 1648342800
            tz.transition :o2, 1667091600
            tz.transition :o3, 1679792400
            tz.transition :o2, 1698541200
            tz.transition :o3, 1711846800
            tz.transition :o2, 1729990800
            tz.transition :o3, 1743296400
            tz.transition :o2, 1761440400
            tz.transition :o3, 1774746000
            tz.transition :o2, 1792890000
            tz.transition :o3, 1806195600
            tz.transition :o2, 1824944400
            tz.transition :o3, 1837645200
            tz.transition :o2, 1856394000
            tz.transition :o3, 1869094800
            tz.transition :o2, 1887843600
            tz.transition :o3, 1901149200
            tz.transition :o2, 1919293200
            tz.transition :o3, 1932598800
            tz.transition :o2, 1950742800
            tz.transition :o3, 1964048400
            tz.transition :o2, 1982797200
            tz.transition :o3, 1995498000
            tz.transition :o2, 2014246800
            tz.transition :o3, 2026947600
            tz.transition :o2, 2045696400
            tz.transition :o3, 2058397200
            tz.transition :o2, 2077146000
            tz.transition :o3, 2090451600
            tz.transition :o2, 2108595600
            tz.transition :o3, 2121901200
            tz.transition :o2, 2140045200
            tz.transition :o3, 2153350800
            tz.transition :o2, 2172099600
            tz.transition :o3, 2184800400
            tz.transition :o2, 2203549200
            tz.transition :o3, 2216250000
            tz.transition :o2, 2234998800
            tz.transition :o3, 2248304400
            tz.transition :o2, 2266448400
            tz.transition :o3, 2279754000
            tz.transition :o2, 2297898000
            tz.transition :o3, 2311203600
            tz.transition :o2, 2329347600
            tz.transition :o3, 2342653200
            tz.transition :o2, 2361402000
            tz.transition :o3, 2374102800
            tz.transition :o2, 2392851600
            tz.transition :o3, 2405552400
            tz.transition :o2, 2424301200
            tz.transition :o3, 2437606800
            tz.transition :o2, 2455750800
            tz.transition :o3, 2469056400
            tz.transition :o2, 2487200400
            tz.transition :o3, 2500506000
            tz.transition :o2, 2519254800
            tz.transition :o3, 2531955600
            tz.transition :o2, 2550704400
            tz.transition :o3, 2563405200
            tz.transition :o2, 2582154000
            tz.transition :o3, 2595459600
            tz.transition :o2, 2613603600
            tz.transition :o3, 2626909200
            tz.transition :o2, 2645053200
            tz.transition :o3, 2658358800
            tz.transition :o2, 2676502800
            tz.transition :o3, 2689808400
            tz.transition :o2, 2708557200
            tz.transition :o3, 2721258000
            tz.transition :o2, 2740006800
            tz.transition :o3, 2752707600
            tz.transition :o2, 2771456400
            tz.transition :o3, 2784762000
            tz.transition :o2, 2802906000
            tz.transition :o3, 2816211600
            tz.transition :o2, 2834355600
            tz.transition :o3, 2847661200
            tz.transition :o2, 2866410000
            tz.transition :o3, 2879110800
            tz.transition :o2, 2897859600
            tz.transition :o3, 2910560400
            tz.transition :o2, 2929309200
            tz.transition :o3, 2942010000
            tz.transition :o2, 2960758800
            tz.transition :o3, 2974064400
            tz.transition :o2, 2992208400
            tz.transition :o3, 3005514000
            tz.transition :o2, 3023658000
            tz.transition :o3, 3036963600
            tz.transition :o2, 3055712400
            tz.transition :o3, 3068413200
            tz.transition :o2, 3087162000
            tz.transition :o3, 3099862800
            tz.transition :o2, 3118611600
            tz.transition :o3, 3131917200
            tz.transition :o2, 3150061200
            tz.transition :o3, 3163366800
            tz.transition :o2, 3181510800
          end
        end
      end
    end
  end
end
