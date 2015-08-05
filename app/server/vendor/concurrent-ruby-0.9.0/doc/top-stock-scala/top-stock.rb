require 'concurrent'
require 'open-uri'

def get_year_end_closing(symbol, year)
  uri = "http://ichart.finance.yahoo.com/table.csv?s=#{symbol}&a=11&b=01&c=#{year}&d=11&e=31&f=#{year}&g=m"
  data = open(uri) {|f| f.collect{|line| line.strip } }
  price = data[1].split(',')[4]
  price.to_f
  [symbol, price.to_f]
end

def get_top_stock(symbols, year, timeout = 5)
  stock_prices = symbols.collect{|symbol| Concurrent::dataflow{ get_year_end_closing(symbol, year) }}
  Concurrent::dataflow(*stock_prices) { |*prices|
    prices.reduce(['', 0.0]){|highest, price| price.last > highest.last ? price : highest}
  }.value(timeout)
end

symbols = ['AAPL', 'GOOG', 'IBM', 'ORCL', 'MSFT']
year = 2008

top_stock, highest_price = get_top_stock(symbols, year)

puts "Top stock of #{year} is #{top_stock} closing at price $#{highest_price}"
