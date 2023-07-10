require 'concurrent'
require 'csv'
require 'open-uri'

def get_year_end_closing(symbol, year, api_key)
  uri = "https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY&symbol=#{symbol}&apikey=#{api_key}&datatype=csv"
  data = []
  csv = URI.parse(uri).read
  if csv.include?('call frequency')
    return :rate_limit_exceeded
  end
  CSV.parse(csv, headers: true) do |row|
    data << row['close'].to_f if row['timestamp'].include?(year.to_s)
  end
  price = data.max
  [symbol, price]
end

def get_top_stock(symbols, year, timeout = 10)
  api_key = ENV['ALPHAVANTAGE_KEY']
  abort(error_message) unless api_key

  stock_prices = symbols.collect{|symbol| Concurrent::dataflow{ get_year_end_closing(symbol, year, api_key) }}
  Concurrent::dataflow(*stock_prices) { |*prices|
    next :rate_limit_exceeded if prices.include?(:rate_limit_exceeded)
    prices.reduce(['', 0.0]){|highest, price| price.last > highest.last ? price : highest}
  }.value(timeout)
end

def error_message
  <<~EOF
    PLEASE provide a Alpha Vantage api key for the example to work
    usage:
      ALPHAVANTAGE_KEY=YOUR_API_KEY bundle exec ruby top-stock-scala/top-stock.rb
  EOF
end

symbols = ['AAPL', 'GOOG', 'IBM', 'ORCL', 'MSFT']
year = 2018

result = get_top_stock(symbols, year)

if result == :rate_limit_exceeded
  puts "API rate limit exceeded"
else
  top_stock, highest_price = result
  puts "Top stock of #{year} is #{top_stock} closing at price $#{highest_price}"
end
