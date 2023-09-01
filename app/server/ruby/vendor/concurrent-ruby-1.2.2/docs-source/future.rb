require 'concurrent'
require 'csv'
require 'open-uri'

class Ticker
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
    year_end = data.first
    year_end
  rescue => e
    p e
  end
end

api_key = ENV['ALPHAVANTAGE_KEY']
abort(error_message) unless api_key

# Future
price = Concurrent::Future.execute{ Ticker.new.get_year_end_closing('TWTR', 2013, api_key) }
p price.state #=> :pending
p price.pending? #=> true
p price.value(0) #=> nil (does not block)

sleep(1)    # do other stuff

p price.value #=> 63.65 (after blocking if necessary)
p price.state #=> :fulfilled
p price.fulfilled? #=> true
p price.value #=> 63.65
