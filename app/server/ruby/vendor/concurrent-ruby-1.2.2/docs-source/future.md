`Future` is inspired by [Clojure's](http://clojure.org/) [future](http://clojuredocs.org/clojure_core/clojure.core/future) function. A future represents a promise to complete an action at some time in the future. The action is atomic and permanent. The idea behind a future is to send an operation for asynchronous completion, do other stuff, then return and retrieve the result of the async operation at a later time. `Future`s run on the global thread pool.

```cucumber
Feature:
  As a highly responsive Ruby application
  I want long-running tasks on a separate thread
  So I can perform other tasks without waiting
```

`Future`s have several possible states: *:unscheduled*, *:pending*, *:processing*, *:rejected*, or *:fulfilled*. These are also aggregated as `#incomplete?` and `#complete?`. When a `Future` is created it is set to *:unscheduled*. Once the `#execute` method is called the state becomes *:pending*. Once a job is pulled from the thread pool's queue and is given to a thread for processing (often immediately upon `#post`) the state becomes *:processing*. The future will remain in this state until processing is complete. A future that is in the *:unscheduled*, *:pending*, or *:processing* is considered `#incomplete?`. A `#complete?` `Future` is either *:rejected*, indicating that an exception was thrown during processing, or *:fulfilled*, indicating success. If a `Future` is *:fulfilled* its `#value` will be updated to reflect the result of the operation. If *:rejected* the `reason` will be updated with a reference to the thrown exception. The predicate methods `#unscheduled?`, `#pending?`, `#rejected?`, and `#fulfilled?` can be called at any time to obtain the state of the `Future`, as can the `#state` method, which returns a symbol.

Retrieving the value of a `Future` is done through the `#value` (alias: `#deref`) method. Obtaining the value of a `Future` is a potentially blocking operation. When a `Future` is *:rejected* a call to `#value` will return `nil` immediately. When a `Future` is *:fulfilled* a call to `#value` will immediately return the current value. When a `Future` is *:pending* a call to `#value` will block until the `Future` is either *:rejected* or *:fulfilled*. A *timeout* value can be passed to `#value` to limit how long the call will block. If `nil` the call will block indefinitely. If `0` the call will not block. Any other integer or float value will indicate the maximum number of seconds to block.

The constructor can also be given zero or more processing options. Currently the only supported options are those recognized by the [Dereferenceable](Dereferenceable) module.

The `Future` class also includes the behavior of the Ruby standard library [Observable](http://ruby-doc.org/stdlib-2.0/libdoc/observer/rdoc/Observable.html) module, but does so in a thread-safe way. On fulfillment or rejection all observers will be notified according to the normal `Observable` behavior. The observer callback function will be called with three parameters: the `Time` of fulfillment/rejection, the final `value`, and the final `reason`. Observers added after fulfillment/rejection will still be notified as normal. The notification will occur on the same thread that processed the job.

### Examples

A fulfilled example:

```ruby
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
```



A rejected example:

```ruby
count = Concurrent::Future.execute{ sleep(10); raise StandardError.new("Boom!") }
count.state #=> :pending
count.pending? #=> true

count.value #=> nil (after blocking)
count.rejected? #=> true
count.reason #=> #<StandardError: Boom!>
```





An example with observation:

```ruby
class Ticker
  Stock = Struct.new(:symbol, :name, :exchange)

  def update(time, value, reason)
    ticker = value.collect do |symbol|
      Stock.new(symbol['symbol'], symbol['name'], symbol['exch'])
    end

    output = ticker.join("\n")
    print "#{output}\n"
  end
end

yahoo = Ticker.new('YAHOO')
future = Concurrent::Future.new { yahoo.update.suggested_symbols }
future.add_observer(Ticker.new)
future.execute

# do important stuff...

#>> #<struct Ticker::Stock symbol="YHOO", name="Yahoo! Inc.", exchange="NMS">
#>> #<struct Ticker::Stock symbol="YHO.DE", name="Yahoo! Inc.", exchange="GER">
#>> #<struct Ticker::Stock symbol="YAHOY", name="Yahoo Japan Corporation", exchange="PNK">
#>> #<struct Ticker::Stock symbol="YAHOF", name="YAHOO JAPAN CORP", exchange="PNK">
#>> #<struct Ticker::Stock symbol="YOJ.SG", name="YAHOO JAPAN", exchange="STU">
#>> #<struct Ticker::Stock symbol="YHO.SG", name="YAHOO", exchange="STU">
#>> #<struct Ticker::Stock symbol="YHOO.BA", name="Yahoo! Inc.", exchange="BUE">
#>> #<struct Ticker::Stock symbol="YHO.DU", name="YAHOO", exchange="DUS">
#>> #<struct Ticker::Stock symbol="YHO.HM", name="YAHOO", exchange="HAM">
#>> #<struct Ticker::Stock symbol="YHO.BE", name="YAHOO", exchange="BER">
```
