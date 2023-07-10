require 'concurrent-edge'                # => true

# This little bit more complicated commented example aims to
# demonstrate some of the capabilities of concurrent-ruby new abstractions.

# It is a concurrent processing pipeline which on one side has several web crawlers.
# They are searching the web for data and filling buffer.
# On the other side there are data processors which are pop the data from buffer.
# They are processing the data and storing results into a DB
# which has limited concurrency level.
# Some of the parts like Web and DB are just stubs.
# Each part logs and increments counters to keep some stats about the pipeline.
# There is also a periodical readout of the stats into log scheduled.

# Schema of the pipeline:

# web-crawlers -> buffer -> data-processing -> DB
#            \____________________________\_____\___> logging

# TODO (pitr-ch 10-Mar-2019): replace with a better more realistic example using
# * actors for limited concurrency with state - local DB connection
# * throttled futures for REST API - limiting server load

# The central logger is defined first.
# It has state like the logger instance, therefore the actor is used.
# It is better to exactly define the communication protocol of the logging actor.
# It will only understand these messages.
Log      = Concurrent::ImmutableStruct.new :severity, :message
# => Log
SetLevel = Concurrent::ImmutableStruct.new :level
# => SetLevel

require 'logger'                         # => false
require 'stringio'                       # => false

# Including actor constants so this scope understands ANY etc.
include Concurrent::ErlangActor::EnvironmentConstants
# => Object
# The logger does not need a dedicated thread, let's use a pool.
LOGGING = Concurrent::ErlangActor.spawn Logger::FATAL,
                                        type: :on_pool,
                                        name: 'logger' do |level|
  # a Logger instance with nicer formatting is created
  @logger           = Logger.new($captured_out)
  @logger.level     = level
  @logger.formatter = lambda do |severity, datetime, progname, msg|
    formatted_message = case msg
                        when String
                          msg
                        when Exception
                          format "%s (%s)\n%s",
                                 msg.message, msg.class, (msg.backtrace || []).join("\n")
                        else
                          msg.inspect
                        end
    format "[%s] %5s -- %s: %s\n",
           datetime.strftime('%Y-%m-%d %H:%M:%S.%L'),
           severity,
           progname,
           formatted_message
  end

  # definition of the logging actor behaviour
  receive(
      # log messages
      on(Log) { |message| @logger.log message.severity, message.message },
      # change level
      on(SetLevel) { |message| @logger.level = message.level },
      # It is a good practice to read and log bad messages,
      # otherwise they would accumulate in the inbox.
      on(ANY) { |message| @logger.error bad_message: message },
      # The logger has static behaviour, therefore keep can be used, and the actor
      # will behave the same with each message received as defined below.
      keep: true)
end
# => #<Concurrent::ErlangActor::Pid:0x000002 logger running>

# testing the logger works as expected
LOGGING.tell Log[Logger::FATAL, :tornado]
# => #<Concurrent::ErlangActor::Pid:0x000002 logger running>
LOGGING.tell Log[Logger::INFO, :wind]
# => #<Concurrent::ErlangActor::Pid:0x000002 logger running>
LOGGING.tell SetLevel[Logger::DEBUG]
# => #<Concurrent::ErlangActor::Pid:0x000002 logger running>
LOGGING.tell Log[Logger::INFO, :breeze]
# => #<Concurrent::ErlangActor::Pid:0x000002 logger running>

sleep 0.05 # the logging is asynchronous, we need to wait a bit until it's written
get_captured_output
# => "[2020-01-26 16:21:45.815] FATAL -- : :tornado\n" +
#    "[2020-01-26 16:21:45.816]  INFO -- : :breeze\n"

# the logging could be wrapped in a method
def log(severity, message)
  LOGGING.tell Log[severity, message]
  true
end                                      # => :log

include Logger::Severity                 # => Object
log INFO, 'alive'                        # => true
sleep 0.05                               # => 0
get_captured_output
# => "[2020-01-26 16:21:45.866]  INFO -- : alive\n"


# The stub which will represent the web
module Web
  @counter = Concurrent::AtomicFixnum.new

  def self.search
    sleep 0.01
    @counter.increment.to_s(16)
  end
end 

# The cancellation which will be used to cancel the whole processing pipeline.
@cancellation, origin = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x000003 pending>

# Buffer for work
buffer_capacity   = 10                   # => 10
@buffer           = Concurrent::Promises::Channel.new buffer_capacity
# => #<Concurrent::Promises::Channel:0x000004 capacity taken 0 of 10>
web_crawler_count = 4                    # => 4

# Track the number of data provided by each crawler
crawler_data_counter = Array.new(web_crawler_count) do |i|
  # this is accessed by multiple threads so it should be a tread-safe counter
  Concurrent::AtomicFixnum.new
end 
# the array is frozen which makes it immutable,
# therefore safe to use when concurrently accessed.
# Otherwise if it was being modified it wound has to be Concurrent::Array to make it safe.
crawler_data_counter.freeze
# => [#<Concurrent::AtomicFixnum:0x000005 value:0>,
#     #<Concurrent::AtomicFixnum:0x000006 value:0>,
#     #<Concurrent::AtomicFixnum:0x000007 value:0>,
#     #<Concurrent::AtomicFixnum:0x000008 value:0>]

# The web crawlers are defined directly with threads to start the example simply.
# They search the web and immediately as they find something they push
# the data into the buffer.
# The push will block if the buffer is full,
# regulating how fast is the work being found.
# This is called backpressure.
crawlers = Array.new web_crawler_count do |i|
  Thread.new do
    while true
      # crawl the web until cancelled
      break if @cancellation.canceled?
      # will block and slow down the crawler if the buffer is full
      data = Web.search
      until @buffer.push data, 0.1
        # It is a good practice to use timeouts on all blocking operations
        # If the pipeline is cancelled and the data-processors finish
        # before taking data from buffer a crawler could get stack on this push.
        break if @cancellation.canceled?
      end
      # it pushed data, increment its counter
      crawler_data_counter[i].increment
      log DEBUG, "crawler #{i} found #{data}"
    end
  end
end.freeze
# => [#<Thread:0x000009@medium-example.in.rb:130 run>,
#     #<Thread:0x00000a@medium-example.in.rb:130 run>,
#     #<Thread:0x00000b@medium-example.in.rb:130 run>,
#     #<Thread:0x00000c@medium-example.in.rb:130 run>]

# So far only the crawlers looking for data are defined
# pushing data into the buffer.
# The data processing definition follows.
# Threads are not used again directly but rather the data processing
# is defined using Futures.
# Even though that makes the definition more complicated
# it has a big advantage that data processors will not require a Thread each
# but they will share and run on a Thread pool.
# That removes an important limitation of the total number of threads process can have,
# which can be an issue in larger systems.
# This example would be fine with using the Threads
# however it would not demonstrate the more advanced usage then.

# The data processing stores results in a DB,
# therefore the stub definition of a database precedes the data processing.
module DB
  @data = Concurrent::Map.new

  # increment a counter for char
  def self.add(char, count)
    @data.compute char do |old|
      (old || 0) + count
    end
    true
  end

  # return the stored data as Hash
  def self.data
    @data.each_pair.reduce({}) { |h, (k, v)| h.update k => v }
  end
end                                      # => :data

# Lets assume that instead having this DB
# we have limited number of connections
# and therefore there is a limit on
# how many threads can communicate with the DB at the same time.
# The throttle is created to limit the number of concurrent access to DB.
@db_throttle = Concurrent::Throttle.new 4
# => #<Concurrent::Throttle:0x00000d capacity available 4 of 4>

# The data processing definition follows
data_processing_count = 20 # this could actually be thousands if required

# track the number of data received by data processors
@data_processing_counters = Array.new data_processing_count do
  Concurrent::AtomicFixnum.new
end.freeze
# => [#<Concurrent::AtomicFixnum:0x00000e value:0>,
#     #<Concurrent::AtomicFixnum:0x00000f value:0>,
#     #<Concurrent::AtomicFixnum:0x000010 value:0>,
#     #<Concurrent::AtomicFixnum:0x000011 value:0>,
#     #<Concurrent::AtomicFixnum:0x000012 value:0>,
#     #<Concurrent::AtomicFixnum:0x000013 value:0>,
#     #<Concurrent::AtomicFixnum:0x000014 value:0>,
#     #<Concurrent::AtomicFixnum:0x000015 value:0>,
#     #<Concurrent::AtomicFixnum:0x000016 value:0>,
#     #<Concurrent::AtomicFixnum:0x000017 value:0>,
#     #<Concurrent::AtomicFixnum:0x000018 value:0>,
#     #<Concurrent::AtomicFixnum:0x000019 value:0>,
#     #<Concurrent::AtomicFixnum:0x00001a value:0>,
#     #<Concurrent::AtomicFixnum:0x00001b value:0>,
#     #<Concurrent::AtomicFixnum:0x00001c value:0>,
#     #<Concurrent::AtomicFixnum:0x00001d value:0>,
#     #<Concurrent::AtomicFixnum:0x00001e value:0>,
#     #<Concurrent::AtomicFixnum:0x00001f value:0>,
#     #<Concurrent::AtomicFixnum:0x000020 value:0>,
#     #<Concurrent::AtomicFixnum:0x000021 value:0>]

def data_processing(i)
  # pop_op returns a future which is fulfilled with a message from buffer
  # when a message is valuable.
  @buffer.pop_op.then_on(:fast) do |data|
    # then we process the message on :fast pool since this has no blocking
    log DEBUG, "data-processor #{i} got #{data}"
    @data_processing_counters[i].increment
    sleep 0.1 # simulate it actually doing something which take some time
    # find the most frequent char
    data.chars.
        group_by { |v| v }.
        map { |ch, arr| [ch, arr.size] }.
        max_by { |ch, size| size }
  end.then_on(@db_throttle.on(:io)) do |char, count|
    # the db access has to be limited therefore the db_throttle is used
    # DBs use io therefore this part is executed on global thread pool wor :io
    DB.add char, count
  end.then_on(:fast) do |_|
    # last section executes back on :fast executor
    # checks if it was cancelled
    # if not then it calls itself recursively
    # which in combination with #run will turn this into infinite data processing
    # (until cancelled)
    # The #run will keep flatting to the inner future as long the value is a future.
    if @cancellation.canceled?
      # return something else then future, #run will stop executing
      :done
    else
      # continue running with a future returned by data_processing
      data_processing i
    end
  end
end 

# create the data processors
data_processors = Array.new data_processing_count do |i|
  data_processing(i).run
end
# => [#<Concurrent::Promises::Future:0x000022 pending>,
#     #<Concurrent::Promises::Future:0x000023 pending>,
#     #<Concurrent::Promises::Future:0x000024 pending>,
#     #<Concurrent::Promises::Future:0x000025 pending>,
#     #<Concurrent::Promises::Future:0x000026 pending>,
#     #<Concurrent::Promises::Future:0x000027 pending>,
#     #<Concurrent::Promises::Future:0x000028 pending>,
#     #<Concurrent::Promises::Future:0x000029 pending>,
#     #<Concurrent::Promises::Future:0x00002a pending>,
#     #<Concurrent::Promises::Future:0x00002b pending>,
#     #<Concurrent::Promises::Future:0x00002c pending>,
#     #<Concurrent::Promises::Future:0x00002d pending>,
#     #<Concurrent::Promises::Future:0x00002e pending>,
#     #<Concurrent::Promises::Future:0x00002f pending>,
#     #<Concurrent::Promises::Future:0x000030 pending>,
#     #<Concurrent::Promises::Future:0x000031 pending>,
#     #<Concurrent::Promises::Future:0x000032 pending>,
#     #<Concurrent::Promises::Future:0x000033 pending>,
#     #<Concurrent::Promises::Future:0x000034 pending>,
#     #<Concurrent::Promises::Future:0x000035 pending>]

# Some statics are collected in crawler_data_counter
# and @data_processing_counters.
# Schedule a periodical readout to a log.
def readout(crawler_data_counter)
  # schedule readout in 0.4 sec or on cancellation
  (@cancellation.origin | Concurrent::Promises.schedule(0.4)).then do
    log INFO,
        "\ncrawlers found: #{crawler_data_counter.map(&:value).join(', ')}\n" +
            "data processors consumed: #{@data_processing_counters.map(&:value).join(', ')}"
  end.then do
    # reschedule if not cancelled
    readout crawler_data_counter unless @cancellation.canceled?
  end
end                                      # => :readout

# start the periodical readouts
readouts = readout(crawler_data_counter).run
# => #<Concurrent::Promises::Future:0x000036 pending>

sleep 2 # let the whole processing pipeline work
# cancel everything
origin.resolve
# => #<Concurrent::Promises::ResolvableEvent:0x000037 resolved>

# wait for everything to stop
crawlers.each(&:join)
# => [#<Thread:0x000009@medium-example.in.rb:130 dead>,
#     #<Thread:0x00000a@medium-example.in.rb:130 dead>,
#     #<Thread:0x00000b@medium-example.in.rb:130 dead>,
#     #<Thread:0x00000c@medium-example.in.rb:130 dead>]
data_processors.each(&:wait!)[0..10]
# => [#<Concurrent::Promises::Future:0x000022 fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x000023 fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x000024 fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x000025 fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x000026 fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x000027 fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x000028 fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x000029 fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x00002a fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x00002b fulfilled with :done>,
#     #<Concurrent::Promises::Future:0x00002c fulfilled with :done>]
readouts.wait!
# => #<Concurrent::Promises::Future:0x000036 fulfilled with nil>

# terminate the logger
Concurrent::ErlangActor.terminate LOGGING, :cancelled
# => true
LOGGING.terminated.wait
# => #<Concurrent::Promises::Future:0x000038 rejected with :cancelled>

# inspect collected char frequencies
DB.data
# => {"1"=>18,
#     "3"=>18,
#     "4"=>18,
#     "2"=>18,
#     "5"=>18,
#     "8"=>18,
#     "6"=>18,
#     "7"=>18,
#     "a"=>18,
#     "b"=>18,
#     "c"=>18,
#     "9"=>18,
#     "f"=>3,
#     "e"=>18,
#     "d"=>18}

# see the logger output
get_captured_output
# => "[2020-01-26 16:21:45.940] DEBUG -- : crawler 0 found 1\n" +
#    "[2020-01-26 16:21:45.940] DEBUG -- : crawler 2 found 2\n" +
#    "[2020-01-26 16:21:45.940] DEBUG -- : crawler 1 found 3\n" +
#    "[2020-01-26 16:21:45.941] DEBUG -- : data-processor 2 got 3\n" +
#    "[2020-01-26 16:21:45.941] DEBUG -- : crawler 3 found 4\n" +
#    "[2020-01-26 16:21:45.941] DEBUG -- : data-processor 3 got 4\n" +
#    "[2020-01-26 16:21:45.942] DEBUG -- : data-processor 0 got 1\n" +
#    "[2020-01-26 16:21:45.942] DEBUG -- : data-processor 1 got 2\n" +
#    "[2020-01-26 16:21:45.950] DEBUG -- : crawler 1 found 5\n" +
#    "[2020-01-26 16:21:45.951] DEBUG -- : crawler 3 found 6\n" +
#    "[2020-01-26 16:21:45.951] DEBUG -- : crawler 0 found 7\n" +
#    "[2020-01-26 16:21:45.951] DEBUG -- : crawler 2 found 8\n" +
#    "[2020-01-26 16:21:45.951] DEBUG -- : data-processor 7 got 8\n" +
#    "[2020-01-26 16:21:45.952] DEBUG -- : data-processor 4 got 5\n" +
#    "[2020-01-26 16:21:45.952] DEBUG -- : data-processor 5 got 6\n" +
#    "[2020-01-26 16:21:45.952] DEBUG -- : data-processor 6 got 7\n" +
#    "[2020-01-26 16:21:45.961] DEBUG -- : crawler 1 found 9\n" +
#    "[2020-01-26 16:21:45.961] DEBUG -- : crawler 3 found a\n" +
#    "[2020-01-26 16:21:45.962] DEBUG -- : data-processor 9 got a\n" +
#    "[2020-01-26 16:21:45.962] DEBUG -- : crawler 0 found b\n" +
#    "[2020-01-26 16:21:45.962] DEBUG -- : crawler 2 found c\n" +
#    "[2020-01-26 16:21:45.962] DEBUG -- : data-processor 11 got c\n" +
#    "[2020-01-26 16:21:45.963] DEBUG -- : data-processor 8 got 9\n" +
#    "[2020-01-26 16:21:45.963] DEBUG -- : data-processor 10 got b\n" +
#    "[2020-01-26 16:21:45.971] DEBUG -- : crawler 1 found d\n" +
#    "[2020-01-26 16:21:45.972] DEBUG -- : crawler 3 found e\n" +
#    "[2020-01-26 16:21:45.972] DEBUG -- : crawler 0 found f\n" +
#    "[2020-01-26 16:21:45.972] DEBUG -- : crawler 2 found 10\n" +
#    "[2020-01-26 16:21:45.982] DEBUG -- : crawler 3 found 11\n" +
#    "[2020-01-26 16:21:45.983] DEBUG -- : crawler 2 found 12\n" +
#    "[2020-01-26 16:21:45.983] DEBUG -- : crawler 0 found 13\n" +
#    "[2020-01-26 16:21:45.983] DEBUG -- : crawler 1 found 14\n" +
#    "[2020-01-26 16:21:45.993] DEBUG -- : crawler 0 found 15\n" +
#    "[2020-01-26 16:21:45.993] DEBUG -- : crawler 1 found 16\n" +
#    "[2020-01-26 16:21:45.993] DEBUG -- : crawler 2 found 17\n" +
#    "[2020-01-26 16:21:45.994] DEBUG -- : crawler 3 found 18\n" +
#    "[2020-01-26 16:21:46.003] DEBUG -- : crawler 0 found 19\n" +
#    "[2020-01-26 16:21:46.003] DEBUG -- : crawler 2 found 1a\n" +
#    "[2020-01-26 16:21:46.004] DEBUG -- : crawler 3 found 1b\n" +
#    "[2020-01-26 16:21:46.004] DEBUG -- : crawler 1 found 1c\n" +
#    "[2020-01-26 16:21:46.014] DEBUG -- : crawler 2 found 1d\n" +
#    "[2020-01-26 16:21:46.014] DEBUG -- : crawler 0 found 1e\n" +
#    "[2020-01-26 16:21:46.040] DEBUG -- : data-processor 12 got d\n" +
#    "[2020-01-26 16:21:46.041] DEBUG -- : data-processor 13 got e\n" +
#    "[2020-01-26 16:21:46.041] DEBUG -- : data-processor 14 got f\n" +
#    "[2020-01-26 16:21:46.041] DEBUG -- : data-processor 15 got 10\n" +
#    "[2020-01-26 16:21:46.050] DEBUG -- : data-processor 16 got 11\n" +
#    "[2020-01-26 16:21:46.051] DEBUG -- : data-processor 17 got 12\n" +
#    "[2020-01-26 16:21:46.055] DEBUG -- : data-processor 18 got 13\n" +
#    "[2020-01-26 16:21:46.055] DEBUG -- : data-processor 19 got 14\n" +
#    "[2020-01-26 16:21:46.065] DEBUG -- : data-processor 0 got 15\n" +
#    "[2020-01-26 16:21:46.065] DEBUG -- : data-processor 2 got 16\n" +
#    "[2020-01-26 16:21:46.065] DEBUG -- : data-processor 3 got 17\n" +
#    "[2020-01-26 16:21:46.066] DEBUG -- : crawler 1 found 1f\n" +
#    "[2020-01-26 16:21:46.066] DEBUG -- : crawler 3 found 20\n" +
#    "[2020-01-26 16:21:46.066] DEBUG -- : crawler 2 found 21\n" +
#    "[2020-01-26 16:21:46.067] DEBUG -- : crawler 0 found 22\n" +
#    "[2020-01-26 16:21:46.067] DEBUG -- : data-processor 1 got 18\n" +
#    "[2020-01-26 16:21:46.073] DEBUG -- : crawler 3 found 23\n" +
#    "[2020-01-26 16:21:46.074] DEBUG -- : crawler 2 found 24\n" +
#    "[2020-01-26 16:21:46.074] DEBUG -- : crawler 0 found 25\n" +
#    "[2020-01-26 16:21:46.075] DEBUG -- : crawler 1 found 26\n" +
#    "[2020-01-26 16:21:46.141] DEBUG -- : data-processor 4 got 19\n" +
#    "[2020-01-26 16:21:46.142] DEBUG -- : data-processor 7 got 1a\n" +
#    "[2020-01-26 16:21:46.142] DEBUG -- : data-processor 5 got 1b\n" +
#    "[2020-01-26 16:21:46.143] DEBUG -- : data-processor 6 got 1c\n" +
#    "[2020-01-26 16:21:46.151] DEBUG -- : data-processor 9 got 1d\n" +
#    "[2020-01-26 16:21:46.152] DEBUG -- : crawler 3 found 27\n" +
#    "[2020-01-26 16:21:46.152] DEBUG -- : crawler 0 found 28\n" +
#    "[2020-01-26 16:21:46.152] DEBUG -- : crawler 2 found 29\n" +
#    "[2020-01-26 16:21:46.153] DEBUG -- : crawler 1 found 2a\n" +
#    "[2020-01-26 16:21:46.153] DEBUG -- : data-processor 10 got 1e\n" +
#    "[2020-01-26 16:21:46.155] DEBUG -- : data-processor 11 got 1f\n" +
#    "[2020-01-26 16:21:46.156] DEBUG -- : data-processor 8 got 20\n" +
#    "[2020-01-26 16:21:46.163] DEBUG -- : data-processor 15 got 21\n" +
#    "[2020-01-26 16:21:46.164] DEBUG -- : data-processor 14 got 22\n" +
#    "[2020-01-26 16:21:46.164] DEBUG -- : data-processor 13 got 23\n" +
#    "[2020-01-26 16:21:46.165] DEBUG -- : data-processor 12 got 24\n" +
#    "[2020-01-26 16:21:46.165] DEBUG -- : crawler 0 found 2b\n" +
#    "[2020-01-26 16:21:46.165] DEBUG -- : crawler 3 found 2c\n" +
#    "[2020-01-26 16:21:46.166] DEBUG -- : crawler 2 found 2d\n" +
#    "[2020-01-26 16:21:46.166] DEBUG -- : crawler 1 found 2e\n" +
#    "[2020-01-26 16:21:46.246] DEBUG -- : data-processor 16 got 25\n" +
#    "[2020-01-26 16:21:46.246] DEBUG -- : crawler 0 found 2f\n" +
#    "[2020-01-26 16:21:46.247] DEBUG -- : crawler 2 found 30\n" +
#    "[2020-01-26 16:21:46.247] DEBUG -- : crawler 1 found 31\n" +
#    "[2020-01-26 16:21:46.247] DEBUG -- : crawler 3 found 32\n" +
#    "[2020-01-26 16:21:46.247] DEBUG -- : data-processor 17 got 26\n" +
#    "[2020-01-26 16:21:46.248] DEBUG -- : data-processor 18 got 27\n" +
#    "[2020-01-26 16:21:46.248] DEBUG -- : data-processor 19 got 28\n" +
#    "[2020-01-26 16:21:46.252] DEBUG -- : data-processor 0 got 29\n" +
#    "[2020-01-26 16:21:46.253] DEBUG -- : data-processor 2 got 2a\n" +
#    "[2020-01-26 16:21:46.256] DEBUG -- : crawler 0 found 33\n" +
#    "[2020-01-26 16:21:46.256] DEBUG -- : crawler 2 found 34\n" +
#    "[2020-01-26 16:21:46.257] DEBUG -- : crawler 1 found 35\n" +
#    "[2020-01-26 16:21:46.257] DEBUG -- : crawler 3 found 36\n" +
#    "[2020-01-26 16:21:46.259] DEBUG -- : data-processor 3 got 2b\n" +
#    "[2020-01-26 16:21:46.260] DEBUG -- : data-processor 1 got 2c\n" +
#    "[2020-01-26 16:21:46.267] DEBUG -- : data-processor 4 got 2d\n" +
#    "[2020-01-26 16:21:46.268] DEBUG -- : crawler 0 found 37\n" +
#    "[2020-01-26 16:21:46.268] DEBUG -- : crawler 2 found 38\n" +
#    "[2020-01-26 16:21:46.269] DEBUG -- : crawler 1 found 39\n" +
#    "[2020-01-26 16:21:46.269] DEBUG -- : crawler 3 found 3a\n" +
#    "[2020-01-26 16:21:46.270] DEBUG -- : data-processor 6 got 2e\n" +
#    "[2020-01-26 16:21:46.270] DEBUG -- : data-processor 7 got 2f\n" +
#    "[2020-01-26 16:21:46.270] DEBUG -- : data-processor 5 got 30\n" +
#    "[2020-01-26 16:21:46.278] DEBUG -- : crawler 0 found 3b\n" +
#    "[2020-01-26 16:21:46.278] DEBUG -- : crawler 2 found 3c\n" +
#    "[2020-01-26 16:21:46.279] DEBUG -- : crawler 1 found 3d\n" +
#    "[2020-01-26 16:21:46.279] DEBUG -- : crawler 3 found 3e\n" +
#    "[2020-01-26 16:21:46.329]  INFO -- : \n" +
#    "crawlers found: 16, 15, 16, 15\n" +
#    "data processors consumed: 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2\n" +
#    "[2020-01-26 16:21:46.346] DEBUG -- : data-processor 9 got 31\n" +
#    "[2020-01-26 16:21:46.347] DEBUG -- : data-processor 10 got 32\n" +
#    "[2020-01-26 16:21:46.347] DEBUG -- : data-processor 11 got 33\n" +
#    "[2020-01-26 16:21:46.348] DEBUG -- : data-processor 8 got 34\n" +
#    "[2020-01-26 16:21:46.354] DEBUG -- : data-processor 13 got 35\n" +
#    "[2020-01-26 16:21:46.354] DEBUG -- : crawler 0 found 3f\n" +
#    "[2020-01-26 16:21:46.355] DEBUG -- : crawler 2 found 40\n" +
#    "[2020-01-26 16:21:46.355] DEBUG -- : crawler 1 found 41\n" +
#    "[2020-01-26 16:21:46.356] DEBUG -- : data-processor 15 got 36\n" +
#    "[2020-01-26 16:21:46.356] DEBUG -- : crawler 3 found 42\n" +
#    "[2020-01-26 16:21:46.361] DEBUG -- : data-processor 12 got 37\n" +
#    "[2020-01-26 16:21:46.361] DEBUG -- : data-processor 14 got 38\n" +
#    "[2020-01-26 16:21:46.366] DEBUG -- : crawler 0 found 43\n" +
#    "[2020-01-26 16:21:46.367] DEBUG -- : crawler 1 found 44\n" +
#    "[2020-01-26 16:21:46.367] DEBUG -- : crawler 2 found 45\n" +
#    "[2020-01-26 16:21:46.367] DEBUG -- : crawler 3 found 46\n" +
#    "[2020-01-26 16:21:46.368] DEBUG -- : data-processor 16 got 39\n" +
#    "[2020-01-26 16:21:46.369] DEBUG -- : data-processor 17 got 3a\n" +
#    "[2020-01-26 16:21:46.369] DEBUG -- : data-processor 18 got 3b\n" +
#    "[2020-01-26 16:21:46.370] DEBUG -- : data-processor 19 got 3c\n" +
#    "[2020-01-26 16:21:46.447] DEBUG -- : data-processor 0 got 3d\n" +
#    "[2020-01-26 16:21:46.448] DEBUG -- : crawler 3 found 47\n" +
#    "[2020-01-26 16:21:46.448] DEBUG -- : crawler 0 found 48\n" +
#    "[2020-01-26 16:21:46.448] DEBUG -- : crawler 1 found 49\n" +
#    "[2020-01-26 16:21:46.449] DEBUG -- : crawler 2 found 4a\n" +
#    "[2020-01-26 16:21:46.449] DEBUG -- : data-processor 2 got 3e\n" +
#    "[2020-01-26 16:21:46.449] DEBUG -- : data-processor 3 got 3f\n" +
#    "[2020-01-26 16:21:46.451] DEBUG -- : data-processor 1 got 40\n" +
#    "[2020-01-26 16:21:46.455] DEBUG -- : data-processor 4 got 41\n" +
#    "[2020-01-26 16:21:46.455] DEBUG -- : data-processor 5 got 42\n" +
#    "[2020-01-26 16:21:46.458] DEBUG -- : crawler 3 found 4b\n" +
#    "[2020-01-26 16:21:46.459] DEBUG -- : crawler 0 found 4c\n" +
#    "[2020-01-26 16:21:46.459] DEBUG -- : crawler 1 found 4d\n" +
#    "[2020-01-26 16:21:46.459] DEBUG -- : crawler 2 found 4e\n" +
#    "[2020-01-26 16:21:46.462] DEBUG -- : data-processor 6 got 43\n" +
#    "[2020-01-26 16:21:46.465] DEBUG -- : data-processor 7 got 44\n" +
#    "[2020-01-26 16:21:46.470] DEBUG -- : data-processor 9 got 45\n" +
#    "[2020-01-26 16:21:46.470] DEBUG -- : crawler 0 found 50\n" +
#    "[2020-01-26 16:21:46.470] DEBUG -- : crawler 1 found 51\n" +
#    "[2020-01-26 16:21:46.471] DEBUG -- : data-processor 8 got 46\n" +
#    "[2020-01-26 16:21:46.471] DEBUG -- : crawler 3 found 4f\n" +
#    "[2020-01-26 16:21:46.471] DEBUG -- : crawler 2 found 52\n" +
#    "[2020-01-26 16:21:46.471] DEBUG -- : data-processor 11 got 47\n" +
#    "[2020-01-26 16:21:46.472] DEBUG -- : data-processor 10 got 48\n" +
#    "[2020-01-26 16:21:46.480] DEBUG -- : crawler 0 found 53\n" +
#    "[2020-01-26 16:21:46.480] DEBUG -- : crawler 1 found 54\n" +
#    "[2020-01-26 16:21:46.480] DEBUG -- : crawler 2 found 55\n" +
#    "[2020-01-26 16:21:46.481] DEBUG -- : crawler 3 found 56\n" +
#    "[2020-01-26 16:21:46.548] DEBUG -- : data-processor 13 got 49\n" +
#    "[2020-01-26 16:21:46.549] DEBUG -- : data-processor 15 got 4a\n" +
#    "[2020-01-26 16:21:46.549] DEBUG -- : data-processor 12 got 4b\n" +
#    "[2020-01-26 16:21:46.554] DEBUG -- : data-processor 14 got 4c\n" +
#    "[2020-01-26 16:21:46.556] DEBUG -- : data-processor 19 got 4d\n" +
#    "[2020-01-26 16:21:46.557] DEBUG -- : crawler 0 found 57\n" +
#    "[2020-01-26 16:21:46.557] DEBUG -- : data-processor 18 got 4e\n" +
#    "[2020-01-26 16:21:46.557] DEBUG -- : crawler 1 found 58\n" +
#    "[2020-01-26 16:21:46.558] DEBUG -- : crawler 2 found 59\n" +
#    "[2020-01-26 16:21:46.558] DEBUG -- : crawler 3 found 5a\n" +
#    "[2020-01-26 16:21:46.564] DEBUG -- : data-processor 16 got 4f\n" +
#    "[2020-01-26 16:21:46.566] DEBUG -- : data-processor 17 got 50\n" +
#    "[2020-01-26 16:21:46.568] DEBUG -- : crawler 0 found 5b\n" +
#    "[2020-01-26 16:21:46.569] DEBUG -- : crawler 1 found 5c\n" +
#    "[2020-01-26 16:21:46.569] DEBUG -- : crawler 2 found 5d\n" +
#    "[2020-01-26 16:21:46.569] DEBUG -- : crawler 3 found 5e\n" +
#    "[2020-01-26 16:21:46.570] DEBUG -- : data-processor 0 got 51\n" +
#    "[2020-01-26 16:21:46.571] DEBUG -- : data-processor 2 got 52\n" +
#    "[2020-01-26 16:21:46.571] DEBUG -- : data-processor 3 got 53\n" +
#    "[2020-01-26 16:21:46.571] DEBUG -- : data-processor 1 got 54\n" +
#    "[2020-01-26 16:21:46.651] DEBUG -- : data-processor 4 got 55\n" +
#    "[2020-01-26 16:21:46.651] DEBUG -- : crawler 0 found 5f\n" +
#    "[2020-01-26 16:21:46.651] DEBUG -- : crawler 1 found 60\n" +
#    "[2020-01-26 16:21:46.651] DEBUG -- : crawler 3 found 61\n" +
#    "[2020-01-26 16:21:46.652] DEBUG -- : crawler 2 found 62\n" +
#    "[2020-01-26 16:21:46.652] DEBUG -- : data-processor 5 got 56\n" +
#    "[2020-01-26 16:21:46.652] DEBUG -- : data-processor 6 got 57\n" +
#    "[2020-01-26 16:21:46.656] DEBUG -- : data-processor 7 got 58\n" +
#    "[2020-01-26 16:21:46.661] DEBUG -- : data-processor 9 got 59\n" +
#    "[2020-01-26 16:21:46.661] DEBUG -- : data-processor 11 got 5a\n" +
#    "[2020-01-26 16:21:46.662] DEBUG -- : crawler 3 found 63\n" +
#    "[2020-01-26 16:21:46.662] DEBUG -- : crawler 1 found 64\n" +
#    "[2020-01-26 16:21:46.662] DEBUG -- : crawler 0 found 65\n" +
#    "[2020-01-26 16:21:46.662] DEBUG -- : crawler 2 found 66\n" +
#    "[2020-01-26 16:21:46.664] DEBUG -- : data-processor 8 got 5b\n" +
#    "[2020-01-26 16:21:46.667] DEBUG -- : data-processor 10 got 5c\n" +
#    "[2020-01-26 16:21:46.672] DEBUG -- : data-processor 12 got 5d\n" +
#    "[2020-01-26 16:21:46.673] DEBUG -- : crawler 1 found 67\n" +
#    "[2020-01-26 16:21:46.673] DEBUG -- : crawler 0 found 68\n" +
#    "[2020-01-26 16:21:46.673] DEBUG -- : data-processor 13 got 5e\n" +
#    "[2020-01-26 16:21:46.673] DEBUG -- : data-processor 15 got 5f\n" +
#    "[2020-01-26 16:21:46.673] DEBUG -- : crawler 3 found 69\n" +
#    "[2020-01-26 16:21:46.674] DEBUG -- : data-processor 14 got 60\n" +
#    "[2020-01-26 16:21:46.674] DEBUG -- : crawler 2 found 6a\n" +
#    "[2020-01-26 16:21:46.682] DEBUG -- : crawler 0 found 6b\n" +
#    "[2020-01-26 16:21:46.683] DEBUG -- : crawler 3 found 6c\n" +
#    "[2020-01-26 16:21:46.683] DEBUG -- : crawler 1 found 6d\n" +
#    "[2020-01-26 16:21:46.683] DEBUG -- : crawler 2 found 6e\n" +
#    "[2020-01-26 16:21:46.729]  INFO -- : \n" +
#    "crawlers found: 28, 27, 28, 27\n" +
#    "data processors consumed: 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4\n" +
#    "[2020-01-26 16:21:46.751] DEBUG -- : data-processor 19 got 61\n" +
#    "[2020-01-26 16:21:46.757] DEBUG -- : data-processor 18 got 62\n" +
#    "[2020-01-26 16:21:46.757] DEBUG -- : data-processor 16 got 63\n" +
#    "[2020-01-26 16:21:46.760] DEBUG -- : data-processor 17 got 64\n" +
#    "[2020-01-26 16:21:46.766] DEBUG -- : data-processor 0 got 65\n" +
#    "[2020-01-26 16:21:46.766] DEBUG -- : crawler 2 found 6f\n" +
#    "[2020-01-26 16:21:46.767] DEBUG -- : crawler 3 found 70\n" +
#    "[2020-01-26 16:21:46.767] DEBUG -- : crawler 1 found 71\n" +
#    "[2020-01-26 16:21:46.767] DEBUG -- : crawler 0 found 72\n" +
#    "[2020-01-26 16:21:46.768] DEBUG -- : data-processor 3 got 66\n" +
#    "[2020-01-26 16:21:46.768] DEBUG -- : data-processor 2 got 67\n" +
#    "[2020-01-26 16:21:46.768] DEBUG -- : data-processor 1 got 68\n" +
#    "[2020-01-26 16:21:46.772] DEBUG -- : data-processor 4 got 69\n" +
#    "[2020-01-26 16:21:46.777] DEBUG -- : data-processor 6 got 6a\n" +
#    "[2020-01-26 16:21:46.778] DEBUG -- : data-processor 5 got 6b\n" +
#    "[2020-01-26 16:21:46.778] DEBUG -- : data-processor 7 got 6c\n" +
#    "[2020-01-26 16:21:46.778] DEBUG -- : crawler 1 found 73\n" +
#    "[2020-01-26 16:21:46.778] DEBUG -- : crawler 0 found 74\n" +
#    "[2020-01-26 16:21:46.779] DEBUG -- : crawler 2 found 75\n" +
#    "[2020-01-26 16:21:46.779] DEBUG -- : crawler 3 found 76\n" +
#    "[2020-01-26 16:21:46.853] DEBUG -- : data-processor 9 got 6d\n" +
#    "[2020-01-26 16:21:46.854] DEBUG -- : crawler 1 found 77\n" +
#    "[2020-01-26 16:21:46.854] DEBUG -- : crawler 0 found 78\n" +
#    "[2020-01-26 16:21:46.856] DEBUG -- : crawler 2 found 79\n" +
#    "[2020-01-26 16:21:46.856] DEBUG -- : crawler 3 found 7a\n" +
#    "[2020-01-26 16:21:46.861] DEBUG -- : data-processor 11 got 6e\n" +
#    "[2020-01-26 16:21:46.861] DEBUG -- : data-processor 10 got 6f\n" +
#    "[2020-01-26 16:21:46.861] DEBUG -- : data-processor 8 got 70\n" +
#    "[2020-01-26 16:21:46.863] DEBUG -- : crawler 1 found 7b\n" +
#    "[2020-01-26 16:21:46.866] DEBUG -- : crawler 0 found 7c\n" +
#    "[2020-01-26 16:21:46.866] DEBUG -- : crawler 2 found 7d\n" +
#    "[2020-01-26 16:21:46.867] DEBUG -- : crawler 3 found 7e\n" +
#    "[2020-01-26 16:21:46.867] DEBUG -- : data-processor 12 got 71\n" +
#    "[2020-01-26 16:21:46.867] DEBUG -- : data-processor 14 got 72\n" +
#    "[2020-01-26 16:21:46.868] DEBUG -- : data-processor 13 got 73\n" +
#    "[2020-01-26 16:21:46.868] DEBUG -- : data-processor 15 got 74\n" +
#    "[2020-01-26 16:21:46.879] DEBUG -- : data-processor 19 got 75\n" +
#    "[2020-01-26 16:21:46.879] DEBUG -- : crawler 1 found 7f\n" +
#    "[2020-01-26 16:21:46.879] DEBUG -- : data-processor 17 got 76\n" +
#    "[2020-01-26 16:21:46.880] DEBUG -- : data-processor 18 got 77\n" +
#    "[2020-01-26 16:21:46.880] DEBUG -- : crawler 0 found 80\n" +
#    "[2020-01-26 16:21:46.880] DEBUG -- : crawler 2 found 81\n" +
#    "[2020-01-26 16:21:46.881] DEBUG -- : crawler 3 found 82\n" +
#    "[2020-01-26 16:21:46.881] DEBUG -- : data-processor 16 got 78\n" +
#    "[2020-01-26 16:21:46.889] DEBUG -- : crawler 1 found 83\n" +
#    "[2020-01-26 16:21:46.889] DEBUG -- : crawler 0 found 84\n" +
#    "[2020-01-26 16:21:46.889] DEBUG -- : crawler 2 found 85\n" +
#    "[2020-01-26 16:21:46.890] DEBUG -- : crawler 3 found 86\n" +
#    "[2020-01-26 16:21:46.954] DEBUG -- : data-processor 0 got 79\n" +
#    "[2020-01-26 16:21:46.961] DEBUG -- : data-processor 3 got 7a\n" +
#    "[2020-01-26 16:21:46.962] DEBUG -- : data-processor 2 got 7b\n" +
#    "[2020-01-26 16:21:46.962] DEBUG -- : data-processor 1 got 7c\n" +
#    "[2020-01-26 16:21:46.967] DEBUG -- : data-processor 4 got 7d\n" +
#    "[2020-01-26 16:21:46.968] DEBUG -- : crawler 2 found 87\n" +
#    "[2020-01-26 16:21:46.968] DEBUG -- : crawler 3 found 88\n" +
#    "[2020-01-26 16:21:46.969] DEBUG -- : crawler 1 found 89\n" +
#    "[2020-01-26 16:21:46.969] DEBUG -- : crawler 0 found 8a\n" +
#    "[2020-01-26 16:21:46.969] DEBUG -- : data-processor 6 got 7e\n" +
#    "[2020-01-26 16:21:46.969] DEBUG -- : data-processor 5 got 7f\n" +
#    "[2020-01-26 16:21:46.970] DEBUG -- : data-processor 7 got 80\n" +
#    "[2020-01-26 16:21:46.978] DEBUG -- : crawler 2 found 8b\n" +
#    "[2020-01-26 16:21:46.978] DEBUG -- : crawler 3 found 8c\n" +
#    "[2020-01-26 16:21:46.978] DEBUG -- : crawler 1 found 8d\n" +
#    "[2020-01-26 16:21:46.979] DEBUG -- : crawler 0 found 8e\n" +
#    "[2020-01-26 16:21:46.979] DEBUG -- : data-processor 9 got 81\n" +
#    "[2020-01-26 16:21:46.980] DEBUG -- : data-processor 11 got 82\n" +
#    "[2020-01-26 16:21:46.980] DEBUG -- : data-processor 8 got 83\n" +
#    "[2020-01-26 16:21:46.980] DEBUG -- : data-processor 10 got 84\n" +
#    "[2020-01-26 16:21:47.056] DEBUG -- : data-processor 12 got 85\n" +
#    "[2020-01-26 16:21:47.057] DEBUG -- : crawler 2 found 8f\n" +
#    "[2020-01-26 16:21:47.057] DEBUG -- : crawler 3 found 90\n" +
#    "[2020-01-26 16:21:47.057] DEBUG -- : crawler 1 found 91\n" +
#    "[2020-01-26 16:21:47.058] DEBUG -- : crawler 0 found 92\n" +
#    "[2020-01-26 16:21:47.062] DEBUG -- : data-processor 14 got 86\n" +
#    "[2020-01-26 16:21:47.066] DEBUG -- : data-processor 13 got 87\n" +
#    "[2020-01-26 16:21:47.066] DEBUG -- : data-processor 15 got 88\n" +
#    "[2020-01-26 16:21:47.067] DEBUG -- : crawler 2 found 93\n" +
#    "[2020-01-26 16:21:47.067] DEBUG -- : crawler 3 found 94\n" +
#    "[2020-01-26 16:21:47.068] DEBUG -- : crawler 1 found 95\n" +
#    "[2020-01-26 16:21:47.068] DEBUG -- : crawler 0 found 96\n" +
#    "[2020-01-26 16:21:47.068] DEBUG -- : data-processor 19 got 89\n" +
#    "[2020-01-26 16:21:47.069] DEBUG -- : data-processor 18 got 8a\n" +
#    "[2020-01-26 16:21:47.069] DEBUG -- : data-processor 16 got 8b\n" +
#    "[2020-01-26 16:21:47.070] DEBUG -- : data-processor 17 got 8c\n" +
#    "[2020-01-26 16:21:47.080] DEBUG -- : data-processor 0 got 8d\n" +
#    "[2020-01-26 16:21:47.080] DEBUG -- : data-processor 3 got 8e\n" +
#    "[2020-01-26 16:21:47.081] DEBUG -- : crawler 2 found 97\n" +
#    "[2020-01-26 16:21:47.081] DEBUG -- : crawler 3 found 98\n" +
#    "[2020-01-26 16:21:47.081] DEBUG -- : crawler 1 found 99\n" +
#    "[2020-01-26 16:21:47.082] DEBUG -- : crawler 0 found 9a\n" +
#    "[2020-01-26 16:21:47.082] DEBUG -- : data-processor 2 got 8f\n" +
#    "[2020-01-26 16:21:47.082] DEBUG -- : data-processor 1 got 90\n" +
#    "[2020-01-26 16:21:47.091] DEBUG -- : crawler 2 found 9b\n" +
#    "[2020-01-26 16:21:47.092] DEBUG -- : crawler 1 found 9c\n" +
#    "[2020-01-26 16:21:47.092] DEBUG -- : crawler 3 found 9d\n" +
#    "[2020-01-26 16:21:47.092] DEBUG -- : crawler 0 found 9e\n" +
#    "[2020-01-26 16:21:47.133]  INFO -- : \n" +
#    "crawlers found: 40, 39, 40, 39\n" +
#    "data processors consumed: 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7\n" +
#    "[2020-01-26 16:21:47.161] DEBUG -- : data-processor 4 got 91\n" +
#    "[2020-01-26 16:21:47.162] DEBUG -- : data-processor 5 got 92\n" +
#    "[2020-01-26 16:21:47.168] DEBUG -- : data-processor 6 got 93\n" +
#    "[2020-01-26 16:21:47.169] DEBUG -- : data-processor 7 got 94\n" +
#    "[2020-01-26 16:21:47.170] DEBUG -- : data-processor 8 got 95\n" +
#    "[2020-01-26 16:21:47.170] DEBUG -- : crawler 2 found 9f\n" +
#    "[2020-01-26 16:21:47.170] DEBUG -- : crawler 1 found a0\n" +
#    "[2020-01-26 16:21:47.170] DEBUG -- : crawler 3 found a1\n" +
#    "[2020-01-26 16:21:47.171] DEBUG -- : crawler 0 found a2\n" +
#    "[2020-01-26 16:21:47.171] DEBUG -- : data-processor 11 got 96\n" +
#    "[2020-01-26 16:21:47.171] DEBUG -- : data-processor 10 got 97\n" +
#    "[2020-01-26 16:21:47.173] DEBUG -- : data-processor 9 got 98\n" +
#    "[2020-01-26 16:21:47.179] DEBUG -- : crawler 2 found a3\n" +
#    "[2020-01-26 16:21:47.180] DEBUG -- : crawler 1 found a4\n" +
#    "[2020-01-26 16:21:47.180] DEBUG -- : crawler 3 found a5\n" +
#    "[2020-01-26 16:21:47.180] DEBUG -- : data-processor 12 got 99\n" +
#    "[2020-01-26 16:21:47.182] DEBUG -- : data-processor 14 got 9a\n" +
#    "[2020-01-26 16:21:47.183] DEBUG -- : data-processor 13 got 9b\n" +
#    "[2020-01-26 16:21:47.184] DEBUG -- : data-processor 15 got 9c\n" +
#    "[2020-01-26 16:21:47.185] DEBUG -- : crawler 0 found a6\n" +
#    "[2020-01-26 16:21:47.192] DEBUG -- : crawler 2 found a7\n" +
#    "[2020-01-26 16:21:47.192] DEBUG -- : crawler 1 found a8\n" +
#    "[2020-01-26 16:21:47.193] DEBUG -- : crawler 3 found a9\n" +
#    "[2020-01-26 16:21:47.194] DEBUG -- : crawler 0 found aa\n" +
#    "[2020-01-26 16:21:47.205] DEBUG -- : crawler 2 found ab\n" +
#    "[2020-01-26 16:21:47.205] DEBUG -- : crawler 0 found ac\n" +
#    "[2020-01-26 16:21:47.205] DEBUG -- : crawler 1 found ad\n" +
#    "[2020-01-26 16:21:47.262] DEBUG -- : data-processor 19 got 9d\n" +
#    "[2020-01-26 16:21:47.266] DEBUG -- : data-processor 18 got 9e\n" +
#    "[2020-01-26 16:21:47.272] DEBUG -- : data-processor 16 got 9f\n" +
#    "[2020-01-26 16:21:47.273] DEBUG -- : data-processor 17 got a0\n" +
#    "[2020-01-26 16:21:47.273] DEBUG -- : data-processor 0 got a1\n" +
#    "[2020-01-26 16:21:47.273] DEBUG -- : data-processor 1 got a2\n" +
#    "[2020-01-26 16:21:47.274] DEBUG -- : data-processor 2 got a3\n" +
#    "[2020-01-26 16:21:47.278] DEBUG -- : data-processor 3 got a4\n" +
#    "[2020-01-26 16:21:47.278] DEBUG -- : crawler 3 found ae\n" +
#    "[2020-01-26 16:21:47.279] DEBUG -- : crawler 2 found af\n" +
#    "[2020-01-26 16:21:47.279] DEBUG -- : crawler 1 found b0\n" +
#    "[2020-01-26 16:21:47.279] DEBUG -- : crawler 0 found b1\n" +
#    "[2020-01-26 16:21:47.280] DEBUG -- : data-processor 4 got a5\n" +
#    "[2020-01-26 16:21:47.283] DEBUG -- : data-processor 5 got a6\n" +
#    "[2020-01-26 16:21:47.283] DEBUG -- : data-processor 8 got a7\n" +
#    "[2020-01-26 16:21:47.289] DEBUG -- : crawler 2 found b2\n" +
#    "[2020-01-26 16:21:47.289] DEBUG -- : crawler 3 found b3\n" +
#    "[2020-01-26 16:21:47.289] DEBUG -- : crawler 1 found b4\n" +
#    "[2020-01-26 16:21:47.290] DEBUG -- : crawler 0 found b5\n" +
#    "[2020-01-26 16:21:47.290] DEBUG -- : data-processor 6 got a8\n" +
#    "[2020-01-26 16:21:47.367] DEBUG -- : data-processor 11 got a9\n" +
#    "[2020-01-26 16:21:47.369] DEBUG -- : data-processor 10 got aa\n" +
#    "[2020-01-26 16:21:47.373] DEBUG -- : data-processor 7 got ab\n" +
#    "[2020-01-26 16:21:47.376] DEBUG -- : data-processor 9 got ac\n" +
#    "[2020-01-26 16:21:47.377] DEBUG -- : crawler 3 found b6\n" +
#    "[2020-01-26 16:21:47.377] DEBUG -- : crawler 0 found b7\n" +
#    "[2020-01-26 16:21:47.378] DEBUG -- : crawler 1 found b8\n" +
#    "[2020-01-26 16:21:47.378] DEBUG -- : crawler 2 found b9\n" +
#    "[2020-01-26 16:21:47.378] DEBUG -- : data-processor 12 got ad\n" +
#    "[2020-01-26 16:21:47.378] DEBUG -- : data-processor 14 got ae\n" +
#    "[2020-01-26 16:21:47.379] DEBUG -- : data-processor 13 got af\n" +
#    "[2020-01-26 16:21:47.379] DEBUG -- : data-processor 15 got b0\n" +
#    "[2020-01-26 16:21:47.381] DEBUG -- : data-processor 19 got b1\n" +
#    "[2020-01-26 16:21:47.383] DEBUG -- : data-processor 18 got b2\n" +
#    "[2020-01-26 16:21:47.388] DEBUG -- : crawler 1 found ba\n" +
#    "[2020-01-26 16:21:47.388] DEBUG -- : crawler 2 found bb\n" +
#    "[2020-01-26 16:21:47.389] DEBUG -- : data-processor 1 got b3\n" +
#    "[2020-01-26 16:21:47.392] DEBUG -- : crawler 3 found bc\n" +
#    "[2020-01-26 16:21:47.392] DEBUG -- : crawler 0 found bd\n" +
#    "[2020-01-26 16:21:47.392] DEBUG -- : data-processor 16 got b4\n" +
#    "[2020-01-26 16:21:47.400] DEBUG -- : crawler 1 found be\n" +
#    "[2020-01-26 16:21:47.400] DEBUG -- : crawler 2 found bf\n" +
#    "[2020-01-26 16:21:47.400] DEBUG -- : crawler 3 found c0\n" +
#    "[2020-01-26 16:21:47.400] DEBUG -- : crawler 0 found c1\n" +
#    "[2020-01-26 16:21:47.411] DEBUG -- : crawler 2 found c2\n" +
#    "[2020-01-26 16:21:47.411] DEBUG -- : crawler 3 found c3\n" +
#    "[2020-01-26 16:21:47.412] DEBUG -- : crawler 0 found c4\n" +
#    "[2020-01-26 16:21:47.412] DEBUG -- : crawler 1 found c5\n" +
#    "[2020-01-26 16:21:47.467] DEBUG -- : data-processor 0 got b5\n" +
#    "[2020-01-26 16:21:47.469] DEBUG -- : data-processor 17 got b6\n" +
#    "[2020-01-26 16:21:47.475] DEBUG -- : data-processor 2 got b7\n" +
#    "[2020-01-26 16:21:47.477] DEBUG -- : data-processor 3 got b8\n" +
#    "[2020-01-26 16:21:47.478] DEBUG -- : data-processor 4 got b9\n" +
#    "[2020-01-26 16:21:47.478] DEBUG -- : data-processor 5 got ba\n" +
#    "[2020-01-26 16:21:47.479] DEBUG -- : data-processor 8 got bb\n" +
#    "[2020-01-26 16:21:47.481] DEBUG -- : data-processor 6 got bc\n" +
#    "[2020-01-26 16:21:47.481] DEBUG -- : crawler 2 found c6\n" +
#    "[2020-01-26 16:21:47.482] DEBUG -- : crawler 3 found c7\n" +
#    "[2020-01-26 16:21:47.482] DEBUG -- : crawler 0 found c8\n" +
#    "[2020-01-26 16:21:47.482] DEBUG -- : crawler 1 found c9\n" +
#    "[2020-01-26 16:21:47.483] DEBUG -- : data-processor 11 got bd\n" +
#    "[2020-01-26 16:21:47.483] DEBUG -- : data-processor 10 got be\n" +
#    "[2020-01-26 16:21:47.490] DEBUG -- : data-processor 7 got bf\n" +
#    "[2020-01-26 16:21:47.492] DEBUG -- : crawler 1 found ca\n" +
#    "[2020-01-26 16:21:47.493] DEBUG -- : data-processor 9 got c0\n" +
#    "[2020-01-26 16:21:47.493] DEBUG -- : crawler 2 found cb\n" +
#    "[2020-01-26 16:21:47.494] DEBUG -- : crawler 3 found cc\n" +
#    "[2020-01-26 16:21:47.494] DEBUG -- : crawler 0 found cd\n" +
#    "[2020-01-26 16:21:47.534]  INFO -- : \n" +
#    "crawlers found: 52, 51, 52, 50\n" +
#    "data processors consumed: 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 9, 9, 9, 9, 9, 9, 9, 9\n" +
#    "[2020-01-26 16:21:47.571] DEBUG -- : data-processor 13 got c1\n" +
#    "[2020-01-26 16:21:47.572] DEBUG -- : data-processor 14 got c2\n" +
#    "[2020-01-26 16:21:47.577] DEBUG -- : data-processor 15 got c3\n" +
#    "[2020-01-26 16:21:47.581] DEBUG -- : data-processor 12 got c4\n" +
#    "[2020-01-26 16:21:47.582] DEBUG -- : data-processor 19 got c5\n" +
#    "[2020-01-26 16:21:47.583] DEBUG -- : data-processor 18 got c6\n" +
#    "[2020-01-26 16:21:47.583] DEBUG -- : crawler 1 found ce\n" +
#    "[2020-01-26 16:21:47.583] DEBUG -- : crawler 3 found cf\n" +
#    "[2020-01-26 16:21:47.584] DEBUG -- : crawler 0 found d0\n" +
#    "[2020-01-26 16:21:47.585] DEBUG -- : crawler 2 found d1\n" +
#    "[2020-01-26 16:21:47.585] DEBUG -- : data-processor 1 got c7\n" +
#    "[2020-01-26 16:21:47.585] DEBUG -- : data-processor 16 got c8\n" +
#    "[2020-01-26 16:21:47.586] DEBUG -- : data-processor 17 got c9\n" +
#    "[2020-01-26 16:21:47.586] DEBUG -- : data-processor 0 got ca\n" +
#    "[2020-01-26 16:21:47.592] DEBUG -- : crawler 3 found d2\n" +
#    "[2020-01-26 16:21:47.593] DEBUG -- : crawler 1 found d3\n" +
#    "[2020-01-26 16:21:47.593] DEBUG -- : crawler 0 found d4\n" +
#    "[2020-01-26 16:21:47.593] DEBUG -- : crawler 2 found d5\n" +
#    "[2020-01-26 16:21:47.595] DEBUG -- : data-processor 2 got cb\n" +
#    "[2020-01-26 16:21:47.598] DEBUG -- : data-processor 4 got cc\n" +
#    "[2020-01-26 16:21:47.605] DEBUG -- : crawler 3 found d6\n" +
#    "[2020-01-26 16:21:47.605] DEBUG -- : crawler 1 found d7\n" +
#    "[2020-01-26 16:21:47.606] DEBUG -- : crawler 0 found d8\n" +
#    "[2020-01-26 16:21:47.606] DEBUG -- : crawler 2 found d9\n" +
#    "[2020-01-26 16:21:47.615] DEBUG -- : crawler 0 found da\n" +
#    "[2020-01-26 16:21:47.616] DEBUG -- : crawler 2 found db\n" +
#    "[2020-01-26 16:21:47.616] DEBUG -- : crawler 1 found dc\n" +
#    "[2020-01-26 16:21:47.671] DEBUG -- : data-processor 3 got cd\n" +
#    "[2020-01-26 16:21:47.672] DEBUG -- : data-processor 5 got ce\n" +
#    "[2020-01-26 16:21:47.678] DEBUG -- : data-processor 8 got cf\n" +
#    "[2020-01-26 16:21:47.684] DEBUG -- : data-processor 6 got d0\n" +
#    "[2020-01-26 16:21:47.685] DEBUG -- : data-processor 11 got d1\n" +
#    "[2020-01-26 16:21:47.685] DEBUG -- : data-processor 10 got d2\n" +
#    "[2020-01-26 16:21:47.686] DEBUG -- : data-processor 7 got d3\n" +
#    "[2020-01-26 16:21:47.686] DEBUG -- : data-processor 9 got d4\n" +
#    "[2020-01-26 16:21:47.686] DEBUG -- : crawler 3 found dd\n" +
#    "[2020-01-26 16:21:47.686] DEBUG -- : crawler 0 found de\n" +
#    "[2020-01-26 16:21:47.687] DEBUG -- : data-processor 14 got d5\n" +
#    "[2020-01-26 16:21:47.687] DEBUG -- : data-processor 13 got d6\n" +
#    "[2020-01-26 16:21:47.687] DEBUG -- : crawler 2 found df\n" +
#    "[2020-01-26 16:21:47.687] DEBUG -- : crawler 1 found e0\n" +
#    "[2020-01-26 16:21:47.696] DEBUG -- : crawler 3 found e1\n" +
#    "[2020-01-26 16:21:47.696] DEBUG -- : crawler 0 found e2\n" +
#    "[2020-01-26 16:21:47.696] DEBUG -- : crawler 2 found e3\n" +
#    "[2020-01-26 16:21:47.697] DEBUG -- : crawler 1 found e4\n" +
#    "[2020-01-26 16:21:47.697] DEBUG -- : data-processor 15 got d7\n" +
#    "[2020-01-26 16:21:47.697] DEBUG -- : data-processor 12 got d8\n" +
#    "[2020-01-26 16:21:47.772] DEBUG -- : data-processor 19 got d9\n" +
#    "[2020-01-26 16:21:47.773] DEBUG -- : data-processor 16 got da\n" +
#    "[2020-01-26 16:21:47.782] DEBUG -- : data-processor 0 got db\n" +
#    "[2020-01-26 16:21:47.782] DEBUG -- : crawler 3 found e5\n" +
#    "[2020-01-26 16:21:47.782] DEBUG -- : crawler 0 found e6\n" +
#    "[2020-01-26 16:21:47.783] DEBUG -- : crawler 1 found e7\n" +
#    "[2020-01-26 16:21:47.783] DEBUG -- : crawler 2 found e8\n" +
#    "[2020-01-26 16:21:47.784] DEBUG -- : data-processor 17 got dc\n" +
#    "[2020-01-26 16:21:47.785] DEBUG -- : data-processor 18 got dd\n" +
#    "[2020-01-26 16:21:47.785] DEBUG -- : data-processor 1 got de\n" +
#    "[2020-01-26 16:21:47.785] DEBUG -- : data-processor 2 got df\n" +
#    "[2020-01-26 16:21:47.786] DEBUG -- : data-processor 4 got e0\n" +
#    "[2020-01-26 16:21:47.786] DEBUG -- : data-processor 5 got e1\n" +
#    "[2020-01-26 16:21:47.786] DEBUG -- : data-processor 3 got e2\n" +
#    "[2020-01-26 16:21:47.792] DEBUG -- : crawler 0 found e9\n" +
#    "[2020-01-26 16:21:47.792] DEBUG -- : crawler 1 found ea\n" +
#    "[2020-01-26 16:21:47.792] DEBUG -- : crawler 3 found eb\n" +
#    "[2020-01-26 16:21:47.793] DEBUG -- : crawler 2 found ec\n" +
#    "[2020-01-26 16:21:47.797] DEBUG -- : data-processor 8 got e3\n" +
#    "[2020-01-26 16:21:47.799] DEBUG -- : data-processor 6 got e4\n" +
#    "[2020-01-26 16:21:47.802] DEBUG -- : crawler 0 found ed\n" +
#    "[2020-01-26 16:21:47.802] DEBUG -- : crawler 1 found ee\n" +
#    "[2020-01-26 16:21:47.803] DEBUG -- : crawler 3 found ef\n" +
#    "[2020-01-26 16:21:47.803] DEBUG -- : crawler 2 found f0\n" +
#    "[2020-01-26 16:21:47.812] DEBUG -- : crawler 0 found f1\n" +
#    "[2020-01-26 16:21:47.813] DEBUG -- : crawler 1 found f2\n" +
#    "[2020-01-26 16:21:47.813] DEBUG -- : crawler 3 found f3\n" +
#    "[2020-01-26 16:21:47.813] DEBUG -- : crawler 2 found f4\n" +
#    "[2020-01-26 16:21:47.875] DEBUG -- : data-processor 11 got e5\n" +
#    "[2020-01-26 16:21:47.875] DEBUG -- : data-processor 10 got e6\n" +
#    "[2020-01-26 16:21:47.882] DEBUG -- : data-processor 7 got e7\n" +
#    "[2020-01-26 16:21:47.884] DEBUG -- : data-processor 13 got e8\n" +
#    "[2020-01-26 16:21:47.885] DEBUG -- : data-processor 9 got e9\n" +
#    "[2020-01-26 16:21:47.888] DEBUG -- : data-processor 14 got ea\n" +
#    "[2020-01-26 16:21:47.889] DEBUG -- : data-processor 12 got eb\n" +
#    "[2020-01-26 16:21:47.889] DEBUG -- : data-processor 15 got ec\n" +
#    "[2020-01-26 16:21:47.889] DEBUG -- : data-processor 19 got ed\n" +
#    "[2020-01-26 16:21:47.889] DEBUG -- : data-processor 16 got ee\n" +
#    "[2020-01-26 16:21:47.889] DEBUG -- : crawler 1 found f5\n" +
#    "[2020-01-26 16:21:47.890] DEBUG -- : crawler 3 found f6\n" +
#    "[2020-01-26 16:21:47.890] DEBUG -- : crawler 0 found f7\n" +
#    "[2020-01-26 16:21:47.890] DEBUG -- : crawler 2 found f8\n" +
#    "[2020-01-26 16:21:47.899] DEBUG -- : crawler 1 found f9\n" +
#    "[2020-01-26 16:21:47.899] DEBUG -- : crawler 3 found fa\n" +
#    "[2020-01-26 16:21:47.900] DEBUG -- : crawler 0 found fb\n" +
#    "[2020-01-26 16:21:47.900] DEBUG -- : data-processor 0 got ef\n" +
#    "[2020-01-26 16:21:47.902] DEBUG -- : data-processor 17 got f0\n" +
#    "[2020-01-26 16:21:47.930]  INFO -- : \n" +
#    "crawlers found: 64, 63, 63, 61\n" +
#    "data processors consumed: 13, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 11, 12\n" +
#    "[2020-01-26 16:21:47.975] DEBUG -- : data-processor 18 got f1\n" +
#    "[2020-01-26 16:21:48.000] DEBUG -- : crawler 2 found fc\n" +
#    "[2020-01-26 16:21:48.011] DEBUG -- : crawler 1 found fd\n" +
#    "[2020-01-26 16:21:48.011] DEBUG -- : crawler 3 found fe\n" +
#    "[2020-01-26 16:21:48.011] DEBUG -- : crawler 0 found ff\n"



