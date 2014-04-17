# compatible with ruby 1.8, 1.9, and jruby
$:.unshift File.join( File.dirname( __FILE__ ), '..', 'lib')
require 'osc-ruby'

require 'rubygems'
require 'eventmachine'
require 'osc-ruby/em_server'

@server = OSC::EMServer.new( 3333 )
@client = OSC::Client.new( 'localhost', 3333 )

@server.add_method '/greeting' do | message |
  puts "#{message.ip_address}:#{message.ip_port} -- #{message.address} -- #{message.to_a}"
end

Thread.new do
  @server.run
end

@client.send( OSC::Message.new( "/greeting" , "hullo!" ))

sleep( 3 )