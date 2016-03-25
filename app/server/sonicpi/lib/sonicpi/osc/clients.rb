module SonicPi
  module OSC
    class Clients

      def initialize(client_class)
        @client_class = client_class
        @clients = []
        @keep_open = []
      end

      def new_client(port, keep_open=false)
        client = @client_class.new(
          "127.0.0.1", port, use_encoder_cache: true)
        @clients.push client
        if keep_open then
          @keep_open.push port
        end
      end

      def send(*args)
        @clients = @clients.select do |client|
          begin
            client.send(*args)
            true
          rescue Errno::EPIPE, Errno::ECONNREFUSED
            STDERR.puts "Client not listening."
            # Remove the client unless it is marked to be kept around forever.
            @keep_open.include? client.port
          end
        end
      end

    end
  end
end
