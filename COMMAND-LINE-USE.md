# Run sonic-pi from command line (tested on OSX)

Useful for development/command line enjoyment

## How to

```sh
# Follow the main readme's to install dependencies
$ brew install fswatch
$ gem install sonic-pi-cli # install it separate to preserve the purity of Gemfile.development
$ cd $SONIC_PI_HOME
$ BUNDLE_GEMFILE=Gemfile.development bundle install
$ BUNDLE_GEMFILE=Gemfile.development bundle exec ruby app/server/bin/sonic-pi-console.rb &
$ BUNDLE_GEMFILE=Gemfile.development bundle exec ruby app/server/bin/sonic-pi-server.rb &
$ cd the/dir/with/sonic-pi-loops
$ fswatch ./**.rb | while read file; do echo "Sending file $file to sonic-pi"; cat $file | sonic_pi; done &
# Try inotifywait for similar use on linux
# Use $YOUR_FAVORITE_EDITOR to edit .rb files with your sonic pi loops
```

