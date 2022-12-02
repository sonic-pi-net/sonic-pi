# Tau

Erlang/Elixir IO Server with C++ NIFs for MIDI and Link plus support for
future Rust NIFs.

# Development mode

To manually start the Tau server in development mode:

1. Set the `MIX_ENV` environment variable to `dev`
2. Run the `setup.dev` mix action
3. Start mix with the `--no-halt` flag.

```batchfile
rem Windows
set MIX_ENV=dev
mix setup.dev
mix run --no-halt
```

```shell
# macOS/Linux
MIX_ENV=dev
mix setup.dev
mix run --no-halt
```

Note, you will need to connect the browser to localhost on the port which is printed to the log files.

Also, note, that unless you compile and provide the shared libraries for the MIDI and Link NIFs, this functionality will not be available.


# Production mode

When Tau is packaged into a Sonic Pi release, it is first turned into a mix release which is then started with `MIX_ENV=prod`. You can manually achieve this with the following:


```batchfile
rem Windows Command Prompt
set MIX_ENV=prod
mix tau.release
_build\prod\rel\tau\bin\tau start > NUL 2>&1
```

```shell
# macOS/Linux Terminal Prompt
MIX_ENV=dev
mix tau.release
_build/prod/rel/tau/bin/tau start > /dev/null 2>&1
```





