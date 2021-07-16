project('kissnet_ci', 'cpp',
version : '0.1.0',
default_options : ['cpp_std=c++17', 'warning_level=3'])

ssl = dependency('OpenSSL')
output_exe = executable('output', 'kissnet.cpp',
  dependencies: ssl,
  cpp_args: '-DKISSNET_USE_OPENSSL')

cpp = meson.get_compiler('cpp')
os = host_machine.system()

winlibs = []
if os == 'windows'
  foreach l : ['wsock32', 'ws2_32', 'Iphlpapi']
    winlibs += cpp.find_library(l, required: true)
  endforeach
endif

threads = dependency('threads', required: false)
if not threads.found()
  threads = []
endif

dir = 'examples/'
foreach t : ['tcp_echo_server']
  executable(t, dir + t + '/main.cpp', dependencies: [winlibs, threads])
endforeach

dir = 'tests/'
foreach t : ['acceptor', 'endpoint', 'error_handler', 'loopback_tcp', 'loopback_udp', 'simple']
  exe = executable(t, dir + t + '/main.cpp', dependencies: winlibs)
  if ['endpoint', 'error_handler', 'loopback_tcp', 'loopback_udp', 'simple'].contains(t)
    test(t, exe, timeout : 5)
  endif
endforeach

foreach t : ['socket']
  exe = executable(t, dir + t + '/main.cpp', dependencies: [winlibs, threads])
endforeach
