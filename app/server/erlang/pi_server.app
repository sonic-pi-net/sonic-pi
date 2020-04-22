%% This is an -*- erlang -*- file.
{application, pi_server,
 [{description, "Sonic Pi Server"},
  {vsn, ""},
  {modules, [pi_server,
             pi_server_app,
             pi_server_sup,
             osc
            ]},
  {registered,[]},
  {mod,{pi_server_app,[]}},
  {env, [{enabled, true},
         {in_port, 4560},    % sane defaults for the ports
         {api_port, 51240},
         {cue_host, {127,0,0,1}},
         {cue_port, 51235},
         {internal, true}
        ]},
  {applications, [kernel,stdlib,sasl]}
 ]
}.
