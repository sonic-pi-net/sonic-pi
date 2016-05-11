SynthDef( \scope, 
  { |bus = 0, scopeNum = 0, maxFrames = 4096, scopeFrames = nil|
    if( scopeFrames == nil, { scopeFrames = maxFrames } );
    ScopeOut2.ar(In.ar(bus,2),scopeNum,maxFrames) }
).writeDefFile("/tmp/");

