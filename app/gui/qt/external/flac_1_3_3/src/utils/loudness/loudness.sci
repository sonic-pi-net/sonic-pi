// Equal Loudness Filter
//
// Adapted from original MATLAB code written by David Robinson
//
//      http://replaygain.hydrogenaudio.org/proposal/equal_loudness.html
//      http://replaygain.hydrogenaudio.org/proposal/mfiles/equalloudfilt.m

// *****************************************************************************
// Print Filter Coefficients
//
// This function takes a vector of filter tap settings, and prints
// each tap setting from least significant to most significant.

function c=printcoeff(p)

  c=coeff(p);
  c=c($:-1:1);

  for ix = 1:1:length(c)
    if ix > 1
        printf(" ")
    end
    printf("%.14f", c(ix));
  end

endfunction

// *****************************************************************************
// Equal Loudness Filter
//
// This function is adapted from David Robison's original MATLAB code.
// Apart from changes to port it to scilab, the other change is to
// use a single specification of the frequency points in the 80dB Equal
// Loudness curve.
//
// The original code had different curves for different sampling
// frequencies. This code dynamically computes the current data
// points to use as determined by the Nyquist frequency.

function [a1,b1,a2,b2]=equalloudfilt(fs);
// Design a filter to match equal loudness curves
// 9/7/2001

[%nargout,%nargin]=argn(0);

// If the user hasn't specified a sampling frequency, use the CD default
if %nargin<1 then
   fs=44100;
end

// Specify the 80 dB Equal Loudness curve
EL80=[0,120;20,113;30,103;40,97;50,93;60,91;70,89;80,87;90,86; ..
      ..
      100,85;200,78;300,76;400,76;500,76;600,76;700,77;800,78;900,79.5; ..
      ..
      1000,80;1500,79;2000,77;2500,74;3000,71.5;3700,70;4000,70.5; ..
      5000,74;6000,79;7000,84;8000,86;9000,86; ..
      ..
      10000,85;12000,95;15000,110;20000,125;24000,140];

for ex = 1:1:length(EL80(:,1))
  if EL80(ex,1) > fs/2
    EL80 = [ EL80(1:ex-1,:); fs/2, EL80(ex-1,2) ];
    break
  elseif EL80(ex,1) == fs/2
    EL80 = EL80(1:ex,:);
    break
  end
  if ex == length(EL80(:,1))
    EL80 = [ EL80(1:$, :); fs/2, EL80($,2) ];
  end
end

// convert frequency and amplitude of the equal loudness curve into format suitable for yulewalk
f=EL80(:,1)./(fs/2);
m=10.^((70-EL80(:,2))/20);

// Use a MATLAB utility to design a best bit IIR filter
[b1,a1]=yulewalk(10,f,m);

// Add a 2nd order high pass filter at 150Hz to finish the job
hz=iir(2,'hp','butt',[150/fs,0],[1e-3 1e-3]);
b2=numer(hz); // b2=b2($:-1:1);
a2=denom(hz); // a2=a2($:-1:1);

endfunction

// *****************************************************************************
// Generate Filter Taps
//
// Generate the filter taps for each of the desired frequencies.

format('v', 16);

freqs = [  8000 11025 12000 16000 18900 22050 24000 ..
          28000 32000 36000 37800 44100 48000 ];

for fx = 1:1:length(freqs)

  printf("\n%d\n", freqs(fx));

  [a1,b1,a2,b2] = equalloudfilt(freqs(fx));

  printf("{ "); bb=printcoeff(b1); printf(" }\n");
  printf("{ "); aa=printcoeff(a1); printf(" }\n");

  printf("{ "); printcoeff(b2); printf(" }\n");
  printf("{ "); printcoeff(a2); printf(" }\n");

// freqz_fwd(bb,aa,1024,freqs(fx));

end


quit
