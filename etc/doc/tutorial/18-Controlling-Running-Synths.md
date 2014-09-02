# Controlling Running Synths

So far we've only concerned ourselves with triggering new sounds and FX. However, Sonic Pi gives us the ability to manipulate and control currently running sounds.

```
s = play 60, release: 5
sleep 0.5
control s, note: 65
sleep 0.5
control s, note: 67
sleep 3
control s, note: 72
```
