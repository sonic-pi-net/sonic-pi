for pid in `ps ax | grep sonic | cut -d' ' -f1`; do kill -9 $pid; done
for pid in `ps ax | grep scsynth | cut -d' ' -f1`; do kill -9 $pid; done
