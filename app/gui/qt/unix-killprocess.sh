for pid in `ps ax | grep sonic | cut -d' ' -f1`; do kill -9 $pid; done
