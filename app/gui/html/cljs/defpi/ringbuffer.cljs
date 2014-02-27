(ns defpi.ringbuffer)

(declare rb-seq)

(deftype RingBuffer [size idx content]
  ISeqable
  (-seq [this] (rb-seq this)))

(defn mk-ringbuffer [size]
  (RingBuffer. size 0 {}))

(defn add [rb el]
  (let [idx  (.-idx rb)
        size (.-size rb)]
    (RingBuffer. size
                 (mod (inc idx) size)
                 (assoc (.-content rb)
                   idx el))))

(defn rb-rseq
  ([rb] (rb-seq rb (.-idx rb) (.-size rb)))
  ([rb idx left]
     (lazy-seq
      (when (> left 0)
        (cons (get (.-content rb) idx) (rb-seq rb (mod (inc idx) (.-size rb)) (dec left)))))))

(defn rb-seq
  ([rb] (rb-seq rb (.-idx rb) (.-size rb)))
  ([rb idx left]
     (let [idx (mod (dec idx) (.-size rb)) ]
       (lazy-seq
        (when (> left 0)
          (cons (get (.-content rb) idx) (rb-seq rb idx (dec left))))))))
