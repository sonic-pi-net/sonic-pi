;;--
;; This file is part of Sonic Pi: http://sonic-pi.net
;; Full project source: https://github.com/samaaron/sonic-pi
;; License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
;;
;; Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
;; All rights reserved.
;;
;; Permission is granted for use, copying, modification, and
;; distribution of modified versions of this work as long as this
;; notice is included.
;;++

;; This file uses Overtone to compile the synths from Clojure to
;; SuperCollider compatible binary files. Overtone is Sonic Pi's big
;; brother. See: http://overtone.github.io

(ns sonic-pi.synths.core
  (:use [overtone.live])

  (:require [clojure.string :as str]))

;; Utility functions (for creating and storing synthdefs)

(def path-to-synthdefs "/Users/sam/Development/RPi/sonic-pi/etc/synthdefs")

(defn save-synthdef [sdef]
  (let [compiled (str path-to-synthdefs "/compiled/" (last (str/split (-> sdef :sdef :name) #"/")) ".scsyndef")
        gv       (str path-to-synthdefs "/graphviz/" (last (str/split (-> sdef :sdef :name) #"/")) ".dot")
        dot     (graphviz sdef)]

    (spit gv dot)
    (overtone.sc.machinery.synthdef/synthdef-write (:sdef sdef) compiled)
    :done))
