(ns projecteuler.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]
          [clojure.math.combinatorics :as combo])
  )



(defn isprime?
  [value]
  (loop [x 2]
    (if-not (<= x (math/sqrt value))
      true
      (if  (= ( mod value x) 0)
        false
        (recur (inc x))
      )
     )   
    )
  )

(defn nextprime
  [value]
  (loop [ x (inc value)]
    (if (isprime? x)
      x
      (recur (inc x))
      )
    )
  
  )


(defn pe1
  "Find the sum of all the multiples of 3 or 5 below 1000."
  [limit]
  
  (loop [x 0 a [] ]
   (if (>= x limit)
      (reduce + a)
      (recur (inc x) 
             (if (or (= (mod x 3) 0) (= (mod x 5) 0 ))
                (conj a x)
                a
                )
             )
      )
   )
)

(defn pe2
  "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
  [limit]
  (loop  [x 1 y 1 a [] ]
    (if (>= y limit)
      (reduce + a)
      (recur 
        y
        (+ x y)
        (if (= (mod y 2) 0)
          (conj a y)
          a
          )
        )
      )
    )
  )

(defn pe3
  "What is the largest prime factor of the number 600851475143 ?"
  [val]
  (loop [x 2 q val]
    (if-not (< x q)
      q
      (if-not (= 0 (mod q x))
        (recur (nextprime x) q)
        (recur (nextprime x) (quot q x ) )
        )
      )
    
    )
  )
    
  
 

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "PE1 is" (pe1 1000))
  (println "PE2 is" (pe2 4000000))
  (println "PE3 is" (pe3 600851475143))
 )
  
