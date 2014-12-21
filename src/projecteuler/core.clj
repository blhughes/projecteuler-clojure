(ns projecteuler.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]
          [clojure.math.combinatorics :as combo]
          [clojure.string :as str])
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
    
(defn revstr [x] (apply str (reverse (str x))))    
(defn palindrome? [x] (= (str x) (revstr x)))
(defn factor [x] 
  (filter 
     (fn [y] (= 0 (mod x y)))
     (range 1 (inc x)))
)  
(defn size3factors? [x] 
  (if (> 2 (count (seq x)))
    false
    (if (and (= 3 (count (str (first x ))))
             (= 3 (count (str (last  x ))))
             )
      true      
      (recur (rest (drop-last x)))
      
      )
    )
  )
  
(defn pe4
  "A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
  Find the largest palindrome made from the product of two 3-digit numbers."
  []
  (loop [x 1000000]
    (if ( and
         (palindrome? x)
         (size3factors? (factor x))
       )
      x      
      (recur (dec x))
      )
    )  
  
  )  


(defn pe5 [x]
 "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 (* 2 3 5 7 11 13 17 19 4 3 2)
 " 
 (let [r (set (range 1 (inc x)))]
 (loop [y 1]
   (if (clojure.set/subset? r (set (factor y)))
     (do 
       (println y (factor y))
        y
     )
     (recur (inc y))
     )
   ))
 )

(defn pe6 [x]
  (- 
    (math/expt (reduce + (range 1 (inc x))) 2)   
    (reduce + 
            (map
              (fn [y](math/expt y 2))
              (range 1 (inc x))
              )
            )
    )
)

(defn pe7 [x]
  (loop [y 1 prime 2]
    (if (= y x)
      prime
      (recur (inc y) (nextprime prime))
      )
    )
  
  )


(defn pe8 [c]
  (def string "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450")
  (def string (str/join (str/split string #"\n")))
  (def array 
    (vec (map (fn [x] (Integer. (str x))) (vec (char-array string))))   
     )  
   (apply max (map 
     (fn [x]
       (reduce *
               (take c (drop (- 1000 x) array))
               )
       )
     (range 1 (inc (- (count array) c)))
     ))
  
  )

(defn -main
  "Print the results of each project euler problem"
  [& args]
  (println "PE1 is" (pe1 1000))
  (println "PE2 is" (pe2 4000000))
  (println "PE3 is" (pe3 600851475143))
  (println "PE4 is" (pe4))
  (println "PE5 is" (* 2 3 5 7 11 13 17 19 4 3 2))
  (println "PE6 is" (pe6 100))
  (println "PE7 is" (pe7 10001))
  (println "PE8 is" (pe8 13))
  
 )
  
