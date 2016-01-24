(ns tiger.test.lexer
  (:use [tiger.lexer])
  (:use [clojure.test])
  (:import (java.io FileInputStream InputStreamReader BufferedReader))
  (:import (java.io FileOutputStream OutputStreamWriter BufferedWriter))) 

(defn parser [src]
  "Return token vector"
  (let [sb (StringBuilder.)]
    (with-open [reader (-> src
                         FileInputStream.
                         InputStreamReader.
                         BufferedReader.)]
      (loop [t (next-token reader 1 ) v []]
        (if (= (t :kind) :TOKEN_EOF)
          v
          (recur (next-token reader (t :linenum)) (concat v (vector t))))))))

(deftest lexer-test
         (let [t (parser "test/tiger/LinkedList.java")]
           (is (= :TOKEN_CLASS ((nth t 0) :kind)))
           (is (= "LinkedList" ((nth t 1) :lexem)))
           (is (= :TOKEN_LBRACE ((nth t 2) :kind)))
           (is (= 2 ((nth t 3) :linenum)))
           (is (= "Married" ((nth t 42) :lexem)))
           (is (= 10 ((nth t 43) :linenum)))
           (is (= 13 ((nth t 44) :linenum)))
           (is (= :TOKEN_AND ((nth t 473) :kind)))
           (is (= :TOKEN_LBRACE ((nth t 511) :kind)))
           (is (= 139 ((nth t 512) :linenum)))
           (is (= "39" ((nth t 924) :lexem)))
           (is (= :TOKEN_NUM ((nth t 1052) :kind)))
           (is (= 258 ((nth t 1055) :linenum)))
           (is (= 278 ((last t) :linenum)))
           (is (= :TOKEN_RBRACE ((last t) :kind)))))
