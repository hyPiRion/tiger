(ns tiger.lexer)

(def token-map
  {
   "+" :TOKEN_ADD,
   "&&" :TOKEN_AND,
   "=" :TOKEN_ASSIGN,
   "boolean" :TOKEN_BOOLEAN,
   "class" :TOKEN_CLASS,
   "," :TOKEN_COMMER,
   "." :TOKEN_DOT,
   "else" :TOKEN_ELSE,
   "EOF" :TOKEN_EOF,
   "extends" :TOKEN_EXTENDS,
   "false" :TOKEN_FALSE,
   "if" :TOKEN_IF,
   "int" :TOKEN_INT,
   "{" :TOKEN_LBRACE,
   "[" :TOKEN_LBRACK,
   "length" :TOKEN_LENGTH,
   "(" :TOKEN_LPAREN,
   "<" :TOKEN_LT,
   "main" :TOKEN_MAIN,
   "new" :TOKEN_NEW,
   "!" :TOKEN_NOT,
   ;NUM
   "out" :TOKEN_OUT,
   "println" :TOKEN_PRINTLN,
   "public" :TOKEN_PUBLIC,
   "}" :TOKEN_RBRACE,
   "]" :TOKEN_RBRACK,
   "return" :TOKEN_RETURN,
   ")" :TOKEN_PAREN,
   ";" :TOKEN_SEMI,
   "static" :TOKEN_STATIC,
   "String" :TOKEN_STRING,
   "-" :TOKEN_SUB,
   "System" :TOKEN_SYSTEM,
   "this" :TOKEN_THIS,
   "*" :TOKEN_TIME,
   "true" :TOKEN_TRUE,
   "void" :TOKEN_VOID,
   "while" :TOKEN_WHILE
   })

(def terminal-set 
  #{\space, \newline, \return, \tab, \+, \=, \,, \., \{, \[, \(, \<,
    \!, \}, \], \), \;, \-, \*})

(defn is-terminal? [c]
  (if (= c -1)
    true
    (some? (terminal-set (char c)))))

(defn is-digit? [n]
  "Test if a Character is digit [0-9]"
  (re-matches #"\d" n))

(defn expect-id-or-digit [s linenum]
  (cond
    (every? is-digit? (map str (apply vector s))) {:kind :TOKEN_NUM, :linenum linenum, :lexem s}
    :else {:kind :TOKEN_ID, :linenum linenum, :lexem s}))

(defn expect-id-key [fstream terminal s linenum]
  "description"
  (cond
    (= terminal -1) {:kind :TOKEN_EOF, :linenum linenum, :lexem "none"}
    (some? (token-map s)) (do
                           (.reset fstream)
                           {:kind (token-map s), :linenum linenum, :lexem "none"})
    (and (= s "") (some? (token-map (str (char terminal))))) {:kind (token-map (str (char terminal))), :linenum linenum, :lexem "none"}
    :else (expect-id-or-digit s linenum)))

(defn skip-comm [fstream linenum]
  (loop [c (.read fstream)]
    (cond
      (= (char c) \/) (letfn [(comm-eat [fstream linenum]
                                (loop [c (.read fstream)]
                                  (cond
                                    (= c -1) {:next c, :linenum linenum}
                                    (= \newline (char c)) {:next (.read fstream), :linenum (inc linenum)}
                                    :else (recur (.read fstream)))))]
                        (comm-eat fstream linenum))
      (= (char c) \*) (letfn [(comm-exit [fstream]
                                (loop [c (.read fstream)]
                                  (cond
                                    (= c -1) (print "Unclosed comment!")
                                    (= \* (char c)) (recur (.read fstream))
                                    (= \/ (char c)) :COMMENT_END
                                    :else c)))
                              (comm-eat [fstream line]
                                (loop [c (.read fstream) linenum line]
                                  (cond
                                    (= :COMMENT_END c) {:next (.read fstream), :linenum linenum}
                                    (= c -1) (print "Unclosed comment!")
                                    (= \newline (char c)) (recur (.read fstream) (inc linenum))
                                    (= \* (char c)) (recur (comm-exit fstream) linenum)
                                    :else (recur (.read fstream) linenum))))]
                        (comm-eat fstream linenum))
      :else (print "ERROR"))))

(defn next-token-internal [fstream pos line]
  (loop [c (.read fstream) linenum line]
    (cond
      (= -1 c) {:kind :TOKEN_EOF, :linenum linenum, :lexem "none"}
      (or (= c 9)
          (= c 13)
          (= c 32)) (recur (do (.mark fstream 1) (.read fstream)) linenum)
      (= c 10) (recur (do (.mark fstream 1) (.read fstream)) (inc linenum))
      (= \/ (char c)) (let [ret (skip-comm fstream linenum)]
                        (recur (ret :next) (ret :linenum)))
      :else (loop [cc c s ""]
              (cond
                (is-terminal? cc) (expect-id-key fstream cc s linenum)
                :else (recur (do (.mark fstream 1) (.read fstream))
                             (str s (char cc))))))))

(defn next-token [fstream pos line]
  (do (.mark fstream 1)
    (next-token-internal fstream 0 line)))
