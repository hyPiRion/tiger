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

(defn careful-read [fstream]
  (do (.mark fstream 1) (.read fstream)))

(defn is-terminal? [c]
  (let [terminal-set #{\space, \newline, \return, \tab, \+, \,, \., \{, \[, \(, \<,
                       \!, \}, \], \), \;, \-, \*, \&}]
    (if (= c -1)
      true
      (some? (terminal-set (char c))))))

(defn expect-id-or-digit [fstream s linenum]
  (letfn [(is-digit? [n] (re-matches #"\d" n))]
    (cond
      (every? is-digit? (map str (apply vector s))) (do (.reset fstream)
                                                      {:kind :TOKEN_NUM, :linenum linenum, :lexem s})
      :else (do (.reset fstream)
              {:kind :TOKEN_ID, :linenum linenum, :lexem s}))))

(defn expect [fstream match]
  (letfn [(eq [x] (= (careful-read fstream) x))]
    (every? eq (map int (apply vector match)))))

(defn expect-id-key [fstream terminal s linenum]
  "Return a map which represent Token"
  (cond
    (= terminal -1) {:kind :TOKEN_EOF, :linenum linenum, :lexem "none"}
    (some? (token-map s)) (do
                            (.reset fstream)
                            {:kind (token-map s), :linenum linenum, :lexem "none"})
    (and (= s "") (= \& (char terminal))) (if (expect fstream "&")
                                            {:kind :TOKEN_AND, :linenum linenum, :lexem "none"}
                                            (throw (Exception. (str "Syntax error at line: " linenum))))
    (and (= s "") (some? (token-map (str (char terminal))))) {:kind (token-map (str (char terminal))), :linenum linenum, :lexem "none"}
    :else (expect-id-or-digit fstream s linenum)))

(defn expect-comm [fstream linenum]
  "Expect comment Token"
  (loop [c (careful-read fstream)]
    (cond
      (= (char c) \/) (letfn [(comm-eat [fstream linenum]
                                (loop [c (careful-read fstream)]
                                  (cond
                                    (= c -1) (do (.reset fstream) {:kind :TOKEN_COMMENT, :linenum linenum, :lexem "comment"})
                                    (= \newline (char c)) (do (.reset fstream) {:kind :TOKEN_COMMENT, :linenum linenum, :lexem "comment"})
                                    :else (recur (careful-read fstream)))))]
                        (comm-eat fstream linenum))
      (= (char c) \*) (letfn [(comm-exit [fstream linenum]
                                (loop [c (careful-read fstream)]
                                  (cond
                                    (= c -1) (throw (Exception. (str "Unclosed comment! at line: " linenum)))
                                    (= \* (char c)) (recur (careful-read fstream))
                                    (= \/ (char c)) :COMMENT_END
                                    :else c)))
                              (comm-eat [fstream line]
                                (loop [c (careful-read fstream) linenum line]
                                  (cond
                                    (= :COMMENT_END c) {:kind :TOKEN_COMMENT, :linenum linenum, :lexem "comment"}
                                    (= c -1) (throw (Exception. (str "Unclosed comment! at line: " linenum)))
                                    (= \newline (char c)) (recur (careful-read fstream) (inc linenum))
                                    (= \* (char c)) (recur (comm-exit fstream linenum) linenum)
                                    :else (recur (careful-read fstream) linenum))))]
                        (comm-eat fstream linenum))
      :else (throw (Exception. (str "Syntax error at line: " linenum))))))

(defn next-token-internal [fstream line]
  (loop [c (careful-read fstream) linenum line]
    (cond
      (= -1 c) {:kind :TOKEN_EOF, :linenum linenum, :lexem "none"}
      (or (= c 9) (= c 13) (= c 32)) (recur (do (careful-read fstream)) linenum)
      (= c 10) (recur (do (careful-read fstream)) (inc linenum))
      :else (loop [cc c s ""]
              (cond
                (= (int \/) cc) (expect-comm fstream linenum)
                (is-terminal? cc) (expect-id-key fstream cc s linenum)
                :else (recur (do (careful-read fstream))
                             (str s (char cc))))))))

(defn next-token [fstream line]
  (loop [t (next-token-internal fstream line)]
    (cond
      (= :TOKEN_COMMENT (t :kind)) (recur (next-token-internal fstream (t :linenum)))
      :else t)))
