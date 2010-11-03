(use '[clojure.string :only (split-lines)])

(defn readBoard
    [filename]
    (defn readLine
        [line]
        (for [c (.toCharArray line)]
            (if (= (int c) 35) ; 35 is #
                1
                0
            )
        )
    )
    (map readLine (split-lines (slurp filename)))
)

(defn cgol
    [brd]

    (defn line
        [y]
        (nth brd y))

    (defn cell
        [x y]
        (nth (line y) x))

    (defn neighbours
        [x y]
        (for [i [(- x 1) x (+ x 1)]
              j [(- y 1) y (+ y 1)]
              :when (and
                  (or (not (= x i)) (not (= y j)))
                  (>= i 0)
                  (>= j 0)
                  (< j (count brd))
                  (< i (count (nth brd j)))
                  (= 1 (cell i j)))]
            [i j]
        )
    )

    (defn neighboursCount
        [x y]
        (count (neighbours x y))
    )

    (defn buildLine
        [y]
        (for [x (range (count (line y)))]
            (if 
                (or (and 
                        (= 1 (cell x y))
                        (= 2 (neighboursCount x y)))
                    (= 3 (neighboursCount x y))) 
                1 0))
    )

    (defn buildBoard
        []
        (for [i (range (count brd))]
            (buildLine i)
        )
    )

    (buildBoard)
)

(defn formatBoard
    [brd]
    (defn formatLine
        [line]
        (defn formatChar
            [ch]
            (if (= 0 ch) "." "#")
        )
        (str (apply str (map formatChar line)) "\n")
    )
    (apply str (map formatLine brd))
)

(defn playGol
    [occurences, brd]
    (def newBrd (cgol brd))
    (println (formatBoard newBrd))
    (if
        (<= occurences 1)
        "Done!"
        (playGol (- occurences 1) newBrd)
    )
)

;; This function parses an integer in a string using the JDK bultin operations
(defn parse-integer
    [str]
    (Integer/parseInt str)
)

(println (playGol (parse-integer (nth *command-line-args* 1)) (readBoard (nth *command-line-args* 0))))
