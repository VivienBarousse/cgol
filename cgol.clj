(def board2 [
    [0, 0, 0, 0, 0],
    [0, 0, 1, 0, 0],
    [0, 0, 1, 0, 0],
    [0, 0, 1, 0, 0],
    [0, 0, 0, 0, 0]])

(defn cgol
    [brd]

    (defn neighbours
        [x y]
        (for [i [(- x 1) x (+ x 1)]
              j [(- y 1) y (+ y 1)]
              :when (and
                  (or (not (= x i)) (not (= y j)))
                  (>= i 0)
                  (>= j 0)
                  (< i (count brd))
                  (< j (count (nth brd i)))
                  (= 1 (nth (nth brd j) i)))]
            [i j]
        )
    )

    (defn neighboursCount
        [x y]
        (count (neighbours x y))
    )

    (defn line
        [y]
        (nth brd y))

    (defn cell
        [x y]
        (nth (line y) x))

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

(println (playGol 100 board2))
