(ns dactyl-keyboard.web
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

; a box with arbitrary corners
(def web-box-faces [[1 0 3 2] ; bottom face
                    [4 5 6 7] ; top face
                    [0 1 5 4]
                    [1 2 6 5]
                    [2 3 7 6]
                    [3 0 4 7]])
(defn web-box [b0 b1 b2 b3 t4 t5 t6 t7]
  (polyhedron [b0 b1 b2 b3 t4 t5 t6 t7] web-box-faces))

; a triangular prism with arbitrary corners
(def web-tri-faces [[0 1 4 3]
                    [1 2 5 4]
                    [2 0 3 5]
                    [2 1 0] ; bottom triangle
                    [3 4 5] ; top triangle
                    ])
(defn web-tri [b0 b1 b2 t3 t4 t5]
  (polyhedron [b0 b1 b2 t3 t4 t5] web-tri-faces))

(defn web-connector-col [edges col row]
  (let [curr (edges [col row])
        next (edges [col (inc row)])]
    (web-box (curr :left-front-bottom)
             (curr :right-front-bottom)
             (next :right-back-bottom)
             (next :left-back-bottom)
             (curr :left-front-top)
             (curr :right-front-top)
             (next :right-back-top)
             (next :left-back-top))))

(defn web-connector-row [edges col row]
  (let [curr (edges [col row])
        next (edges [(inc col) row])]
    (web-box (curr :right-back-bottom)
             (next :left-back-bottom)
             (next :left-front-bottom)
             (curr :right-front-bottom)
             (curr :right-back-top)
             (next :left-back-top)
             (next :left-front-top)
             (curr :right-front-top))))

(defn web-connector-diag-square [edges col row]
  (let [a (edges [col row])
        b (edges [(inc col) row])
        c (edges [(inc col) (inc row)])
        d (edges [col (inc row)])]
    (web-box (a :right-front-bottom)
             (b :left-front-bottom)
             (c :left-back-bottom)
             (d :right-back-bottom)
             (a :right-front-top)
             (b :left-front-top)
             (c :left-back-top)
             (d :right-back-top))))

(defn web-connector-diag-left-back [edges col row]
  (let [a (edges [col row])
        b (edges [(inc col) row])
        c (edges [(inc col) (inc row)])]
    (web-tri (a :right-front-bottom)
             (b :left-front-bottom)
             (c :left-back-bottom)
             (a :right-front-top)
             (b :left-front-top)
             (c :left-back-top))))

(defn web-connector-diag-right-back [edges col row]
  (let [a (edges [col row])
        b (edges [(inc col) row])
        d (edges [col (inc row)])]
    (web-tri (a :right-front-bottom)
             (b :left-front-bottom)
             (d :right-back-bottom)
             (a :right-front-top)
             (b :left-front-top)
             (d :right-back-top))))

(defn web-connector-diag-left-front [edges col row]
  (let [b (edges [(inc col) row])
        c (edges [(inc col) (inc row)])
        d (edges [col (inc row)])]
    (web-tri (b :left-front-bottom)
             (c :left-back-bottom)
             (d :right-back-bottom)
             (b :left-front-top)
             (c :left-back-top)
             (d :right-back-top))))

(defn web-connector-diag-right-front [edges col row]
  (let [a (edges [col row])
        c (edges [(inc col) (inc row)])
        d (edges [col (inc row)])]
    (web-tri (a :right-front-bottom)
             (c :left-back-bottom)
             (d :right-back-bottom)
             (a :right-front-top)
             (c :left-back-top)
             (d :right-back-top))))

(defn web-connector-fill-right-front [edges col row]
  "Fills the key at col, row with a triangle prism to the right-front corner"
  (let [a (edges [col (dec row)])
        b (edges [(inc col) row])]
    (web-box (a :left-front-bottom)
             (a :right-front-bottom)
             (b :left-back-bottom)
             (b :left-front-bottom)
             (a :left-front-top)
             (a :right-front-top)
             (b :left-back-top)
             (b :left-front-top))))

(defn web-connector-fill-left-front [edges col row]
  "Fills the key at col, row with a triangle prism to the left-front corner"
  (let [a (edges [col (dec row)])
        b (edges [(dec col) row])]
    (web-box (a :left-front-bottom)
             (a :right-front-bottom)
             (b :right-front-bottom)
             (b :right-back-bottom)
             (a :left-front-top)
             (a :right-front-top)
             (b :right-front-top)
             (b :right-back-top))))

(defn web-connector-fill-right-back [edges col row]
  "Fills the key at col, row with a triangle prism to the right-back corner"
  (let [a (edges [(inc col) row])
        b (edges [col (inc row)])]
    (web-box (a :left-back-bottom)
             (a :left-front-bottom)
             (b :right-back-bottom)
             (b :left-back-bottom)
             (a :left-back-top)
             (a :left-front-top)
             (b :right-back-top)
             (b :left-back-top))))

(defn web-connector-fill-left-back [edges col row]
  "Fills the key at col, row with a triangle prism to the left-back corner"
  (let [a (edges [(dec col) row])
        b (edges [col (inc row)])]
    (web-box (a :right-front-bottom)
             (a :right-back-bottom)
             (b :right-back-bottom)
             (b :left-back-bottom)
             (a :right-front-top)
             (a :right-back-top)
             (b :right-back-top)
             (b :left-back-top))))

(defn web-connectors [cols rows edges skip? extra]
  (let [place? (fn [col row] (not (skip? col row)))]
    (apply union
           (concat
            ; connect columns
            (for [col (range 0 cols)
                  row (range 0 (dec rows))
                  :when (and (place? col row)
                             (place? col (inc row)))]
              (web-connector-col edges col row))
            ; connect rows
            (for [col (range 0 (dec cols))
                  row (range 0 rows)
                  :when (and (place? col row)
                             (place? (inc col) row))]
              (web-connector-row edges col row))
            ; connect square diagonals
            (for [col (range 0 (dec cols))
                  row (range 0 (dec rows))
                  :when (and (place? col row)
                             (place? col (inc row))
                             (place? (inc col) row)
                             (place? (inc col) (inc row)))]
              (web-connector-diag-square edges col row))
            ; connect triangular diagonals
            (for [col (range 0 (dec cols))
                  row (range 0 (dec rows))
                  :when (and (place? col row)
                             (skip? col (inc row))
                             (place? (inc col) row)
                             (place? (inc col) (inc row)))]
              (web-connector-diag-left-back edges col row))
            (for [col (range 0 (dec cols))
                  row (range 0 (dec rows))
                  :when (and (place? col row)
                             (place? col (inc row))
                             (place? (inc col) row)
                             (skip? (inc col) (inc row)))]
              (web-connector-diag-right-back edges col row))
            (for [col (range 0 (dec cols))
                  row (range 0 (dec rows))
                  :when (and (skip? col row)
                             (place? col (inc row))
                             (place? (inc col) row)
                             (place? (inc col) (inc row)))]
              (web-connector-diag-left-front edges col row))
            (for [col (range 0 (dec cols))
                  row (range 0 (dec rows))
                  :when (and (place? col row)
                             (place? col (inc row))
                             (skip? (inc col) row)
                             (place? (inc col) (inc row)))]
              (web-connector-diag-right-front edges col row))
            ; additional geometry - can't be auto-filled, because we don't know
            ; where it isn't desired for e.g. the thumb cluster connection
            extra))))
