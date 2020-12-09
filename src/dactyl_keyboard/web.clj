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
(defn web-box [p0 p1 p2 p3]
  (let [b0 (p0 :bottom)
        b1 (p1 :bottom)
        b2 (p2 :bottom)
        b3 (p3 :bottom)
        t0 (p0 :top)
        t1 (p1 :top)
        t2 (p2 :top)
        t3 (p3 :top)]
    (polyhedron [b0 b1 b2 b3 t0 t1 t2 t3] web-box-faces)))

; a triangular prism with arbitrary corners
(def web-tri-faces [[0 1 4 3]
                    [1 2 5 4]
                    [2 0 3 5]
                    [2 1 0] ; bottom triangle
                    [3 4 5] ; top triangle
                    ])
(defn web-tri [p0 p1 p2]
  (let [b0 (p0 :bottom)
        b1 (p1 :bottom)
        b2 (p2 :bottom)
        t0 (p0 :top)
        t1 (p1 :top)
        t2 (p2 :top)]
    (polyhedron [b0 b1 b2 t0 t1 t2] web-tri-faces)))

(defn web-connector-col [edges col row]
  (let [curr (edges [col row])
        next (edges [col (inc row)])]
    (web-box (curr :left-front)
             (curr :right-front)
             (next :right-back)
             (next :left-back))))

(defn web-connector-row [edges col row]
  (let [curr (edges [col row])
        next (edges [(inc col) row])]
    (web-box (curr :right-back)
             (next :left-back)
             (next :left-front)
             (curr :right-front))))

(defn web-connector-diag-square [edges col row]
  (let [a (edges [col row])
        b (edges [(inc col) row])
        c (edges [(inc col) (inc row)])
        d (edges [col (inc row)])]
    (web-box (a :right-front)
             (b :left-front)
             (c :left-back)
             (d :right-back))))

(defn web-connector-diag-left-back [edges col row]
  (let [a (edges [col row])
        b (edges [(inc col) row])
        c (edges [(inc col) (inc row)])]
    (web-tri (a :right-front)
             (b :left-front)
             (c :left-back))))

(defn web-connector-diag-right-back [edges col row]
  (let [a (edges [col row])
        b (edges [(inc col) row])
        d (edges [col (inc row)])]
    (web-tri (a :right-front)
             (b :left-front)
             (d :right-back))))

(defn web-connector-diag-left-front [edges col row]
  (let [b (edges [(inc col) row])
        c (edges [(inc col) (inc row)])
        d (edges [col (inc row)])]
    (web-tri (b :left-front)
             (c :left-back)
             (d :right-back))))

(defn web-connector-diag-right-front [edges col row]
  (let [a (edges [col row])
        c (edges [(inc col) (inc row)])
        d (edges [col (inc row)])]
    (web-tri (a :right-front)
             (c :left-back)
             (d :right-back))))

(defn web-connector-fill-right-front [edges col row]
  "Fills the key at col, row with a triangle prism to the right-front corner"
  (let [a (edges [col (dec row)])
        b (edges [(inc col) row])]
    (web-box (a :left-front)
             (a :right-front)
             (b :left-back)
             (b :left-front))))

(defn web-connector-fill-left-front [edges col row]
  "Fills the key at col, row with a triangle prism to the left-front corner"
  (let [a (edges [col (dec row)])
        b (edges [(dec col) row])]
    (web-box (a :left-front)
             (a :right-front)
             (b :right-front)
             (b :right-back))))

(defn web-connector-fill-right-back [edges col row]
  "Fills the key at col, row with a triangle prism to the right-back corner"
  (let [a (edges [(inc col) row])
        b (edges [col (inc row)])]
    (web-box (a :left-back)
             (a :left-front)
             (b :right-back)
             (b :left-back))))

(defn web-connector-fill-left-back [edges col row]
  "Fills the key at col, row with a triangle prism to the left-back corner"
  (let [a (edges [(dec col) row])
        b (edges [col (inc row)])]
    (web-box (a :right-front)
             (a :right-back)
             (b :right-back)
             (b :left-back))))

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
