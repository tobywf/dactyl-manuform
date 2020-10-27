(ns dactyl-keyboard.dactyl
    (:refer-clojure :exclude [use import])
    (:require [clojure.core.matrix :refer [array matrix mmul]]
              [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]))

(def π pi)
(defn deg2rad [degrees]
  (* (/ degrees 180) π))

;;;;;;;;;;;;;;;;;
;; Conventions ;;
;;;;;;;;;;;;;;;;;

; all units are in mm
; width is x, height is y, depth is z, length or size is usually x or y

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

; --- main options

; for Cherry MX or Gateron switches, this can be turned on to hold the switches
; more securely. for other switches (like Kailh), turn this off. you can print
; the switch hole plate tester to check the fit.
(def create-side-nubs? true)

; --- geometry options, may need tweaking for non-Cherry MXs or non-Gaterons

; dimensions of the switch hole (matches my Gaterons pretty closely)
(def switch-hole-size 14.15)
; switch hole wall thickness - set this so two walls and the switch hole are the
; same size as a unit length keycap (~18.5mm)
(def wall-thickness 2.35)
(def keycap-size (+ switch-hole-size (* wall-thickness 2)))
; depth of the switch hole - 4mm is really the minimum for side nubs to work,
; 5mm is possible but might make soldering tricky. print the plate tester!
(def plate-depth 4)
; depth of the side nubs, this couldn't be bigger due to switch geometry
(def side-nub-depth 4)
; starting depth of the retention tab hole/cutout from the top of the plate
(def retention-start-depth 1.5)
(def retention-hole-depth (- plate-depth retention-start-depth))
; width/height of the retention tab hole/cutout
(def retention-hole-size 5)

(defn key-plate [width-multiplier]
  (let [
        ; wall with hole for switch's retention tab
        top-wall (->> (cube keycap-size wall-thickness plate-depth)
                      (translate [0
                                  (+ (/ wall-thickness 2) (/ switch-hole-size 2))
                                  (/ plate-depth 2)]))
        ; hole for switch's retention tab
        retention-hole (->> (cube retention-hole-size
                                 retention-hole-size
                                 retention-hole-depth)
                           (translate [(+ (/ switch-hole-size 2.5))
                                       0
                                       ; shift up slightly to prevent z-fighting
                                       (- (/ retention-hole-depth 2) 0.1)])
                           (rotate (/ π 2) [0 0 1]))
        ; calculations for plates wider than 1.0u
        ; the extra width to add to 1.0u (for 1.0u, this will be 0)
        plate-extra-width (* (/ keycap-size 2) (- width-multiplier 1))
        ; the wall width doesn't include the thickness/hole
        ; (for 1.0u, this will just be the wall thickness)
        left-wall-width (+ wall-thickness plate-extra-width)
        ; wall with optional side nub
        left-wall (->> (cube left-wall-width
                             (+ switch-hole-size (* wall-thickness 2))
                             plate-depth)
                       (translate [(+ (/ left-wall-width 2) (/ switch-hole-size 2))
                                   0
                                   (/ plate-depth 2)]))
        ; side nub (wedge and rounded bottom)
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ switch-hole-size 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-depth)
                                 (translate [(+ (/ 1.5 2) (/ switch-hole-size 2))
                                             0
                                             (/ side-nub-depth 2)])))
                      (translate [0 0 (- plate-depth side-nub-depth)]))
        plate-half (union (difference top-wall retention-hole)
                          left-wall
                          (if create-side-nubs? side-nub))]

    (union plate-half (->> plate-half (mirror [1 0 0]) (mirror [0 1 0])))))

(def single-plate (key-plate 1.0))
(def one-quater-plate (key-plate 1.25))
(def one-half-plate (key-plate 1.5))
(def double-plate (key-plate 2.0))

; the plate tester is to quickly print the switch holes and check switches fit
(def plate-tester
  (->> (union (->> one-half-plate (translate [0 -20 0]))
              (->> single-plate (translate [10 0 0]))
              (->> one-quater-plate (translate [-13 0 0]))
              (->> double-plate (translate [0 20 0])))
       ; flip the plates on their head for easier printing (no supports)
       (mirror [0 0 1])
       (translate [0 0 plate-depth])))
(spit "things/plate-tester.scad" (write-scad plate-tester))

;;;;;;;;;;;;;
;; Keycaps ;;
;;;;;;;;;;;;;

; --- main options

; the depth of the profile; i.e. how tall is the keycap?
(def keycap-profile-depth 7.5)
; the depth of the switch; i.e. how far does it stick up from the plate until
; the keycap begins? set this to close to zero to simulate keys being "pressed"
(def switch-depth 6.3)

; --- geometry options

(def keycap-face-size 12) ; only affects key cap rendering, nothing else

(defn key-cap [width-multiplier]
  (let [cap-width (* keycap-size width-multiplier)
        ; don't scale the face width to keep the edge at the same angle
        face-diff (- keycap-size keycap-face-size)
        face-width (- cap-width face-diff)
        cap (hull (->> (square cap-width keycap-size)
                       (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                       (translate [0 0 0.05]))
                  (->> (square face-width keycap-face-size)
                       (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                       (translate [0 0 keycap-profile-depth]))
                  )
        push (cube cap-width keycap-size switch-depth)]
   (union (->> cap
               (translate [0 0 (+ plate-depth switch-depth)])
               (color [220/255 163/255 163/255 1.0]))
          (->> push
               (translate [0 0 (+ plate-depth (/ switch-depth 2))])
               (color [135/255 206/255 235/255 0.2])))))

(def single-cap (key-cap 1.0))
(def one-quater-cap (key-cap 1.25))
(def one-half-cap (key-cap 1.5))
(def double-cap (key-cap 2.0))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

; number of rows, if changed also look at "back-row?" and "row-angle-offset"
(def rows 5)
; front rows curvature
(def α1 (/ π 18))
; back rows curvature
(def α2 (/ π 8))
; row spacing
(def row-spaced (+ keycap-size 1))
; which curvature to apply (note: range is 0/backmost - rows/frontmost)
(defn back-row? [row] (< row 3))
; basically which row is considered the "bottom" of the curve
(def row-angle-offset 2)

; number of columns, if changed also look at "inner-col?", "col-angle-offset",
; and "col-offsets"
(def columns 6)
; outer column curvature
(def β1 (/ π 32))
; inner column curvature
(def β2 (/ π 24))
; column spacing
(def col-spaced (+ keycap-size 4))
; which curvature to apply (note range is 0/innermost - columns/outermost)
(defn inner-col? [column] (< column 4))
; basically which column is considered the "bottom" of the curve
(def col-angle-offset 3)
; move columns around to adjust for different finger lengths
(defn col-offset [col] (cond (= col 2) [0 2.82 -3.0]
                             (>= col 4) [0 -5.8 5.64]
                             :else [0 0 0]))

; coarse tilt/tenting adjust (separate from the curvature, acts on all keys)
(def tilt (/ π 24))
; raise plates (equals more height to print, but also to wire)
(def place-z-offset 13)
; which keys to skip (default is inner front and outer front)
(defn skip-place? [col row]
  (and (= row (dec rows)) (or (= col 0) (= col (dec columns)))))

; --- geometry options (mainly derived from curvature params)

(def cap-top-depth (+ plate-depth keycap-profile-depth))
(def row-radius1 (+ (/ (/ row-spaced 2) (Math/sin (/ α1 2))) cap-top-depth))
(def row-radius2 (+ (/ (/ row-spaced 2) (Math/sin (/ α2 2))) cap-top-depth))
(def col-radius1 (+ (/ (/ col-spaced 2) (Math/sin (/ β1 2))) cap-top-depth))
(def col-radius2 (+ (/ (/ col-spaced 2) (Math/sin (/ β2 2))) cap-top-depth))

(defn key-place [col row shape]
  (let [
        row-radius (if (back-row? row) row-radius2 row-radius1)
        row-angle (* (if (back-row? row) α2 α1) (- row-angle-offset row))
        col-radius (if (inner-col? col) col-radius2 col-radius1)
        col-angle (* (if (inner-col? col) β2 β1) (- col-angle-offset col))
        placed-shape (->> shape
                          ; row rotation should be first, to preserve columnar
                          ; distances (i.e. distances between keys in a col)
                          (translate [0 0 (- row-radius)])
                          (rotate row-angle [1 0 0])
                          (translate [0 0 row-radius])
                          ; next, apply column rotation/curvatures
                          (translate [0 0 (- col-radius)])
                          (rotate col-angle [0 1 0])
                          (translate [0 0 col-radius])
                          ; tweak translation to account for finger lengths
                          (translate (col-offset col)))]
    (->> placed-shape
         (rotate tilt [0 1 0])
         (translate [0 0 place-z-offset]))))

(defn place-all [single-shape one-half-shape]
  (apply union
         (for [col (range 0 columns)
               row (range 0 rows)
               :when (not (skip-place? col row))]
           (key-place col row single-shape))))

(def key-plates (place-all single-plate one-half-plate))
(def key-caps (place-all single-cap one-half-cap))

(spit "things/right.scad" (write-scad (union key-plates key-caps)))
