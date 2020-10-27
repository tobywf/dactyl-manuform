(ns dactyl-keyboard.dactyl
    (:refer-clojure :exclude [use import])
    (:require [clojure.core.matrix :refer [array matrix mmul]]
              [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]))

(def π pi)
(defn deg2rad [degrees]
  (* (/ degrees 180) π))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options (if you don't want to read the entire code) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; for Cherry MX or Gateron switches, this can be turned on to hold the switches
; more securely. for other switches (like Kailh), turn this off. you can print
; the switch hole plate tester to check the fit.
(def create-side-nubs? true)

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

; Note: units are all in mm

; dimensions of the switch hole, matches my Gaterons pretty closely
(def switch-hole-height 14.15)
(def switch-hole-width switch-hole-height)
; switch hole wall thickness - i have set this so two walls and the switch hole
; is the same size as a keycap (~18.5mm)
(def wall-thickness 2.35)
; depth of the switch hole - 4mm is really the minimum for side nubs to work,
; 5mm is possible but might make soldering tricky. print the plate tester!
(def plate-depth 4)
; depth of the side nubs, this couldn't be bigger
(def side-nub-depth 4)
; starting depth of the retention tab hole/cutout from the top of the plate
(def retention-start-depth 1.5)
(def retention-hole-depth (- plate-depth retention-start-depth))
; width/height of the retention tab hole/cutout
(def retention-hole-size 5)

(defn key-plate [width-multiplier]
  (let [
        ; wall with hole for switch's retention tab
        top-wall (->> (cube (+ switch-hole-width (* wall-thickness 2))
                            wall-thickness
                            plate-depth)
                      (translate [0
                                  (+ (/ wall-thickness 2) (/ switch-hole-height 2))
                                  (/ plate-depth 2)]))
        ; hole for switch's retention tab
        retention-hole (->> (cube retention-hole-size
                                 retention-hole-size
                                 retention-hole-depth)
                           (translate [(+ (/ switch-hole-width 2.5))
                                       0
                                       ; shift up slightly to prevent z-fighting
                                       (- (/ retention-hole-depth 2) 0.1)])
                           (rotate (/ π 2) [0 0 1]))
        ; calculations for plates wider than 1.0u
        ; half the 1.0u plate's width
        plate-half-width (+ (/ switch-hole-width 2) wall-thickness)
        ; the extra width to add to 1.0u (for 1.0u, this will be 0)
        plate-extra-width (* plate-half-width (- width-multiplier 1))
        ; the wall width doesn't include the thickness/hole
        ; (for 1.0u, this will just be the wall thickness)
        left-wall-width (+ wall-thickness plate-extra-width)
        ; wall with optional side nub
        left-wall (->> (cube left-wall-width
                             (+ switch-hole-height (* wall-thickness 2))
                             plate-depth)
                       (translate [(+ (/ left-wall-width 2) (/ switch-hole-width 2))
                                   0
                                   (/ plate-depth 2)]))
        ; side nub (wedge and rounded bottom)
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ switch-hole-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-depth)
                                 (translate [(+ (/ 1.5 2) (/ switch-hole-width 2))
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
