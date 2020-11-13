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

; "tester" files are for printing, and checking that everything works. i highly
; recommend printing these before printing the final keyboard.

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

; --- main options

; for Cherry MX or Gateron switches, this can be turned on to hold the switches
; more securely. for other switches (like Kailh), turn this off.
; you can print the switch hole plate tester to check the fit. turned off
; automatically for the key well tester, so switches are easier to remove.
(def create-nubs? true)
; create holes for the little caches on switches to mount in. i haven't found a
; reason to turn this off - it might make printing easier?
; you can print the switch hole plate tester to check the fit. turned off
; automatically for the key well tester, so switches are easier to remove.
(def create-holes? true)

; --- geometry options, may need tweaking for non-Cherry MXs or non-Gaterons

; dimensions of the switch hole (matches my Gaterons pretty closely)
(def switch-hole-size 14.15)
; switch hole wall thickness - set this so two walls and the switch hole are the
; same size as a unit length keycap (~18.5mm)
(def wall-thickness 2.35)
(def keycap-size (+ switch-hole-size (* wall-thickness 2)))
; depth of the switch hole - 4mm is really the minimum for side nubs to work,
; 5mm is possible but might make soldering tricky. print the plate tester!
(def plate-thickness 4)
; depth of the side nubs, this couldn't be bigger due to switch geometry
(def side-nub-depth 4)
; starting depth of the retention tab hole/cutout from the top of the plate
(def retention-start-depth 1.5)
(def retention-hole-depth (- plate-thickness retention-start-depth))
; width/height of the retention tab hole/cutout
(def retention-hole-size 5)

(defn key-plate [holes? nubs? width-multiplier]
  (let [
        ; wall with hole for switch's retention tab
        top-wall (->> (cube keycap-size wall-thickness plate-thickness)
                      (translate [0
                                  (+ (/ wall-thickness 2) (/ switch-hole-size 2))
                                  (/ plate-thickness 2)]))
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
                             plate-thickness)
                       (translate [(+ (/ left-wall-width 2) (/ switch-hole-size 2))
                                   0
                                   (/ plate-thickness 2)]))
        ; side nub (wedge and rounded bottom)
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ switch-hole-size 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-depth)
                                 (translate [(+ (/ 1.5 2) (/ switch-hole-size 2))
                                             0
                                             (/ side-nub-depth 2)])))
                      (translate [0 0 (- plate-thickness side-nub-depth)]))
        top-wall (if holes? (difference top-wall retention-hole) top-wall)
        left-wall (if nubs? (union left-wall side-nub) left-wall)
        plate-half (union top-wall left-wall)]

    (union plate-half (->> plate-half (mirror [1 0 0]) (mirror [0 1 0])))))

; default plate, with specified features
(def key-plate-default (partial key-plate create-holes? create-nubs?))
; test plate, with no holes or nubs for easier key removal - note this isn't
; used for they key plate tester, since you want to check the features. but it
; is handy for the key well tester
(def key-plate-simple (partial key-plate false false))

(def single-plate (key-plate-default 1.00))
(def one-quater-plate (key-plate-default 1.25))
(def one-half-plate (key-plate-default 1.50))
(def double-plate (key-plate-default 2.00))

; the plate tester is to quickly print the switch holes and check switches fit
(def plate-tester
  (->> (union (->> one-half-plate (translate [0 -20 0]))
              (->> single-plate (translate [10 0 0]))
              (->> one-quater-plate (translate [-13 0 0]))
              (->> double-plate (translate [0 20 0])))
       ; flip the plates on their head for easier printing (no supports)
       (mirror [0 0 1])
       (translate [0 0 plate-thickness])))
(spit "things/plate-tester.scad" (write-scad plate-tester))

;;;;;;;;;;;;;
;; Keycaps ;;
;;;;;;;;;;;;;

; --- main options

; the depth of the profile; i.e. how tall is the keycap? (default is DSA)
(def keycap-profile-depth 7.5)
; the depth of the switch; i.e. how far does it stick up from the plate until
; the keycap begins?
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
               (translate [0 0 (+ plate-thickness switch-depth)])
               (color [220/255 163/255 163/255 1.0]))
          (->> push
               (translate [0 0 (+ plate-thickness (/ switch-depth 2))])
               (color [135/255 206/255 235/255 0.2])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key well Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

(def columns 6)
(def rows 5)

; which keys to skip (default is inner front and outer front)
(defn skip-place? [col row]
  (and (= row (dec rows)) (or (= col 0) (= col (dec columns)))))
; TODO: parametrize me
(defn shape-width [col row] 1.0)

; the grid spacing
(def col-spaced (+ keycap-size 2.0))
(def row-spaced (+ keycap-size 0.5))

; --- curvature options

; the options are usually parametrised per column

; the radius of a circle onto which each column's keys will be mapped
(def col-radius [70 85 75 70 90 75])
; the depth/z offset for each column; roughly corresponds to finger length
; this is subtracted, so higher values means deeper/lower
(def col-depth [22 22 20 20 12 5])
; a general depth/z offset, to adjust keys up after col-depth was applied
(def col-z-offset 30)
; the tilt of each column in degrees
(def col-tilt-deg [0 -5 -10 -10 -18 -28])
; small tweak to column spacing - useful to compensate for large differences
; in tilt between columns
(def col-x-tweak [0.0 0.0 0.5 0.0 0.0 0.5])
(def col-y-tweak [-3.0 -4.0 0.0 0.0 0.0 -2.0])
; which key is the center/lowest point of the circle - can be fractional
(def row-center 1.75)

; --- geometry options

(def cap-depth (+ plate-thickness switch-depth))
; angle = row-spaced (arc length) / radius
(def col-angle (mapv (fn [r] (/ row-spaced (- r cap-depth))) col-radius))

(defn key-place [col row shape]
  (let [
        ; invert row, so row 0 is at the front (y positive) and row 5 is at the
        ; back (y negative)
        row (- row-center row)
        ; x is grid spacing, but because of the tilt it may need a tweak
        x (+ (* col col-spaced) (col-x-tweak col))
        ; z is easy, too
        z (- col-z-offset (col-depth col))
        ; y is harder - want to map the linear row position onto a circle/arc
        α (col-angle col)
        r (col-radius col)
        y (col-y-tweak col)
        ; finally, the tilt of each column to create a bowl-like key well
        β (deg2rad (col-tilt-deg col))]
   (->> shape
              (translate [0 0 (- r)])
              (rotate (* row α) [1 0 0])
              (translate [0 0 r])
              (translate [0 0 (- plate-thickness)])
              (rotate β [0 1 0])
              (translate [0 0 plate-thickness])
              (translate [x y z]))))

; TODO: parametrize me
(defn place-all [shape]
  (apply union
         (for [col (range 0 columns)
               row (range 0 rows)
               :when (not (skip-place? col row))]
           (key-place col row shape))))
(def key-plates (place-all (key-plate-default 1.0)))
(def key-caps (place-all (key-cap 1.0)))
; test plate, with no holes or nubs for easier key removal
(def test-plates (place-all (key-plate-simple 1.0)))

; the visual key well representation is good for checking clearances
(spit "things/keywell-visual.scad" (write-scad (union key-plates key-caps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key well Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

; see skipped-keys-fill below - i can't move it up here because of where the
; function definitions are

; --- geometry options (shouldn't need changing)

(def web-thickness plate-thickness)
(def post-size 0.1)
(def post-adj (/ post-size 2))
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2) plate-thickness)])))

; ---

; most of these functions can take a different shape. this seems unnecessary
; now, but i had planned to also generate support material

(defn post-tr [shape col row]
  (let [keycap-width (* keycap-size (shape-width col row))]
   (translate [(- (/ keycap-width 2) post-adj) (- (/ keycap-size 2) post-adj) 0] shape)))
(defn post-tl [shape col row]
  (let [keycap-width (* keycap-size (shape-width col row))]
   (translate [(+ (/ keycap-width -2) post-adj) (- (/ keycap-size 2) post-adj) 0] shape)))
(defn post-bl [shape col row]
  (let [keycap-width (* keycap-size (shape-width col row))]
   (translate [(+ (/ keycap-width -2) post-adj) (+ (/ keycap-size -2) post-adj) 0] shape)))
(defn post-br [shape col row]
  (let [keycap-width (* keycap-size (shape-width col row))]
   (translate [(- (/ keycap-width 2) post-adj) (+ (/ keycap-size -2) post-adj) 0] shape)))

(def web-post-tr (partial post-tr web-post))
(def web-post-tl (partial post-tl web-post))
(def web-post-bl (partial post-bl web-post))
(def web-post-br (partial post-br web-post))

(defn post-place [col row shape-fn] (key-place col row (shape-fn col row)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

; triangle in top-right (actually 3 triangles, to fill plate and connectors)
; 2 +--+ 3
;    \ |
;     \|
;      * 1
(defn triangle-tr [shape col row]
  (let [up-row (dec row)
        right-col (inc col)
        shape-post-tr (partial post-tr shape)
        shape-post-tl (partial post-tl shape)
        shape-post-bl (partial post-bl shape)
        shape-post-br (partial post-br shape)]
   (union
     (triangle-hulls ; triangle to fill plate
       (post-place right-col row shape-post-bl)
       (post-place col up-row shape-post-bl)
       (post-place col row shape-post-tr))
     (triangle-hulls ; triangle to fill top connector
       (post-place col up-row shape-post-bl)
       (post-place col up-row shape-post-br)
       (post-place col row shape-post-tr))
     (triangle-hulls ; triangle to fill right connector
       (post-place right-col row shape-post-tl)
       (post-place right-col row shape-post-bl)
       (post-place col row shape-post-tr)))))

; triangle in top-left (actually 3 triangles, to fill plate and connectors)
; 3 +--+ 1
;   | /
;   |/
; 2 *
(defn triangle-tl [shape col row]
  (let [up-row (dec row)
        left-col (dec col)
        shape-post-tr (partial post-tr shape)
        shape-post-tl (partial post-tl shape)
        shape-post-bl (partial post-bl shape)
        shape-post-br (partial post-br shape)]
   (union
     (triangle-hulls ; triangle to fill plate
       (post-place col up-row shape-post-br)
       (post-place left-col row shape-post-br)
       (post-place col row shape-post-tl))
     (triangle-hulls ; triangle to fill left connector
       (post-place left-col row shape-post-br)
       (post-place left-col row shape-post-tr)
       (post-place col row shape-post-tl))
     (triangle-hulls ; triangle to fill top connector
       (post-place col up-row shape-post-br)
       (post-place col up-row shape-post-bl)
       (post-place col row shape-post-tl)))))

; triangle in bottom-right (actually two triangles, to fill plate and connector)
;      * 2
;     /|
;    / |
; 1 +--+ 3
(defn triangle-br [shape col row]
  (let [down-row (inc row)
        right-col (inc col)
        shape-post-tr (partial post-tr shape)
        shape-post-tl (partial post-tl shape)
        shape-post-bl (partial post-bl shape)
        shape-post-br (partial post-br shape)]
   (union
     (triangle-hulls ; triangle to fill plate
       (post-place col down-row shape-post-tl)
       (post-place right-col row shape-post-tl)
       (post-place col row shape-post-br))
     (triangle-hulls ; triangle to fill right connector
       (post-place right-col row shape-post-tl)
       (post-place right-col row shape-post-bl)
       (post-place col row shape-post-br))
     (triangle-hulls ; triangle to fill bottom connector
       (post-place col down-row shape-post-tr)
       (post-place col down-row shape-post-tl)
       (post-place col row shape-post-br)))))

; triangle in bottom-left (actually two triangles, to fill plate and connector)
; *
; |\
; | \
; +--+
(defn triangle-bl [shape col row]
  (let [down-row (inc row)
        left-col (dec col)
        shape-post-tr (partial post-tr shape)
        shape-post-tl (partial post-tl shape)
        shape-post-bl (partial post-bl shape)
        shape-post-br (partial post-br shape)]
   (union
     (triangle-hulls ; triangle to fill plate
       (post-place left-col row shape-post-tr)
       (post-place col down-row shape-post-tr)
       (post-place col row shape-post-bl))
     (triangle-hulls ; triangle to fill left connector
       (post-place left-col row shape-post-br)
       (post-place left-col row shape-post-tr)
       (post-place col row shape-post-bl))
     (triangle-hulls ; triangle to fill top connector
       (post-place col down-row shape-post-tr)
       (post-place col down-row shape-post-tl)
       (post-place col row shape-post-bl)))))

; --- main options

; fill geometry for skipped keys (e.g. corners if desired). also see loop for
; diagonal connections below, and skip those if no fill geometry is desired.
(defn skipped-keys-fill [shape]
  [(triangle-tr shape 0 (dec rows))
   (triangle-tl shape (dec columns) (dec rows))])

; ---

(def web-connectors
  (apply union
         (concat
          ; row connections
          (for [col (drop-last (range 0 columns))
                row (range 0 rows)
                :when (not (or (skip-place? col row)
                               (skip-place? (inc col) row)))]
            (triangle-hulls
             (post-place (inc col) row web-post-tl)
             (post-place col row web-post-tr)
             (post-place (inc col) row web-post-bl)
             (post-place col row web-post-br)))
          ; column connections
          (for [col (range 0 columns)
                row (drop-last (range 0 rows))
                :when (not (or (skip-place? col row)
                               (skip-place? col (inc row))))]
            (triangle-hulls
             (post-place col row web-post-bl)
             (post-place col row web-post-br)
             (post-place col (inc row) web-post-tl)
             (post-place col (inc row) web-post-tr)))
          ; diagonal connections
          (for [col (drop-last (range 0 columns))
                row (drop-last (range 0 rows))]
                ; if skipped keys aren't filled, these can be left off
                ; :when (not (or (skip-place? col row)
                ;                (skip-place? col (inc row))
                ;                (skip-place? (inc col) row)
                ;                (skip-place? (inc col) (inc row))))]
            (triangle-hulls
             (post-place col row web-post-br)
             (post-place col (inc row) web-post-tr)
             (post-place (inc col) row web-post-bl)
             (post-place (inc col) (inc row) web-post-tl)))
          ; geometry to fill skipped keys
          (skipped-keys-fill web-post)
          )))

; the key well tester is good for checking the geometry by printing
(spit "things/keywell-tester.scad" (write-scad (union test-plates web-connectors)))
