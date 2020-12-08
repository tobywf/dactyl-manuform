(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.holders :refer :all]
            [dactyl-keyboard.web :refer :all]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

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
(def plate-thickness 4.0)
; depth of the side nubs, this couldn't be bigger due to switch geometry
(def side-nub-depth 4.0)
; starting depth of the retention tab hole/cut-out from the top of the plate
(def retention-start-depth 1.5)
(def retention-hole-depth (- plate-thickness retention-start-depth))
; width/height of the retention tab hole/cut-out
(def retention-hole-size 5.0)

(defn key-plate [holes? nubs? width-multiplier]
  (let [; wall with hole for switch's retention tab
        top-wall (->> (cube keycap-size wall-thickness plate-thickness)
                      (translate [0.0
                                  (+ (/ wall-thickness 2) (/ switch-hole-size 2))
                                  (/ plate-thickness 2)]))
        ; hole for switch's retention tab
        retention-hole (->> (cube retention-hole-size
                                  retention-hole-size
                                  retention-hole-depth)
                            (translate [(+ (/ switch-hole-size 2.5))
                                        0.0
                                       ; shift up slightly to prevent z-fighting
                                        (- (/ retention-hole-depth 2) z-fighting)])
                            (rotate (/ pi 2) [0 0 1]))
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
                                   0.0
                                   (/ plate-thickness 2)]))
        ; side nub (wedge and rounded bottom)
        side-nub (->> (with-fn 30 (cylinder 1.0 2.75))
                      (rotate (/ pi 2) [1 0 0])
                      (translate [(+ (/ switch-hole-size 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-depth)
                                 (translate [(+ (/ 1.5 2) (/ switch-hole-size 2))
                                             0.0
                                             (/ side-nub-depth 2)])))
                      (translate [0.0 0.0 (- plate-thickness side-nub-depth)]))
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
  (->> (union (->> one-half-plate (translate [0.0 -20.0 0.0]))
              (->> single-plate (translate [10.0 0.0 0.0]))
              (->> one-quater-plate (translate [-13.0 0.0 0.0]))
              (->> double-plate (translate [0.0 20.0 0.0])))
       ; flip the plates on their head for easier printing (no supports)
       (mirror [0 0 1])
       (translate [0.0 0.0 plate-thickness])))
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

(def keycap-face-size 12.0) ; only affects key cap rendering, nothing else

(defn key-cap [width-multiplier]
  (let [cap-width (* keycap-size width-multiplier)
        ; don't scale the face width to keep the edge at the same angle
        face-diff (- keycap-size keycap-face-size)
        face-width (- cap-width face-diff)
        cap (hull (->> (square cap-width keycap-size)
                       (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                       (translate [0.0 0.0 0.05]))
                  (->> (square face-width keycap-face-size)
                       (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                       (translate [0 0 keycap-profile-depth])))
        push (cube cap-width keycap-size switch-depth)]
    (union (->> cap
                (translate [0.0 0.0 (+ plate-thickness switch-depth)])
                (color [220/255 163/255 163/255 1.0]))
           (->> push
                (translate [0.0 0.0 (+ plate-thickness (/ switch-depth 2))])
                (color [135/255 206/255 235/255 0.2])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Well Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

(def key-columns 6)
(def key-rows 5)

; which keys to skip (default is inner front and outer front)
(defn key-skip-place? [col row]
  (and (= row (dec key-rows)) (or (= col 0) (= col (dec key-columns)))))
(defn key-shape-width [col row] 1.0)

; the grid spacing
(def key-col-spaced (+ keycap-size 2.0))
(def key-row-spaced (+ keycap-size 0.5))

; --- curvature options

; the options are usually parametrised per column

; the radius of a circle onto which each column's keys will be mapped
(def key-col-radius [70 85 75 70 90 75])
; the depth/z offset for each column; roughly corresponds to finger length
; this is subtracted, so higher values means deeper/lower
(def key-col-depth [22 22 20 20 12 5])
; a general depth/z offset, to adjust keys up after key-col-depth was applied
(def key-col-z-offset 31.0)
; the tilt of each column in degrees
(def key-col-tilt-deg [0 -5 -10 -10 -18 -28])
; small tweak to column spacing - useful to compensate for large differences
; in tilt between columns
(def key-col-x-tweak [0.0 0.0 0.5 0.0 0.0 0.5])
; simple tweak to slightly shift columns closer to back (pos)/front (neg)
(def key-col-y-tweak [-3.0 -4.0 0.0 0.0 0.0 -2.0])
; which key is the center/lowest point of the circle - can be fractional
(def key-row-center 1.75)

; --- geometry options

(def cap-depth (+ plate-thickness switch-depth))
; angle = row-spaced (arc length) / radius
(def key-col-angle
  (mapv (fn [r] (/ key-row-spaced (- r cap-depth))) key-col-radius))

(defn key-place [col row shape]
  (let [; invert row, so row 0 is at the front (y positive) and row 5 is at the
        ; back (y negative)
        row (- key-row-center row)
        ; for y/z - want to map the linear row position onto a circle/arc around x
        α (* (key-col-angle col) row)
        r (key-col-radius col)
        ; the tilt of each column to create a bowl-like key well
        β (deg2rad (key-col-tilt-deg col))
        ; x is grid spacing, but because of the tilt it may need a tweak
        x-offset (+ (* col key-col-spaced) (key-col-x-tweak col))
        ; y is just the tweak
        y-offset (key-col-y-tweak col)
        ; z is easy, too
        z-offset (- key-col-z-offset (key-col-depth col))]
    (->> shape
         ; map the linear row position onto a circle/arc
         (translate [0.0 0.0 (- r)])
         (rotate α [1 0 0])
         (translate [0.0 0.0 r])
         ; tilt the key
         (translate [0.0 0.0 (- plate-thickness)])
         (rotate β [0 1 0])
         (translate [0.0 0.0 plate-thickness])
         ; add the simple translations
         (translate [x-offset y-offset z-offset]))))

(def center-pos [0.0 0.0 0.0])

(defn key-position [col row position]
  (let [; this  can be used to project a point other than the center
        [x-pos y-pos z-pos] (if (vector? position) position (position col row))
        ; invert row, so row 0 is at the front (y positive) and row 5 is at the
        ; back (y negative)
        row (- key-row-center row)

        ; for y/z - want to map the linear row position onto a circle/arc around x
        α (* (key-col-angle col) row)
        r (key-col-radius col)
        ; this means the rotation is in the x plane
        ; y1 = y0 * cos(angle) - z0 * sin(angle)
        ; z1 = y0 * sin(angle) + z0 * cos(angle)
        ; where y0 = y-pos and z0 = - r + z-pos
        z-pos (- z-pos r)
        cos (Math/cos α)
        sin (Math/sin α)
        y-rot (- (* y-pos cos) (* z-pos sin))
        z-rot (+ (* y-pos sin) (* z-pos cos) r)

        ; the tilt of each column to create a bowl-like key well
        β (deg2rad (key-col-tilt-deg col))
        ; for the tilt, the rotation is around y:
        ; x1 = x0 * cos(angle) - z0 * sin(angle)
        ; z1 = x0 * sin(angle) + z0 * cos(angle)
        ; where x0 = x-pos and z0 from previous
        z-rot (- z-rot plate-thickness)
        cos (Math/cos (- β))
        sin (Math/sin (- β))
        x-rot (- (* x-pos cos) (* z-rot sin))
        z-rot (+ (* x-pos sin) (* z-rot cos) plate-thickness)

        x-offset (+ (* col key-col-spaced) (key-col-x-tweak col))
        y-offset (key-col-y-tweak col)
        z-offset (- key-col-z-offset (key-col-depth col))

        x-global (+ x-offset x-rot)
        y-global (+ y-offset y-rot)
        z-global (+ z-offset z-rot)]
    [x-global y-global z-global]))

(defn key-place-all [shape-fn]
  (apply union
         (for [col (range 0 key-columns)
               row (range 0 key-rows)
               :when (not (key-skip-place? col row))]
           (key-place col row (shape-fn (key-shape-width col row))))))
(def key-plates (key-place-all key-plate-default))
(def key-caps (key-place-all key-cap))
; test plate, with no holes or nubs for easier key removal
(def test-key-plates (key-place-all key-plate-simple))

; the visual key well representation is good for checking clearances
(def keywell-visual (union key-plates key-caps))
(spit "things/keywell-visual.scad" (write-scad keywell-visual))

;;;;;;;;;;;;;;;;;;;
;; Thumb Cluster ;;
;;;;;;;;;;;;;;;;;;;

; --- main options

; the grid spacing
(def thumb-col-spaced (+ keycap-size 0.5))
(def thumb-row-spaced (+ keycap-size 0.5))

(def thumb-columns 3)
(def thumb-rows 2)

(defn thumb-skip-place? [col row] (and (= col 0) (= row 0)))
(defn thumb-shape-height [col row] (if (= row 1) 2.0 1.0))
(defn thumb-z-offset [col row]
  (cond
    (and (= row 0) (> col 0)) 3.0
    (and (= row 1) (= col 2)) 2.0
    :else 0.0))

(def thumb-cluster-offset-z 30.0)
(def thumb-cluster-key-join (key-position (- key-columns 1) (- key-rows 2) center-pos))
(def thumb-cluster-offset
  [(+ (/ keycap-size 2) (thumb-cluster-key-join 0))
   (+ (/ keycap-size -2) (thumb-cluster-key-join 1))
   thumb-cluster-offset-z])
(def thumb-cluster-rotate (deg2rad -20))

; ---

(def thumb-cluster-rotate-sin (Math/sin thumb-cluster-rotate))
(def thumb-cluster-rotate-cos (Math/cos thumb-cluster-rotate))

; the thumb plates run 90 degrees to the ones in the main well. ideally, the
; switch mounts wouldn't be rotated, but that doesn't seem bad enough to fix
(def thumb-shape-rotate (partial rotate (/ pi 2) [0 0 1]))
(defn thumb-plate-default [height] (thumb-shape-rotate (key-plate-default height)))
(defn thumb-plate-simple [height] (thumb-shape-rotate (key-plate-simple height)))
(defn thumb-cap [height] (thumb-shape-rotate (key-cap height)))

(defn thumb-place [col row shape]
  (let [x (* col thumb-col-spaced)
        ; because the heights are variable, need to sum all previous ones
        prev (apply + (for [r (range 0 row)]
                        (* thumb-row-spaced (thumb-shape-height col r))))
        ; also, if the height is larger than 1.0, center it
        diff (- (thumb-shape-height col row) 1.0)
        y (+ prev (/ (* thumb-row-spaced diff) 2))
        z (thumb-z-offset col row)]
    ; invert row, so row 0 is at the front (y positive) and row 2 is at the
    ; back (y negative)
    (->> shape (translate [x (- y) z])
         (rotate thumb-cluster-rotate [0 0 1])
         (translate thumb-cluster-offset))))

(defn thumb-position [col row position]
  (let [; this  can be used to project a point other than the center
        [x-pos y-pos z-pos] (if (vector? position) position (position col row))
        x-local (+ (* col thumb-col-spaced) x-pos)
        ; because the heights are variable, need to sum all previous ones
        y-prev (apply + (for [r (range 0 row)]
                          (* thumb-row-spaced (thumb-shape-height col r))))
        ; also, if the height is larger than 1.0, center it
        diff (- (thumb-shape-height col row) 1.0)
        ; invert row, so row 0 is at the front (y positive) and row 2 is at the
        ; back (y negative)
        y-local (- (+ y-prev (/ (* thumb-row-spaced diff) 2) y-pos))
        z-local (thumb-z-offset col row)
        ; apply cluster rotation manually (around z, in x/y)
        x-rot (- (* x-local thumb-cluster-rotate-cos)
                 (* y-local thumb-cluster-rotate-sin))
        y-rot (+ (* x-local thumb-cluster-rotate-sin)
                 (* y-local thumb-cluster-rotate-cos))
        ; apply cluster offset manually
        [x-offset y-offset z-offset] thumb-cluster-offset
        x-global (+ x-rot x-offset)
        y-global (+ y-rot y-offset)
        z-global (+ z-pos z-local z-offset)]
    [x-global y-global z-global]))

(defn thumb-place-all [shape-fn]
  (apply union
         (for [col (range 0 thumb-columns)
               row (range 0 thumb-rows)
               :when (not (thumb-skip-place? col row))]
           (thumb-place col row (shape-fn (thumb-shape-height col row))))))

(def thumb-plates (thumb-place-all thumb-plate-default))
(def thumb-caps (thumb-place-all thumb-cap))
; test plate, with no holes or nubs for easier key removal
(def test-thumb-plates (thumb-place-all thumb-plate-simple))

; the visual thumb cluster representation is good for checking clearances
(def thumbwell-visual (union thumb-plates thumb-caps))
(spit "things/thumbwell-visual.scad" (write-scad thumbwell-visual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Well Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

; the last col/row isn't filled, as the thumb cluster will join here
(defn key-web-fill [edges]
  [(web-connector-fill-right-front edges 0 (dec key-rows))])

; ---

; pre-calculate all edge points for a single key plate
(defn key-edge [col row]
  (let [keycap-width (* keycap-size (key-shape-width col row))
        w (/ keycap-width 2)
        h (/ keycap-size 2)]
    {:right-back-bottom (key-position col row [w h 0.0])
     :left-back-bottom (key-position col row [(- w) h 0.0])
     :right-front-bottom (key-position col row [w (- h) 0.0])
     :left-front-bottom (key-position col row [(- w) (- h) 0.0])
     :right-back-top (key-position col row [w h plate-thickness])
     :left-back-top (key-position col row [(- w) h plate-thickness])
     :right-front-top (key-position col row [w (- h) plate-thickness])
     :left-front-top (key-position col row [(- w) (- h) plate-thickness])}))

; pre-calculate all edge points for all key plates
(def key-edges
  (apply merge (for [col (range 0 key-columns)
                     row (range 0 key-rows)
                     :when (not (key-skip-place? col row))]
                 {[col row] (key-edge col row)})))

; re-define this now that the edges have been calculated
(def key-web-fill (key-web-fill key-edges))

(def key-web-connectors
  (web-connectors key-columns key-rows key-edges key-skip-place? key-web-fill))

; the key well tester is good for checking the geometry by printing
(def keywell-tester (union test-key-plates key-web-connectors))
(spit "things/keywell-tester.scad" (write-scad keywell-tester))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thumb Cluster Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; pre-calculate all edge points for a single thumb plate
(defn thumb-edge [col row]
  (let [keycap-height (* keycap-size (thumb-shape-height col row))
        w (/ keycap-size 2)
        h (/ keycap-height 2)]
    {:right-back-bottom (thumb-position col row [w (- h) 0.0])
     :left-back-bottom (thumb-position col row [(- w) (- h) 0.0])
     :right-front-bottom (thumb-position col row [w h 0.0])
     :left-front-bottom (thumb-position col row [(- w) h 0.0])
     :right-back-top (thumb-position col row [w (- h) plate-thickness])
     :left-back-top (thumb-position col row [(- w) (- h) plate-thickness])
     :right-front-top (thumb-position col row [w h plate-thickness])
     :left-front-top (thumb-position col row [(- w) h plate-thickness])}))

; pre-calculate all edge points for all thumb plates
(def thumb-edges
  (apply merge (for [col (range 0 thumb-columns)
                     row (range 0 thumb-rows)
                     :when (not (thumb-skip-place? col row))]
                 {[col row] (thumb-edge col row)})))

(def thumb-web-connectors
  (web-connectors thumb-columns thumb-rows thumb-edges thumb-skip-place? []))

; the thumb well tester is good for checking the geometry by printing, but since
; the geometry is so simple, this probably isn't needed
(def thumbwell-tester (union test-thumb-plates thumb-web-connectors))
(spit "things/thumbwell-tester.scad" (write-scad thumbwell-tester))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Join Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def join-web-connectors
  (let [last-col (dec key-columns)
        last-row (dec key-rows)
        key-right-back (key-edges [last-col (dec last-row)])
        key-left-front (key-edges [(dec last-col) last-row])
        thumb-right-back (thumb-edges [1 0])
        thumb-left-front (thumb-edges [0 1])
        key-right-back2 (key-edges [last-col (- last-row 2)])
        key-left2-front (key-edges [(- last-col 2) last-row])]
    (union
     (web-tri (key-right-back2 :right-front-bottom)
              (thumb-right-back :left-back-bottom)
              (key-right-back :right-back-bottom)
              (key-right-back2 :right-front-top)
              (thumb-right-back :left-back-top)
              (key-right-back :right-back-top))
     (web-tri (key-right-back :right-back-bottom)
              (thumb-right-back :left-back-bottom)
              (key-right-back :right-front-bottom)
              (key-right-back :right-back-top)
              (thumb-right-back :left-back-top)
              (key-right-back :right-front-top))
     (web-tri (key-right-back :right-front-bottom)
              (thumb-right-back :left-back-bottom)
              (thumb-right-back :left-front-bottom)
              (key-right-back :right-front-top)
              (thumb-right-back :left-back-top)
              (thumb-right-back :left-front-top))
     (web-tri (key-right-back :right-front-bottom)
              (thumb-right-back :left-front-bottom)
              (thumb-left-front :right-back-bottom)
              (key-right-back :right-front-top)
              (thumb-right-back :left-front-top)
              (thumb-left-front :right-back-top))
     (web-tri (key-right-back :right-front-bottom)
              (thumb-left-front :right-back-bottom)
              (thumb-left-front :left-back-bottom)
              (key-right-back :right-front-top)
              (thumb-left-front :right-back-top)
              (thumb-left-front :left-back-top))
     ; smoothing polyhedron
     (polyhedron [(key-right-back :right-front-top)
                  (thumb-left-front :right-back-top)
                  (thumb-left-front :left-back-top)
                  (thumb-right-back :left-front-top)]
                 [[0 2 1]
                  [0 3 2]
                  [0 1 3]
                  [2 3 1]])
     (web-tri (key-right-back :left-front-bottom)
              (key-right-back :right-front-bottom)
              (thumb-left-front :left-back-bottom)
              (key-right-back :left-front-top)
              (key-right-back :right-front-top)
              (thumb-left-front :left-back-top))
     (web-tri (key-right-back :left-front-bottom)
              (thumb-left-front :left-back-bottom)
              (key-left-front :right-back-bottom)
              (key-right-back :left-front-top)
              (thumb-left-front :left-back-top)
              (key-left-front :right-back-top))
     (web-tri (key-left-front :right-back-bottom)
              (thumb-left-front :left-back-bottom)
              (key-left-front :right-front-bottom)
              (key-left-front :right-back-top)
              (thumb-left-front :left-back-top)
              (key-left-front :right-front-top))
     (web-tri (key-left-front :right-front-bottom)
              (thumb-left-front :left-back-bottom)
              (thumb-left-front :left-front-bottom)
              (key-left-front :right-front-top)
              (thumb-left-front :left-back-top)
              (thumb-left-front :left-front-top))
     (web-tri (key-left-front :right-front-bottom)
              (thumb-left-front :left-front-bottom)
              (key-left-front :left-front-bottom)
              (key-left-front :right-front-top)
              (thumb-left-front :left-front-top)
              (key-left-front :left-front-top))
     ; this contains a devilish edge due to the extreme key z difference...
     (web-tri (key-left-front :left-front-bottom)
              (thumb-left-front :left-front-bottom)
              (key-left2-front :right-front-bottom)
              (key-left-front :left-front-top)
              (thumb-left-front :left-front-top)
              (key-left2-front :right-front-top))
     ; ...this polyhedron smooths/fills this
     (polyhedron [(key-left-front :left-front-top)
                  (key-left-front :right-front-top)
                  (thumb-left-front :left-front-top)
                  (key-left2-front :right-front-top)]
                 [[0 2 1]
                  [1 2 3]
                  [3 2 0]
                  [0 1 3]]))))

; ---


(spit "things/left.scad"
      (write-scad (union key-plates
                         key-web-connectors
                         thumb-plates
                         thumb-web-connectors
                         join-web-connectors
                         ; key-walls
                         ; thumb-walls
                         ; join-walls
                         )))
