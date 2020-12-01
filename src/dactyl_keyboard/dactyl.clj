(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.holders :refer :all]
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
(def key-col-z-offset 30.0)
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

(defn center-pos [col row] [0.0 0.0 0.0])

(defn key-position [col row position-fn]
  (let [; this  can be used to project a point other than the center
        [x-pos y-pos z-pos] (position-fn col row)
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

(defn key-project [col row position-fn]
  (let [[x y z] (key-position col row position-fn)]
    [x y 0.0]))

(defn key-edge-tr [col row]
  (let [keycap-width (* keycap-size (key-shape-width col row))
        x (/ keycap-width 2)
        y (/ keycap-size 2)]
    [x y 0.0]))
(defn key-edge-tl [col row]
  (let [keycap-width (* keycap-size (key-shape-width col row))
        x (/ keycap-width -2)
        y (/ keycap-size 2)]
    [x y 0.0]))
(defn key-edge-bl [col row]
  (let [keycap-width (* keycap-size (key-shape-width col row))
        x (/ keycap-width -2)
        y (/ keycap-size -2)]
    [x y 0.0]))
(defn key-edge-br [col row]
  (let [keycap-width (* keycap-size (key-shape-width col row))
        x (/ keycap-width 2)
        y (/ keycap-size -2)]
    [x y 0.0]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Well Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

; see skipped-keys-fill below - i can't move it up here because of where the
; function definitions are

; --- geometry options (shouldn't need changing)

(def web-thickness plate-thickness)
(def post-size 0.1)
(defn web-post-fn [x y]
  (let [shape (cube post-size post-size web-thickness)
        z (+ (/ web-thickness -2) plate-thickness)]
    (translate [x y z] shape)))

(def post-adj (/ post-size 2))
(def web-post-tr (web-post-fn (- post-adj) (- post-adj)))
(def web-post-tl (web-post-fn post-adj (- post-adj)))
(def web-post-bl (web-post-fn post-adj post-adj))
(def web-post-br (web-post-fn (- post-adj) post-adj))

; ---

; most of these functions can take a different shape. this seems unnecessary
; now, but i had planned to also generate support material

(defn key-web-post-tr [col row] (translate (key-edge-tr col row) web-post-tr))
(defn key-web-post-tl [col row] (translate (key-edge-tl col row) web-post-tl))
(defn key-web-post-bl [col row] (translate (key-edge-bl col row) web-post-bl))
(defn key-web-post-br [col row] (translate (key-edge-br col row) web-post-br))

(defn key-edge-place [col row shape-fn] (key-place col row (shape-fn col row)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

; triangle in top-right (actually 3 triangles, to fill plate and connectors)
; 2 +--+ 3
;    \ |
;     \|
;      * 1
(defn key-triangle-tr [col row]
  (let [up-row (dec row) right-col (inc col)]
    (union
     (triangle-hulls ; triangle to fill plate
      (key-edge-place right-col row key-web-post-bl)
      (key-edge-place col up-row key-web-post-bl)
      (key-edge-place col row key-web-post-tr))
     (triangle-hulls ; triangle to fill top connector
      (key-edge-place col up-row key-web-post-bl)
      (key-edge-place col up-row key-web-post-br)
      (key-edge-place col row key-web-post-tr))
     (triangle-hulls ; triangle to fill right connector
      (key-edge-place right-col row key-web-post-tl)
      (key-edge-place right-col row key-web-post-bl)
      (key-edge-place col row key-web-post-tr)))))

; triangle in top-left (actually 3 triangles, to fill plate and connectors)
; 3 +--+ 1
;   | /
;   |/
; 2 *
(defn key-triangle-tl [col row]
  (let [up-row (dec row) left-col (dec col)]
    (union
     (triangle-hulls ; triangle to fill plate
      (key-edge-place col up-row key-web-post-br)
      (key-edge-place left-col row key-web-post-br)
      (key-edge-place col row key-web-post-tl))
     (triangle-hulls ; triangle to fill left connector
      (key-edge-place left-col row key-web-post-br)
      (key-edge-place left-col row key-web-post-tr)
      (key-edge-place col row key-web-post-tl))
     (triangle-hulls ; triangle to fill top connector
      (key-edge-place col up-row key-web-post-br)
      (key-edge-place col up-row key-web-post-bl)
      (key-edge-place col row key-web-post-tl)))))

; triangle in bottom-right (actually two triangles, to fill plate and connector)
;      * 2
;     /|
;    / |
; 1 +--+ 3
(defn key-triangle-br [col row]
  (let [down-row (inc row) right-col (inc col)]
    (union
     (triangle-hulls ; triangle to fill plate
      (key-edge-place col down-row key-web-post-tl)
      (key-edge-place right-col row key-web-post-tl)
      (key-edge-place col row key-web-post-br))
     (triangle-hulls ; triangle to fill right connector
      (key-edge-place right-col row key-web-post-tl)
      (key-edge-place right-col row key-web-post-bl)
      (key-edge-place col row key-web-post-br))
     (triangle-hulls ; triangle to fill bottom connector
      (key-edge-place col down-row key-web-post-tr)
      (key-edge-place col down-row key-web-post-tl)
      (key-edge-place col row key-web-post-br)))))

; triangle in bottom-left (actually two triangles, to fill plate and connector)
; *
; |\
; | \
; +--+
(defn key-triangle-bl [col row]
  (let [down-row (inc row) left-col (dec col)]
    (union
     (triangle-hulls ; triangle to fill plate
      (key-edge-place left-col row key-web-post-tr)
      (key-edge-place col down-row key-web-post-tr)
      (key-edge-place col row key-web-post-bl))
     (triangle-hulls ; triangle to fill left connector
      (key-edge-place left-col row key-web-post-br)
      (key-edge-place left-col row key-web-post-tr)
      (key-edge-place col row key-web-post-bl))
     (triangle-hulls ; triangle to fill top connector
      (key-edge-place col down-row key-web-post-tr)
      (key-edge-place col down-row key-web-post-tl)
      (key-edge-place col row key-web-post-bl)))))

; --- main options

; fill geometry for skipped keys (e.g. corners if desired). also see loop for
; diagonal connections below, and skip those if no fill geometry is desired.
; the skipped key for the thumb cluster connection shouldn't be filled.
(def key-skipped-fill
  [(key-triangle-tr 0 (dec key-rows))])

; ---

(def key-web-connectors
  (apply union
         (concat
          ; row connections
          (for [col (drop-last (range 0 key-columns))
                row (range 0 key-rows)
                :when (not (or (key-skip-place? col row)
                               (key-skip-place? (inc col) row)))]
            (triangle-hulls
             (key-edge-place (inc col) row key-web-post-tl)
             (key-edge-place col row key-web-post-tr)
             (key-edge-place (inc col) row key-web-post-bl)
             (key-edge-place col row key-web-post-br)))
          ; column connections
          (for [col (range 0 key-columns)
                row (drop-last (range 0 key-rows))
                :when (not (or (key-skip-place? col row)
                               (key-skip-place? col (inc row))))]
            (triangle-hulls
             (key-edge-place col row key-web-post-bl)
             (key-edge-place col row key-web-post-br)
             (key-edge-place col (inc row) key-web-post-tl)
             (key-edge-place col (inc row) key-web-post-tr)))
          ; diagonal connections
          (for [col (drop-last (range 0 key-columns))
                row (drop-last (range 0 key-rows))]
                ; if skipped keys aren't filled, these can be left off
                ; :when (not (or (key-skip-place? col row)
                ;                (key-skip-place? col (inc row))
                ;                (key-skip-place? (inc col) row)
                ;                (key-skip-place? (inc col) (inc row))))]
            (triangle-hulls
             (key-edge-place col row key-web-post-br)
             (key-edge-place col (inc row) key-web-post-tr)
             (key-edge-place (inc col) row key-web-post-bl)
             (key-edge-place (inc col) (inc row) key-web-post-tl)))
          ; geometry to fill skipped keys
          key-skipped-fill)))


; the key well tester is good for checking the geometry by printing


(def keywell-tester (union test-key-plates key-web-connectors))
(spit "things/keywell-tester.scad" (write-scad keywell-tester))

;;;;;;;;;;;;;;;;;;;;
;; Key Well Walls ;;
;;;;;;;;;;;;;;;;;;;;

; --- main options

; how far the walls are extruded from the key well (in x and y)
(def wall-extrude 2.0)

; length of the first downward-sloping part of the wall
(def wall-z-depth -8.0)

; ---

(defn bottom-hull [& p]
  (hull p (extrude-linear {:height 0.001 :twist 0 :convexity 0} (project p))))

(defn wall-locate1 [dcol drow]
  [(* dcol wall-thickness)
   (* drow wall-thickness)
   -1.0])
(defn wall-locate2 [dcol drow]
  [(* dcol wall-extrude)
   (* drow wall-extrude)
   wall-z-depth])
(defn wall-locate3 [dcol drow]
  [(* dcol (+ wall-extrude wall-thickness))
   (* drow (+ wall-extrude wall-thickness))
   wall-z-depth])

(defn wall-brace [place1 dcol1 drow1 post1 place2 dcol2 drow2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dcol1 drow1) post1))
    (place1 (translate (wall-locate2 dcol1 drow1) post1))
    (place1 (translate (wall-locate3 dcol1 drow1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dcol2 drow2) post2))
    (place2 (translate (wall-locate2 dcol2 drow2) post2))
    (place2 (translate (wall-locate3 dcol2 drow2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dcol1 drow1) post1))
    (place1 (translate (wall-locate3 dcol1 drow1) post1))
    (place2 (translate (wall-locate2 dcol2 drow2) post2))
    (place2 (translate (wall-locate3 dcol2 drow2) post2)))))

(defn key-wall-brace [col1 row1 dcol1 drow1 post1 col2 row2 dcol2 drow2 post2]
  (wall-brace (partial key-place col1 row1) dcol1 drow1 (post1 col1 row1)
              (partial key-place col2 row2) dcol2 drow2 (post2 col2 row2)))

(def key-walls
  (let [lastcol (dec key-columns) lastrow (dec key-rows)]
    (union
     ; right wall keys
     ; skip last row (thumb corner)
     (for [row (range 0 (dec lastrow))]
       (key-wall-brace lastcol row       1 0 key-web-post-tr lastcol row 1 0 key-web-post-br))
     ; right wall web
     (for [row (range 1 lastrow)]
       (key-wall-brace lastcol (dec row) 1 0 key-web-post-br lastcol row 1 0 key-web-post-tr))
     ; back-right corner
     (key-wall-brace lastcol 0 0 1 key-web-post-tr lastcol 0 1 0 key-web-post-tr)
     ; left wall keys
     (for [row (range 0 lastrow)]
       (key-wall-brace 0 row       -1 0 key-web-post-tl 0 row -1 0 key-web-post-bl))
     ; left wall web
     (for [row (range 1 lastrow)]
       (key-wall-brace 0 (dec row) -1 0 key-web-post-bl 0 row -1 0 key-web-post-tl))
     ; back-left corner
     (key-wall-brace 0 0 0 1 key-web-post-tl 0 0 -1 0 key-web-post-tl)
     ; back wall keys
     (for [col (range 0 key-columns)]
       (key-wall-brace col 0 0 1 key-web-post-tl col       0 0 1 key-web-post-tr))
     ; back wall web
     (for [col (range 1 key-columns)]
       (key-wall-brace col 0 0 1 key-web-post-tl (dec col) 0 0 1 key-web-post-tr))
     ; front wall keys
     ; skip first col (pinky corner) and last col (thumb corner)
     (for [col (range 1 (dec lastcol))]
       (key-wall-brace col lastrow 0 -1 key-web-post-bl col       lastrow 0 -1 key-web-post-br))
     ; front wall web
     ; skip first col (pinky corner) and last col (thumb corner)
     (for [col (range 2 lastcol)]
       (key-wall-brace col lastrow 0 -1 key-web-post-bl (dec col) lastrow 0 -1 key-web-post-br))
     ; front-left corner
     ; spans across the missing pinky corner
     (key-wall-brace 0 (dec lastrow) -1 0 key-web-post-bl 1 lastrow 0 -1 key-web-post-bl)
     ; no front-right corner; that's where the thumb cluster will join
     )))

;;;;;;;;;;;;;;;;;;;;;;;;
;; USB Holder Cut-out ;;
;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

(def holder-x-offset 0.0)
(def holder-y-offset (+ (/ keycap-size 2) (/ wall-extrude 2) wall-thickness))

; ---

(def usb-holder-wall-translate
  (->> usb-holder-wall
       (translate (key-project (- key-columns 2) 0 center-pos))
       (translate [holder-x-offset holder-y-offset 0.0])))
(def usb-holder-slot-translate
  (->> usb-holder-slot
       (translate (key-project (- key-columns 2) 0 center-pos))
       (translate [holder-x-offset holder-y-offset 0.0])))

(def key-walls (difference (union key-walls usb-holder-wall-translate)
                           usb-holder-slot-translate))

(def keywall-visual (union key-plates key-caps key-web-connectors key-walls))
(spit "things/keywall-visual.scad" (write-scad keywall-visual))

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

(def thumb-cluster-offset
  (mapv +
        (key-project (dec key-columns) (- key-rows 2) center-pos)
        [(/ keycap-size 2) (/ keycap-size -2) 30.0]))
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

(defn thumb-position [col row position-fn]
  (let [; this  can be used to project a point other than the center
        [x-pos y-pos z-pos] (position-fn col row)
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

(defn thumb-project [col row position-fn]
  (let [[x y z] (thumb-position col row position-fn)]
    [x y 0.0]))

(defn thumb-edge-tr [col row]
  (let [keycap-height (* keycap-size (thumb-shape-height col row))
        x (/ keycap-size 2)
        y (/ keycap-height 2)]
    [x y 0.0]))
(defn thumb-edge-tl [col row]
  (let [keycap-height (* keycap-size (thumb-shape-height col row))
        x (/ keycap-size -2)
        y (/ keycap-height 2)]
    [x y 0.0]))
(defn thumb-edge-bl [col row]
  (let [keycap-height (* keycap-size (thumb-shape-height col row))
        x (/ keycap-size -2)
        y (/ keycap-height -2)]
    [x y 0.0]))
(defn thumb-edge-br [col row]
  (let [keycap-height (* keycap-size (thumb-shape-height col row))
        x (/ keycap-size 2)
        y (/ keycap-height -2)]
    [x y 0.0]))

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
(spit "things/thumbwell-visual.scad"
      (write-scad (union keywell-visual thumbwell-visual)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thumb Cluster Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; most of these functions can take a different shape. this seems unnecessary
; now, but i had planned to also generate support material


(defn thumb-web-post-tr [col row] (translate (thumb-edge-tr col row) web-post-tr))
(defn thumb-web-post-tl [col row] (translate (thumb-edge-tl col row) web-post-tl))
(defn thumb-web-post-bl [col row] (translate (thumb-edge-bl col row) web-post-bl))
(defn thumb-web-post-br [col row] (translate (thumb-edge-br col row) web-post-br))

(defn thumb-edge-place [col row shape-fn] (thumb-place col row (shape-fn col row)))

; ---

(def thumb-web-connectors
  (apply union
         (concat
          ; row connections
          (for [col (drop-last (range 0 thumb-columns))
                row (range 0 thumb-rows)
                :when (not (or (thumb-skip-place? col row)
                               (thumb-skip-place? (inc col) row)))]
            (triangle-hulls
             (thumb-edge-place (inc col) row thumb-web-post-tl)
             (thumb-edge-place col row thumb-web-post-tr)
             (thumb-edge-place (inc col) row thumb-web-post-bl)
             (thumb-edge-place col row thumb-web-post-br)))
          ; column connections
          (for [col (range 0 thumb-columns)
                row (drop-last (range 0 thumb-rows))
                :when (not (or (thumb-skip-place? col row)
                               (thumb-skip-place? col (inc row))))]
            (triangle-hulls
             (thumb-edge-place col row thumb-web-post-bl)
             (thumb-edge-place col row thumb-web-post-br)
             (thumb-edge-place col (inc row) thumb-web-post-tl)
             (thumb-edge-place col (inc row) thumb-web-post-tr)))
          ; diagonal connections
          (for [col (drop-last (range 0 thumb-columns))
                row (drop-last (range 0 thumb-rows))
                :when (not (thumb-skip-place? col row))]
            (triangle-hulls
             (thumb-edge-place col row thumb-web-post-br)
             (thumb-edge-place col (inc row) thumb-web-post-tr)
             (thumb-edge-place (inc col) row thumb-web-post-bl)
             (thumb-edge-place (inc col) (inc row) thumb-web-post-tl))))))


; the thumb well tester is good for checking the geometry by printing, but since
; the geometry is so simple, this probably isn't needed


(def thumbwell-tester (union test-thumb-plates thumb-web-connectors))
(spit "things/thumbwell-tester.scad" (write-scad thumbwell-tester))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thumb Cluster Walls ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; ---

(defn thumb-wall-brace [col1 row1 dcol1 drow1 post1 col2 row2 dcol2 drow2 post2]
  (wall-brace (partial thumb-place col1 row1) dcol1 drow1 (post1 col1 row1)
              (partial thumb-place col2 row2) dcol2 drow2 (post2 col2 row2)))

(def thumb-walls
  (let [lastcol (dec thumb-columns) lastrow (dec thumb-rows)]
    (union
     ; right wall keys
     (for [row (range 0 thumb-rows)]
       (thumb-wall-brace lastcol row       1 0 thumb-web-post-tr lastcol row 1 0 thumb-web-post-br))
     ; right wall web
     (for [row (range 1 thumb-rows)]
       (thumb-wall-brace lastcol (dec row) 1 0 thumb-web-post-br lastcol row 1 0 thumb-web-post-tr))
     ; back-right corner
     (thumb-wall-brace lastcol 0 0 1 thumb-web-post-tr lastcol 0 1 0 thumb-web-post-tr)
     ; left wall keys
     ; skip missing key and one more to join up with key walls
     (for [row (range 2 thumb-rows)]
       (thumb-wall-brace 0 row       -1 0 thumb-web-post-tl 0 row -1 0 thumb-web-post-bl))
     ; left wall web
     ; skip missing key and one more to join up with key walls
     (for [row (range 2 thumb-rows)]
       (thumb-wall-brace 0 (dec row) -1 0 thumb-web-post-bl 0 row -1 0 thumb-web-post-tl))
     ; no back-left corner; that's where it joins up to the key walls
     ; back wall keys
     (for [col (range 1 thumb-columns)]
       (thumb-wall-brace col 0 0 1 thumb-web-post-tl col       0 0 1 thumb-web-post-tr))
     ; back wall web
     ; skip top corner
     (for [col (range 2 thumb-columns)]
       (thumb-wall-brace col 0 0 1 thumb-web-post-tl (dec col) 0 0 1 thumb-web-post-tr))
     ; front wall keys
     (for [col (range 0 thumb-columns)]
       (thumb-wall-brace col lastrow 0 -1 thumb-web-post-bl col       lastrow 0 -1 thumb-web-post-br))
     ; front wall web
     (for [col (range 1 thumb-columns)]
       (thumb-wall-brace col lastrow 0 -1 thumb-web-post-bl (dec col) lastrow 0 -1 thumb-web-post-br))
     ; front-left corner
     ; (thumb-wall-brace 0 lastrow -1 0 thumb-web-post-bl 0 lastrow 0 -1 thumb-web-post-bl)
     ; front-right corner
     (thumb-wall-brace lastcol lastrow 1 0 thumb-web-post-br lastcol lastrow 0 -1 thumb-web-post-br))))

(def join-walls
  (let [last-col (dec key-columns)
        front-col (dec last-col)
        last-row (dec key-rows)
        back-row (dec last-row)]
    (union
         ; front
     (wall-brace (partial key-place front-col last-row)
                 0
                 -1
                 (key-web-post-bl front-col last-row)
                 (partial thumb-place 0 (inc 0))
                 0
                 -1
                 (thumb-web-post-bl 0 (inc 0)))
         ; back
     (wall-brace (partial key-place last-col back-row)
                 1
                 0
                 (key-web-post-tr last-col back-row)
                 (partial thumb-place (inc 0) 0)
                 0
                 1
                 (thumb-web-post-tl (inc 0) 0))
         ; key to thumb... ugh
     (triangle-hulls
      (key-edge-place last-col back-row key-web-post-tr)
      (thumb-edge-place (inc 0) 0 thumb-web-post-tl)
      (key-edge-place last-col back-row key-web-post-br))
     (triangle-hulls
      (key-edge-place last-col back-row key-web-post-br)
      (thumb-edge-place (inc 0) 0 thumb-web-post-tl)
      (thumb-edge-place (inc 0) 0 thumb-web-post-bl))
     (triangle-hulls
      (key-edge-place last-col back-row key-web-post-br)
      (thumb-edge-place (inc 0) 0 thumb-web-post-bl)
      (thumb-edge-place (inc 0) (inc 0) thumb-web-post-tl))
     (triangle-hulls
      (key-edge-place last-col back-row key-web-post-br)
      (thumb-edge-place (inc 0) (inc 0) thumb-web-post-tl)
      (thumb-edge-place 0 (inc 0) thumb-web-post-tr))
     (triangle-hulls
      (key-edge-place last-col back-row key-web-post-br)
      (thumb-edge-place 0 (inc 0) thumb-web-post-tr)
      (thumb-edge-place 0 (inc 0) thumb-web-post-tl))
     (triangle-hulls
      (key-edge-place last-col back-row key-web-post-bl)
      (key-edge-place last-col back-row key-web-post-br)
      (thumb-edge-place 0 (inc 0) thumb-web-post-tl))
     (triangle-hulls
      (key-edge-place last-col back-row key-web-post-bl)
      (thumb-edge-place 0 (inc 0) thumb-web-post-tl)
      (key-edge-place last-col last-row key-web-post-tl))
     (triangle-hulls
      (key-edge-place front-col last-row key-web-post-tr)
      (key-edge-place last-col last-row key-web-post-tl)
      (key-edge-place front-col last-row key-web-post-br))
     (triangle-hulls
      (key-edge-place last-col last-row key-web-post-tl)
      (thumb-edge-place 0 (inc 0) thumb-web-post-tl)
      (key-edge-place front-col last-row key-web-post-br))
     (triangle-hulls
      (key-edge-place front-col last-row key-web-post-br)
      (thumb-edge-place 0 (inc 0) thumb-web-post-tl)
      (thumb-edge-place 0 (inc 0) thumb-web-post-bl))
     (triangle-hulls
      (key-edge-place front-col last-row key-web-post-br)
      (thumb-edge-place 0 (inc 0) thumb-web-post-bl)
      (key-edge-place front-col last-row key-web-post-bl)))))

(def thumbwall-visual
  (union thumb-plates thumb-caps thumb-web-connectors thumb-walls))
(spit "things/thumbwall-visual.scad" (write-scad (union keywall-visual thumbwall-visual join-walls)))
