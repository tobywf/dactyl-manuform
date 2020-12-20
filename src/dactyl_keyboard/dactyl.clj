(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [dactyl-keyboard.holders :refer :all]
            [dactyl-keyboard.web :refer :all]
            [scad-clj.scad :refer :all]
            [scad-clj.model
             :refer :all
             :rename {cylinder cylinder-base
                      sphere sphere-base
                      circle circle-base}]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(defn cylinder [fn rs h & {:keys [center] :or {center true}}]
  (with-fn fn (cylinder-base rs h :center center)))
(defn sphere [fn r] (with-fn fn (sphere-base r)))
(defn circle [fn r] (with-fn fn (circle-base r)))

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
(def switch-hole-border 2.35)
(def keycap-size (+ switch-hole-size (* switch-hole-border 2)))
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
        top-wall (->> (cube keycap-size switch-hole-border plate-thickness)
                      (translate [0.0
                                  (+ (/ switch-hole-border 2) (/ switch-hole-size 2))
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
        left-wall-width (+ switch-hole-border plate-extra-width)
        ; wall with optional side nub
        left-wall (->> (cube left-wall-width
                             (+ switch-hole-size (* switch-hole-border 2))
                             plate-thickness)
                       (translate [(+ (/ left-wall-width 2) (/ switch-hole-size 2))
                                   0.0
                                   (/ plate-thickness 2)]))
        ; side nub (wedge and rounded bottom)
        side-nub (->> (cylinder 30 1.0 2.75)
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
; (make sure these are never exactly 0; important for wall geometry construction)
(def key-col-tilt-deg [1 -5 -10 -10 -18 -28])
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
(def thumb-cluster-rotate-angle (deg2rad -20))

; ---

(def thumb-cluster-rotate-sin (Math/sin thumb-cluster-rotate-angle))
(def thumb-cluster-rotate-cos (Math/cos thumb-cluster-rotate-angle))

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
         (rotate thumb-cluster-rotate-angle [0 0 1])
         (translate thumb-cluster-offset))))

(defn thumb-cluster-rotate [position]
  (let [[x-pos y-pos z-pos] position
        x-rot (- (* x-pos thumb-cluster-rotate-cos)
                 (* y-pos thumb-cluster-rotate-sin))
        y-rot (+ (* x-pos thumb-cluster-rotate-sin)
                 (* y-pos thumb-cluster-rotate-cos))]
    [x-rot y-rot z-pos]))

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
        [x-rot y-rot _] (thumb-cluster-rotate [x-local y-local 0.0])
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
(spit "things/thumbwell-visual.scad"
      (write-scad (union keywell-visual thumbwell-visual)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key/Thumb Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    {:right-back {:bottom (key-position col row [w h 0.0])
                  :top (key-position col row [w h plate-thickness])}
     :left-back {:bottom (key-position col row [(- w) h 0.0])
                 :top (key-position col row [(- w) h plate-thickness])}
     :right-front {:bottom (key-position col row [w (- h) 0.0])
                   :top (key-position col row [w (- h) plate-thickness])}
     :left-front {:bottom (key-position col row [(- w) (- h) 0.0])
                  :top (key-position col row [(- w) (- h) plate-thickness])}}))

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

; pre-calculate all edge points for a single thumb plate
(defn thumb-edge [col row]
  (let [keycap-height (* keycap-size (thumb-shape-height col row))
        w (/ keycap-size 2)
        h (/ keycap-height 2)]
    {:right-back {:bottom (thumb-position col row [w (- h) 0.0])
                  :top (thumb-position col row [w (- h) plate-thickness])}
     :left-back {:bottom (thumb-position col row [(- w) (- h) 0.0])
                 :top (thumb-position col row [(- w) (- h) plate-thickness])}
     :right-front {:bottom (thumb-position col row [w h 0.0])
                   :top (thumb-position col row [w h plate-thickness])}
     :left-front {:bottom (thumb-position col row [(- w) h 0.0])
                  :top (thumb-position col row [(- w) h plate-thickness])}}))

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

;;;;;;;;;;;;;;;;;;;;;
;; Key/Thumb Walls ;;
;;;;;;;;;;;;;;;;;;;;;

; --- main options

(def wall-thickness 6.0)

; ---

(defmacro quote-points [& points]
  {:keywords (mapv keyword points) :values (mapv identity points)})

(defn nice-poly [quoted-points faces]
  (let [kw-to-index (into {} (map-indexed (fn [index item] {item index})
                                          (quoted-points :keywords)))
        points (quoted-points :values)
        faces (mapv (fn [face] (mapv kw-to-index face)) faces)]
    (polyhedron points faces)))

(defn displace [point dir amount]
  (let [coords (case dir
                 :left [(- amount) 0.0 0.0]
                 :right [amount 0.0 0.0]
                 :front [0.0 (- amount) 0.0]
                 :back [0.0 amount 0.0])]
    (mapv + point coords)))

(defn vertical-align [bottom top dir]
  (let [[bx by bz] bottom
        [tx ty tz] top
        ; cheat: invent an "alignment" for straight edges, so geometry works
        cheat (displace top dir 0.1)]
    (case dir
      :left (cond (> bx tx) (throw (Exception. "Can't valign convex edge (left)"))
                  (< bx tx) [bx ty (+ bz plate-thickness)]
                  :equal cheat)
      :right (cond (< bx tx) (throw (Exception. "Can't valign convex edge (right)"))
                   (> bx tx) [bx ty (+ bz plate-thickness)]
                   :equal cheat)
      :front (cond (> by ty) (throw (Exception. "Can't valign convex edge (front)"))
                   (< by ty) [tx by (+ bz plate-thickness)]
                   :equal cheat)
      :back (cond (< by ty) (throw (Exception. "Can't valign convex edge (back)"))
                  (> by ty) [tx by (+ bz plate-thickness)]
                  :equal cheat))))

(defn project-z [point] [(point 0) (point 1) 0.0])

(defn construct-wall [edge0 dir0 edge1 dir1]
  (let [b0 (edge0 :bottom)
        t0 (edge0 :top)
        b1 (edge1 :bottom)
        t1 (edge1 :top)
        ; valign
        v0 (vertical-align b0 t0 dir0)
        v1 (vertical-align b1 t1 dir1)
        ; chamfer/extrude
        c0 (displace b0 dir0 wall-thickness)
        c1 (displace b1 dir1 wall-thickness)
        ; project floor
        pb0 (project-z b0)
        pc0 (project-z c0)
        pb1 (project-z b1)
        pc1 (project-z c1)
        points (quote-points b0 b1 t0 t1 v0 v1 c0 c1 pb0 pb1 pc0 pc1)]
    (nice-poly points
               [[:b0 :c0 :v0 :t0] ; edge0 valign/chamfer
                [:b1 :t1 :v1 :c1] ; edge1 valign/chamfer
                [:b0 :t0 :t1 :b1] ; edge0/edge1 face
                [:t0 :v0 :v1 :t1] ; valign top
                [:c0 :c1 :v1 :v0] ; chamfer out
                [:b0 :b1 :pb1 :pb0] ; edge0/edge1 project
                [:b0 :pb0 :pc0 :c0] ; edge0 project
                [:b1 :c1 :pc1 :pb1] ; edge1 project
                [:c1 :c0 :pc0 :pc1] ; chamfer project
                [:pb0 :pb1 :pc1 :pc0]])))

(defn construct-corner [edge0 dir0 dir1]
  (let [b0 (edge0 :bottom)
        t0 (edge0 :top)
        ; valign
        v0 (vertical-align b0 t0 dir0)
        v1 (vertical-align b0 t0 dir1)
        ; chamfer/extrude
        c0 (displace b0 dir0 wall-thickness)
        c1 (displace b0 dir1 wall-thickness)
        ; project floor
        pb0 (project-z b0)
        pc0 (project-z c0)
        pc1 (project-z c1)
        points (quote-points b0 t0 v0 v1 c0 c1 pb0 pc0 pc1)]
    (nice-poly points
               [[:b0 :c0 :v0 :t0] ; dir0 valign/chamfer wall
                [:b0 :t0 :v1 :c1] ; dir1 valign/chamfer wall
                [:t0 :v0 :v1] ; valign top
                [:v1 :v0 :c0 :c1] ; valign/chamfer out
                [:c0 :b0 :pb0 :pc0] ; dir0 project wall
                [:b0 :c1 :pc1 :pb0] ; dir1 project wall
                [:c1 :c0 :pc0 :pc1] ; project out
                [:pc1 :pc0 :pb0]])))

(def key-walls
  (let [firstcol 0
        firstrow 0
        lastcol (dec key-columns)
        lastrow (dec key-rows)]
    (union
     ; outer walls (left, at pinky keys)
     ; skip last row (pinky corner)
     (for [row (range 0 (- key-rows 1))]
       (construct-wall ((key-edges [firstcol row]) :left-front) :left
                       ((key-edges [firstcol row]) :left-back) :left))
     ; outer webs (connect from back to front, current to next)
     ; skip last row (skipped pinky corner will fill this)
     (for [row (range 0 (- key-rows 2))]
       (construct-wall ((key-edges [firstcol (inc row)]) :left-back) :left
                       ((key-edges [firstcol row]) :left-front) :left))
     ; inner walls (right, at thumb cluster)
     ; skip last two rows (thumb cluster join)
     (for [row (range 0 (- key-rows 2))]
       (construct-wall ((key-edges [lastcol row]) :right-back) :right
                       ((key-edges [lastcol row]) :right-front) :right))
     ; inner webs (connect from back to front, current to next)
     ; skip last two rows (thumb cluster join)
     (for [row (range 0 (- key-rows 2))]
       (construct-wall ((key-edges [lastcol row]) :right-front) :right
                       ((key-edges [lastcol (inc row)]) :right-back) :right))
     ; back walls
     (for [col (range 0 key-columns)]
       (construct-wall ((key-edges [col firstrow]) :left-back) :back
                       ((key-edges [col firstrow]) :right-back) :back))
     ; back webs (connect from outer to inner, current to next)
     (for [col (range 0 (- key-columns 1))]
       (construct-wall ((key-edges [col firstrow]) :right-back) :back
                       ((key-edges [(inc col) firstrow]) :left-back) :back))
     ; front walls
     ; skip first col (pinky corner)
     ; skip last col (thumb cluster join)
     (for [col (range 1 (- key-columns 2))]
       (construct-wall ((key-edges [col lastrow]) :right-front) :front
                       ((key-edges [col lastrow]) :left-front) :front))
     ; front webs (connect from outer to inner, current to next)
     ; skip first col (pinky corner)
     ; skip last col (thumb cluster join)
     (for [col (range 1 (- key-columns 2))]
       (construct-wall ((key-edges [(inc col) lastrow]) :left-front) :front
                       ((key-edges [col lastrow]) :right-front) :front))
     ; front-outer corner (spans across the missing pinky corner)
     (construct-wall ((key-edges [(inc firstcol) lastrow]) :left-front) :front
                     ((key-edges [firstcol (dec lastrow)]) :left-front) :left)
     ; no front-inner corner; that's where the thumb cluster join is
     ; back-outer corner
     (construct-corner ((key-edges [firstcol firstrow]) :left-back) :left :back)
     ; back-inner corner
     (construct-corner ((key-edges [lastcol firstrow]) :right-back) :back :right))))

(def thumb-walls
  (let [firstcol 0
        firstrow 0
        lastcol (dec thumb-columns)
        lastrow (dec thumb-rows)]
    (union
     ; outer walls (right, away from keys)
     (for [row (range 0 thumb-rows)]
       (construct-wall ((thumb-edges [lastcol row]) :right-back) :right
                       ((thumb-edges [lastcol row]) :right-front) :right))
     ; outer webs (connect from back to front, current to next)
     (for [row (range 0 (- thumb-rows 1))]
       (construct-wall ((thumb-edges [lastcol row]) :right-front) :right
                       ((thumb-edges [lastcol (inc row)]) :right-back) :right))
     ; no inner walls (thumb cluster join)
     ; back walls
     ; skip first col (thumb cluster join)
     (for [col (range 1 thumb-columns)]
       (construct-wall ((thumb-edges [col firstrow]) :left-back) :back
                       ((thumb-edges [col firstrow]) :right-back) :back))
     ; back webs (connect from outer to inner, current to next)
     (for [col (range 1 (- thumb-columns 1))]
       (construct-wall ((thumb-edges [col firstrow]) :right-back) :back
                       ((thumb-edges [(inc col) firstrow]) :left-back) :back))
     ; front walls
     (for [col (range 0 thumb-columns)]
       (construct-wall ((thumb-edges [col lastrow]) :right-front) :front
                       ((thumb-edges [col lastrow]) :left-front) :front))
     ; front webs (connect from outer to inner, current to next)
     (for [col (range 0 (- thumb-columns 1))]
       (construct-wall ((thumb-edges [(inc col) lastrow]) :left-front) :front
                       ((thumb-edges [col lastrow]) :right-front) :front))
     ; back-outer corner
     (construct-corner ((thumb-edges [lastcol firstrow]) :right-back) :back :right)
     ; front-outer corner
     (construct-corner ((thumb-edges [lastcol lastrow]) :right-front) :right :front))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key/Thumb Join Geometry ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def join-web-connectors
  (let [last-col (dec key-columns)
        last-row (dec key-rows)
        key-right-back (key-edges [last-col (dec last-row)])
        key-left-front (key-edges [(dec last-col) last-row])
        thumb-right-back (thumb-edges [1 0])
        thumb-left-front (thumb-edges [0 1])]
    (union
     (web-tri (key-right-back :right-back)
              (thumb-right-back :left-back)
              (key-right-back :right-front))
     (web-tri (key-right-back :right-front)
              (thumb-right-back :left-back)
              (thumb-right-back :left-front))
     (web-tri (key-right-back :right-front)
              (thumb-right-back :left-front)
              (thumb-left-front :right-back))
     (web-tri (key-right-back :right-front)
              (thumb-left-front :right-back)
              (thumb-left-front :left-back))
     ; smoothing polyhedron
     (polyhedron [((key-right-back :right-front) :top)
                  ((thumb-left-front :right-back) :top)
                  ((thumb-left-front :left-back) :top)
                  ((thumb-right-back :left-front) :top)]
                 [[0 2 1]
                  [0 3 2]
                  [0 1 3]
                  [2 3 1]])
     (web-tri (key-right-back :left-front)
              (key-right-back :right-front)
              (thumb-left-front :left-back))
     (web-tri (key-right-back :left-front)
              (thumb-left-front :left-back)
              (key-left-front :right-back))
     (web-tri (key-left-front :right-back)
              (thumb-left-front :left-back)
              (key-left-front :right-front))
     (web-tri (key-left-front :right-front)
              (thumb-left-front :left-back)
              (thumb-left-front :left-front))
     (web-tri (key-left-front :right-front)
              (thumb-left-front :left-front)
              (key-left-front :left-front)))))

(def join-walls
  (union
   ; front wall
   (construct-wall ((thumb-edges [0 (- thumb-rows 1)]) :left-front) :front
                   ((key-edges [(- key-columns 2) (- key-rows 1)]) :left-front) :front)
   ; back wall
   (construct-wall ((key-edges [(- key-columns 1) (- key-rows 2)]) :right-back) :right
                   ((thumb-edges [1 0]) :left-back) :back)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; USB Holder Cut-out ;;
;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

(def holder-x-offset 2.0)
(def holder-y-offset (- wall-thickness 2.0))

; ---

(def usb-holder-translate
  (let [edges (key-edges [(- key-columns 2) 0])
        ; find mid-point (roughly)
        pos-l ((edges :left-back) :bottom)
        pos-r ((edges :right-back) :bottom)
        pos (mapv (fn [a b] (/ (+ a b) 2)) pos-l pos-r)
        pos (project-z pos)
        offset [holder-x-offset holder-y-offset 0.0]]
    (mapv + pos offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bottom Plate Screw Holes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

; M3 screws are obviously 3.0mm, but the hot-melt insert will provide the
; structure for the screws to grab onto. my inserts are quite skinny, so this
; diameter is big enough to clear the screw threads and the insert, but small
; enough to support the insert melting into the plastic
(def screw-post-inner (/ 4.0 2))
; my hot-melt insert diameter is 4.6mm, so 8.0mm should be enough
(def screw-post-outer (/ 8.0 2))
; maximum depth of the screw (without the bottom-plate thickness)
(def screw-post-depth 10.0)

; ---

(defn screw-place-shape [shape edge displace-pos]
  (let [insert-pos (project-z (edge :bottom))]
    (translate (mapv + insert-pos displace-pos) shape)))

(def screw-post
  (let [outer (cylinder 30
                        screw-post-outer
                        screw-post-depth
                        :center false)
        inner (cylinder 30
                        screw-post-inner
                        (+ screw-post-depth z-fighting)
                        :center false)
        inner (translate [0.0 0.0 (/ z-fighting -2)] inner)
        cap-cut (cube (* screw-post-outer 2)
                      (* screw-post-outer 2)
                      (+ screw-post-outer z-fighting))
        cap (difference (sphere 30 screw-post-outer)
                        (translate [0.0 0.0 (/ screw-post-outer -2)] cap-cut))
        cap (translate [0.0 0.0 (- screw-post-depth z-fighting)] cap)]
    (union (difference outer inner) cap)))

; used to cut out from base plate
(def screw-hole (circle 30 screw-post-inner))

(def screw-post-place (partial screw-place-shape screw-post))
(def screw-hole-place (partial screw-place-shape screw-hole))

; calculate one or two displacements for keys (simple)
(defn key-displace [dir0 dir1]
  (let [offset (displace [0.0 0.0 0.0] dir0 (- screw-post-inner))]
    (if (nil? dir1) offset (displace offset dir1 (- screw-post-inner)))))

; calculate one or two displacements for thumbs (need to factor in rotation)
(defn thumb-displace [dir0 dir1]
  (thumb-cluster-rotate (key-displace dir0 dir1)))

(def screw-positions
  [; keys back
   [((key-edges [0 0]) :left-back) (key-displace :left :back)]
   [((key-edges [(- key-columns 3) 0]) :left-back) (key-displace :left :back)]
   [((key-edges [(- key-columns 1) 0]) :right-back) (key-displace :right :back)]
   ; keys front
   [((key-edges [0 (- key-rows 2)]) :left-front) (key-displace :left :front)]
   [((key-edges [1 (- key-rows 1)]) :right-front) (key-displace :right :front)]
   [((key-edges [(- key-columns 2) (- key-rows 1)]) :left-front) (key-displace :front nil)]
   ; key inner/thumb back
   [((key-edges [(- key-columns 1) (- key-rows 2)]) :right-back) (key-displace :right nil)]
   ; thumbs
   [((thumb-edges [(- thumb-columns 1) 0]) :right-back) (thumb-displace :right :back)]
   [((thumb-edges [(- thumb-columns 1) (- thumb-rows 1)]) :right-front) (thumb-displace :right :front)]
   [((thumb-edges [0 (- thumb-rows 1)]) :left-front) (thumb-displace :left :front)]])

(def screw-posts
  (union (mapv (fn [args] (apply screw-post-place args)) screw-positions)))

(def screw-holes
  (union (mapv (fn [args] (apply screw-hole-place args)) screw-positions)))

(spit "things/screw-tester.scad"
      (write-scad (screw-post-place {:bottom [0.0 0.0 0.0]} [0.0 0.0 0.0])))

;;;;;;;;;;;;;;;;
;; Wrist Rest ;;
;;;;;;;;;;;;;;;;

; --- main options

; my wrist rests are the commonly-available kidney-shaped gel ones. these
; dimensions are just used to visualise the kidney shape fits, and offset it
(def wrist-rest-width 110.0)
(def wrist-rest-height 78.0)
; this is however used
(def wrist-rest-depth thumb-cluster-offset-z)

; the smaller diameter of the outer lobes
(def wrist-body-diameter 60.0)
; the angle of the outer lobes
(def wrist-layout-angle (deg2rad 25.0))

; connects the body to the rest
; "thickness" of the connecting rod/box when viewed from top
(def wrist-connector-width 20.0)
; "length" of the connecting rod/box when viewed from top
(def wrist-connector-height 60.0)
(def wrist-connector-depth 10.0)
; the bottom gap; to clear the base plate of the keyboard. this should be as
; close to the thickness of the actual base plate, but this isn't always
; possible to know. so the gap can be a bit bigger to definitely clear the
; plate, and the tolerance will allow the join to move up if the plate is
; thinner. the gap could be shimmed with e.g. foam to prevent rattling
(def wrist-connector-gap 3.0)
(def wrist-connector-tol 2.0)

(def wrist-join-reenforce-radius (/ 15.0 2))
(def wrist-join-slot-radius (/ wrist-join-reenforce-radius 3))
(def wrist-join-clearance 0.2)

; ---

(def wrist-join-wall
  (let [offset (/ (+ wrist-connector-width wrist-join-clearance) 2)
        depth (+ wrist-connector-depth wrist-connector-tol)
        post (union (translate [offset 0.0 0.0]
                               (cylinder 60
                                         wrist-join-reenforce-radius
                                         depth
                                         :center false))
                    (translate [offset 0.0 depth]
                               (sphere 60 wrist-join-reenforce-radius))
                    (->> (cylinder 60
                                   wrist-join-reenforce-radius
                                   offset
                                   :center false)
                         (rotate (deg2rad 90.0) [0 1 0])
                         (translate [0.0 0.0 depth])))
        ; makes this section solid in the base plate projection
        fill (translate [0.0 0.0 (/ depth 2)]
                        (cube (+ wrist-connector-width (* wrist-join-clearance 2))
                              (* wrist-join-reenforce-radius 2)
                              depth))]
    (union post (mirror [1 0 0] post) fill)))

(def wrist-join-slot
  (let [offset (/ (+ wrist-connector-width wrist-join-clearance) 2)
        depth (+ wrist-connector-depth wrist-connector-tol)
        slot-radius (+ wrist-join-slot-radius (/ wrist-join-clearance 2))
        slot (translate [offset 0.0 (- z-fighting)]
                        (cylinder 60
                                  slot-radius
                                  (+ depth z-fighting)
                                  :center false))
        join (cube (+ wrist-connector-width (* wrist-join-clearance 2))
                   (* (+ wrist-join-reenforce-radius z-fighting) 2)
                   (* depth 2))]
    (union slot (mirror [1 0 0] slot) join)))

(def wrist-body-radius (/ wrist-body-diameter 2))
(def wrist-layout-radius wrist-body-diameter)
(def wrist-layout-steps
  [-1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0])

; this function seems to produce the same curvature as the back when multiplied
; with the layout radius. f(0.0) = 0.9, f(1.0) = f(-1.0) = 1.0
(defn wrist-plot-back [x]
  (let [x-prime (/ x (Math/sqrt 10))] (+ 0.9 (* x-prime x-prime))))

(defn wrist-rest-circle-back [lerp]
  (let [shape-layout-radius (- (* wrist-body-diameter (wrist-plot-back lerp)) 2.5)]
    (->> (circle 60 wrist-body-radius)
         (translate [0.0 (- wrist-layout-radius) 0.0])
         (rotate (* wrist-layout-angle (* lerp 0.95)) [0 0 1])
         (translate [0.0 shape-layout-radius 0.0]))))

(defn wrist-rest-circle-front [lerp]
  (let [shape-layout-radius (- wrist-body-diameter 2.5)]
    (->> (circle 60 wrist-body-radius)
         (translate [0.0 (- shape-layout-radius) 0.0])
         (rotate (* wrist-layout-angle lerp) [0 0 1])
         (translate [0.0 wrist-layout-radius 0.0]))))

(def wrist-rest-bounding-box
  (color [0/255 0/255 255/255 0.2] (cube wrist-rest-width wrist-rest-height 1.0)))

(def wrist-rest-base
  (union
   (for [lerp wrist-layout-steps] (wrist-rest-circle-back lerp))
   (for [lerp wrist-layout-steps] (wrist-rest-circle-front lerp))))

(def wrist-connector
  (translate [(/ wrist-connector-width -2) -10.0 0.0]
             (cube wrist-connector-width
                   wrist-connector-height
                   wrist-connector-depth
                   :center false)))

(def wrist-rest
  (let [rest (extrude-linear {:height wrist-rest-depth
                              :twist 0
                              :convexity 3
                              :center false}
                             wrist-rest-base)
        conn-offset (- wrist-connector-height 10.0 wrist-join-reenforce-radius)
        rest-offset (+ (/ wrist-rest-height 2) conn-offset)
        join-offset (/ wrist-connector-width 2)
        join (translate [join-offset 0.0 0.0]
                        (cylinder 30
                                  wrist-join-slot-radius
                                  wrist-connector-depth
                                  :center false))]
    (union (translate [0.0 (- rest-offset) (- wrist-connector-gap)] rest)
           (translate [0.0 (- conn-offset) 0.0] wrist-connector)
           join
           (mirror [1 0 0] join))))

(def wrist-join-translate
  (displace (project-z (((key-edges [2 (dec key-rows)]) :right-front) :bottom))
            :front
            (/ wall-thickness 2)))

(def key-walls-filled
  (union key-walls
         (translate usb-holder-translate usb-holder-wall)
         (translate wrist-join-translate wrist-join-wall)))

(def key-walls
  (difference key-walls-filled
              (translate usb-holder-translate usb-holder-slot)
              (translate wrist-join-translate wrist-join-slot)))

(def wrist-tester
  (let [wall (translate [0.0 (* wrist-join-reenforce-radius 2.5) 0.0]
                        (difference wrist-join-wall wrist-join-slot))
        join (difference wrist-rest
                         (translate [0.0 (- -250.0 wrist-join-reenforce-radius) 0.0]
                                    (cube 500.0 500.0 500.0)))]
    (union wall join)))
(spit "things/wrist-join-tester.scad" (write-scad wrist-tester))

(spit "things/wrist-rest.scad" (write-scad wrist-rest))

;;;;;;;;;;;
;; Model ;;
;;;;;;;;;;;

(def model-left
  (union key-plates
         thumb-plates
         key-web-connectors
         thumb-web-connectors
         join-web-connectors
         key-walls
         thumb-walls
         join-walls
         screw-posts))
(spit "things/left.scad" (write-scad model-left))

;;;;;;;;;;;;;;;;
;; Base Plate ;;
;;;;;;;;;;;;;;;;

(def base-plate-points
  (let [key-firstcol 0
        key-firstrow 0
        key-lastcol (dec key-columns)
        key-lastrow (dec key-rows)
        thumb-firstcol 0
        thumb-firstrow 0
        thumb-lastcol (dec thumb-columns)
        thumb-lastrow (dec thumb-rows)]
    (concat
     ; key front walls
     ; skip first col (pinky corner)
     ; skip last col (thumb cluster join)
     (for [col (reverse (range 1 (- key-columns 2)))]
       [((key-edges [col key-lastrow]) :right-front)
        ((key-edges [col key-lastrow]) :left-front)])
     ; key outer walls (left, at pinky keys)
     ; skip last row (pinky corner)
     (for [row (reverse (range 0 (- key-rows 1)))]
       [((key-edges [key-firstcol row]) :left-front)
        ((key-edges [key-firstcol row]) :left-back)])
     ; key back walls
     (for [col (range 0 key-columns)]
       [((key-edges [col key-firstrow]) :left-back)
        ((key-edges [col key-firstrow]) :right-back)])
     ; key inner walls (right, at thumb cluster)
     ; skip last two rows (thumb cluster join)
     (for [row (range 0 (- key-rows 2))]
       [((key-edges [key-lastcol row]) :right-back)
        ((key-edges [key-lastcol row]) :right-front)])
     ; thumb back walls
     ; skip first col (thumb cluster join)
     (for [col (range 1 thumb-columns)]
       [((thumb-edges [col thumb-firstrow]) :left-back)
        ((thumb-edges [col thumb-firstrow]) :right-back)])
     ; thumb outer walls (right, away from keys)
     (for [row (range 0 thumb-rows)]
       [((thumb-edges [thumb-lastcol row]) :right-back)
        ((thumb-edges [thumb-lastcol row]) :right-front)])
     ; thumb front walls
     (for [col (reverse (range 0 thumb-columns))]
       [((thumb-edges [col thumb-lastrow]) :right-front)
        ((thumb-edges [col thumb-lastrow]) :left-front)]))))

(defn project-2d [edge] (let [p (edge :bottom)] [(p 0) (p 1)]))
(def base-plate-fill (mapv project-2d (apply concat base-plate-points)))

(def plate-left
  (let [walls (cut (translate [0 0 (- z-fighting)]
                              (union key-walls-filled
                                     thumb-walls
                                     join-walls)))
        fill (polygon base-plate-fill)
        plate (difference (union walls fill) screw-holes)]
    plate))
(spit "things/left-plate.scad" (write-scad plate-left))
