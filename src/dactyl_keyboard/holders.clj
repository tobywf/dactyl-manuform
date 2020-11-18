(ns dactyl-keyboard.holders
    (:refer-clojure :exclude [use import])
    (:require [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]))

(def z-fighting 0.01)
; make the cut-outs slightly transparent. doesn't affect geometry/difference,
; but helps when visualising cut-outs as solid objects
(def cut-out-color [220/255 163/255 163/255 0.2])

;;;;;;;;;;;;;;;;;;;;;
;; USB Holder TRRS ;;
;;;;;;;;;;;;;;;;;;;;;

; this is specifically modelled on the PJ-320A TRRS jack. the jack is meant to
; be inserted from the top at an angle, with the pins facing upwards.

; --- main options

; outer diameter of the ring/hole of the TRRS jack; the part that goes through
; the panel wall. usually exactly 5mm, but add some clearance for PLA shrink
(def trrs-ring-diameter 5.2)
; the ring length, from the square body part to the end. this is a good
; indicator of how thick the mounting panel should be (a cut-out for the cable
; is generated in case the panel is thicker)
(def trrs-ring-length 2.0)
; length of the square body without the ring. a bit of tolerance here for
; angled insertion is good
(def trrs-jack-height 12.5)
; width of the square body (with the pins sticking up)
(def trrs-jack-width 6.2)
; depth/thickness of the square body (without the pins)
(def trrs-jack-depth 5.0)
; the retaining pin is inserted across the housing, and stops the jack popping
; up the way it was inserted. from the front of the square body (without the
; ring), where the retaining pin is located (between two electrical pins)
(def trrs-retaining-pin-height 8.6)
; the radius of the retaining pin's hole, be careful your printer can handle a
; very small setting here
(def trrs-retaining-pin-radius 0.5)

;---

; the thickness of the housing walls
(def trrs-housing-wall 2.0)
; some extra padding on the bottom (in addition to a wall)
(def trrs-housing-bottom-pad trrs-housing-wall)
; a wedge to help/clear inserting the jack at an angle
(def trrs-housing-wedge trrs-housing-wall)

(def trrs-housing-width (+ trrs-jack-width (* trrs-housing-wall 2)))
; housing only includes one wall, the other is the panel
(def trrs-housing-height (+ trrs-jack-height
                            trrs-housing-wall
                            trrs-housing-wedge))
(def trrs-housing-depth (+ trrs-jack-depth
                           trrs-housing-wall
                           trrs-housing-bottom-pad))

; square up the TRRS housing; with the front-right-bottom corner at 0,0,0. this
; helps when combining it with the USB housing and the panel. for modelling
; however i found it easiest to have the jack lie on the x/y plane
(def trrs-housing-translate-z (+ trrs-housing-wall trrs-housing-bottom-pad))
(def trrs-housing-translate [(/ trrs-housing-width -2)
                             0
                             trrs-housing-translate-z])
; (def trrs-housing-translate [0.0 0.0 0.0])

(def trrs-housing
  (let [; the housing is symmetrical in x
        housing-x (/ trrs-housing-width -2)
        ; the housing is *not* symmetrical in y, because the front is open
        housing-y (- trrs-housing-height)
        ; the bottom should end up so that 0 is where the jack lies
        housing-z (- trrs-housing-translate-z)
        housing (->> (cube trrs-housing-width
                           trrs-housing-height
                           trrs-housing-depth
                           :center false)
                     (translate [housing-x housing-y housing-z]))
        ; the TRRS jack (without the ring)
        jack (->> (cube trrs-jack-width
                        (+ trrs-jack-height z-fighting)
                        (+ trrs-jack-depth z-fighting))
                  ; move the jack cut-out against the front/top faces
                  (translate [0 (/ trrs-jack-height -2) (/ trrs-jack-depth 2)]))
        ; the wedge cut-out is a triangular prism, to allow angled insertion.
        ; ignoring the x axis, imagine the top part of a cut-in-half rectangle:
        ; +--+
        ; | /
        ; |/
        ; +  . (0.0y, 0.0z)
        wedge-height (+ trrs-housing-wedge (/ trrs-housing-wall 2))
        wedge-depth (- trrs-jack-depth 1.5)
        wedge-points [[0 0 0]
                      [trrs-jack-width 0 0]
                      [trrs-jack-width 0 wedge-depth]
                      [0 0 wedge-depth]
                      [0 (- wedge-height) wedge-depth]
                      [trrs-jack-width (- wedge-height) wedge-depth]]
        wedge-faces [[0 1 2 3] [5 4 3 2] [0 4 5 1] [0 3 4] [5 2 1]]
        wedge-x (/ trrs-jack-width -2)
        wedge-y (- trrs-jack-height)
        wedge-z (+ 1.5 z-fighting)
        wedge (->> (polyhedron wedge-points wedge-faces)
                   (translate [wedge-x wedge-y wedge-z]))
        ; a hole in the bottom to make removing the jack easier
        access-hole-length (+ trrs-housing-wall
                              trrs-housing-bottom-pad
                              z-fighting)
        access-hole-y (+ (- trrs-jack-height) 2.5)
        access-hole (->> (with-fn 30 (cylinder 2.0 access-hole-length :center false))
                  (translate [0 access-hole-y (- access-hole-length)]))
        ; the retaining pin mounts
        pin-mount-x (/ (+ trrs-housing-wall trrs-jack-width) 2)
        pin-mount (->> (binding [*fn* 30] (cylinder trrs-housing-wall
                                                    trrs-housing-wall))
                      (rotate (/ pi 2) [0 1 0])
                      (translate [0
                                  (- trrs-retaining-pin-height)
                                  trrs-jack-depth]))
        pin-mount-right (translate [pin-mount-x 0 0] pin-mount)
        pin-mount-left (translate [(- pin-mount-x) 0 0] pin-mount)
        ; the retaining pin hole
        pin-hole-length (+ trrs-housing-width (* z-fighting 2))
        pin-hole-depth (+ trrs-jack-depth trrs-retaining-pin-radius)
        pin-hole (->> (binding [*fn* 30] (cylinder trrs-retaining-pin-radius
                                                   pin-hole-length))
                      (rotate (/ pi 2) [0 1 0])
                      (translate [0
                                  (- trrs-retaining-pin-height)
                                  pin-hole-depth]))
        shape (union housing pin-mount-right pin-mount-left)
        shape (difference shape jack wedge access-hole pin-hole)
        ]
   (translate trrs-housing-translate shape)))

(def trrs-cut-out
  (let [; the TRSS ring cut-out (sticking through the panel)
        ring-radius (/ trrs-ring-diameter 2)
        ring-length (+ trrs-ring-length (* z-fighting 2))
        ring (->> (binding [*fn* 30] (cylinder ring-radius ring-length :center false))
                  (rotate (/ pi 2) [-1 0 0])
                  (translate [0 (- z-fighting) ring-radius]))
        ; cable clearance cut-out, if the panel ends up being thick
        cable-clear-y (+ trrs-ring-length z-fighting)
        cable-clear (->> (binding [*fn* 30] (cylinder 4.5 10.0 :center false))
                         (rotate (/ pi 2) [-1 0 0])
                         (translate [0 cable-clear-y ring-radius]))

        ; the TRRS connector cut-out
        conn-width (- trrs-jack-width z-fighting)
        conn-height (- trrs-jack-height z-fighting)
        conn-depth (- trrs-jack-depth z-fighting)
        conn (->> (cube conn-width conn-height conn-depth)
                  ; move the connector cut-out against the front/top faces
                  (translate [0 (/ trrs-jack-height -2) (/ conn-depth 2)]))
        ; an angled cut-out, in the shape of the connector. the connector is
        ; boxy, so unlike the PCBs where the length is much greater than the
        ; thickness, a bit of relief is required to be able to jam it in, but it
        ; then still sit snugly
        angled-angle (Math/asin (/ 1.5 trrs-jack-height))
        angled-height 4.0
        angled (->> (cube conn-width angled-height (* trrs-jack-depth 1.5))
                    (translate [0 (/ angled-height -2) (/ trrs-jack-depth 4)])
                    (rotate angled-angle [-1 0 0])
                    (translate [0 0 (/ trrs-jack-depth 2)]))
        ]
    (->> (union ring cable-clear conn angled)
         (translate trrs-housing-translate)
         (color cut-out-color))))

(def trrs-test-panel (->> (cube trrs-housing-width 4.0 14.0)
                          (translate [0 0 (- 7.0 trrs-housing-translate-z)])
                          (translate trrs-housing-translate)))
(spit "things/test.scad" (write-scad (difference (union trrs-housing trrs-test-panel) trrs-cut-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USB Holder Elite-C/Pro-Micro ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --- main options

; PCB sizes can vary a bit, and you want a snug fit! (but watch out for PLA shrinkage)

; width of your Elite-C
(def elite-c-width 18.6)
; height of your Elite-C (without the USB-C connector)
(def elite-c-height 33.5)
; width of your Pro-Micro
(def pro-micro-width 18.2)
; height of your Pro-Micro (without the Micro-USB connector)
(def pro-micro-height 33.3)

;  the USB connector options hopefully shouldn't need to be changed (much)...

; thickness of the USB-C connector (add some tolerance to allow angled insertion)
(def usb-c-conn-thickness 3.5)
; width of the USB-C connector
(def usb-c-conn-width 9.2)
; start of the USB-C connector relative to the bottom of the PCB
(def usb-c-conn-z-offset 0.0)
; stick-out of the USB-C cable to the PCB (i.e. include the connector stick-out)
; make this a bit smaller, you don't want to have trouble inserting the cable
; all the way
(def usb-c-cable-stickout 2.4)

; thickness of the Micro-USB connector (add some tolerance to allow angled insertion)
(def usb-micro-conn-thickness 2.7)
; width of the Micro-USB connector
(def usb-micro-conn-width 7.4)
; start of the Micro-USB connector relative to the bottom of the PCB
(def usb-micro-conn-z-offset 1.6)
; stick-out of the Micro-USB cable to the PCB (i.e. include the connector
; stick-out). make this a bit smaller, you don't want to have trouble inserting
; the cable all the way
(def usb-micro-cable-stickout 3.0)

; other options

; the thickness of the housing's wall
(def usb-housing-wall 2.0)
; the depth the PCB will rest in the holder (not critical)
(def usb-housing-inner-depth 5.0)
; how much to cut out on the left/right to relieve headers (not critical)
(def usb-housing-header-cut-out 3.5)

; ---

(def usb-housing-bottom-pad usb-housing-wall)
(def usb-housing-depth (+ usb-housing-inner-depth usb-housing-bottom-pad))

; calculated geometry
(def elite-c-housing-width (+ elite-c-width (* usb-housing-wall 2)))
(def elite-c-housing-height (+ elite-c-height
                               usb-housing-wall
                               usb-c-cable-stickout))

(def pro-micro-housing-width (+ pro-micro-width (* usb-housing-wall 2)))
(def pro-micro-housing-height (+ pro-micro-height
                               usb-housing-wall
                               usb-micro-cable-stickout))


(def elite-c-housing-translate [(/ elite-c-housing-width 2)
                                (- (/ elite-c-height -2) usb-c-cable-stickout)
                                usb-housing-bottom-pad])
; (def elite-c-housing-translate [0.0 0.0 0.0])

(def pro-micro-housing-translate [(/ pro-micro-housing-width 2)
                                  (- (/ pro-micro-height -2) usb-micro-cable-stickout)
                                  usb-housing-bottom-pad])
; (def pro-micro-housing-translate [0.0 0.0 0.0])

(def elite-c-housing
  (let [; the housing is symmetrical in x
        housing-x (/ elite-c-housing-width -2)
        ; the housing is *not* symmetrical in y, because of the USB connector
        housing-y (- (/ elite-c-height -2) usb-housing-wall)
        ; the bottom should end up at minus usb-housing-bottom-pad, so that
        ; all the cut-outs can reference 0 z as the bottom of the PCB
        housing-z (- usb-housing-bottom-pad)
        housing (->> (cube elite-c-housing-width
                           elite-c-housing-height
                           usb-housing-depth
                           :center false)
                     (translate [housing-x housing-y housing-z]))

        ; the micro-controller (without the USB connector)
        mcu-depth (+ usb-housing-inner-depth z-fighting)
        mcu (->> (cube elite-c-width elite-c-height mcu-depth)
                 (translate [0 0 (/ usb-housing-inner-depth 2)]))
        ; header cut-outs
        header-depth (+ usb-housing-bottom-pad z-fighting)
        header-x (/ (- elite-c-width usb-housing-header-cut-out) 2)
        header-z (/ usb-housing-bottom-pad -2)
        header (cube usb-housing-header-cut-out elite-c-height header-depth)
        header-left (translate [(- header-x) 0 header-z] header)
        header-right (translate [header-x 0 header-z] header)
        ; the wedge cut-out is a triangular prism, to allow angled insertion.
        wedge-height (/ usb-housing-wall 2)
        wedge-depth (- usb-housing-inner-depth usb-housing-wall)
        wedge-points [[0 0 0]
                      [elite-c-width 0 0]
                      [elite-c-width 0 wedge-depth]
                      [0 0 wedge-depth]
                      [0 (- wedge-height) wedge-depth]
                      [elite-c-width (- wedge-height) wedge-depth]]
        wedge-faces [[0 1 2 3] [5 4 3 2] [0 4 5 1] [0 3 4] [5 2 1]]
        wedge-x (/ elite-c-width -2)
        wedge-y (+ (/ elite-c-height -2) z-fighting)
        wedge-z (+ usb-housing-wall z-fighting)
        wedge (->> (polyhedron wedge-points wedge-faces)
                   (translate [wedge-x wedge-y wedge-z]))
        ; a hole to make removing the micro-controller easier
        hole-length (+ usb-housing-bottom-pad z-fighting)
        hole-y (+ (/ elite-c-height -2) 3.0)
        hole (->> (binding [*fn* 30] (cylinder 2.0 hole-length :center false))
                  (translate [0 hole-y (- hole-length)]))
        ]
   (->> (difference housing mcu header-left header-right wedge hole)
        (translate elite-c-housing-translate))))

(def elite-c-cut-out
  (let [
        ; ; the micro-controller cut-out without USB connector (taller)
        ; mcu-depth (* usb-housing-inner-depth 2)
        ; mcu (->> (cube elite-c-width elite-c-height mcu-depth)
        ;          (translate [0 0 usb-housing-inner-depth]))
        ; USB-C cut-out
        ; thickness is the diameter...?!
        usb-c-radius (/ usb-c-conn-thickness 2)
        usb-c-length (+ usb-c-cable-stickout (* z-fighting 2))
        ; the cylinders edge-to-edge should be the total width
        usb-c-x (/ (- usb-c-conn-width usb-c-conn-thickness) 2)
        ; shift up past the PCB
        usb-c-y (- (/ elite-c-height 2) z-fighting)
        usb-c-z (+ usb-c-radius usb-c-conn-z-offset)
        usb-c-part (->> (binding [*fn* 30]
                                 (cylinder usb-c-radius
                                           usb-c-length
                                           :center false))
                        (rotate (/ pi 2) [-1 0 0])
                        (translate [0 usb-c-y usb-c-z]))
        usb-c (hull (translate [usb-c-x 0 0] usb-c-part)
                    (translate [(- usb-c-x) 0 0] usb-c-part))
        ; cable clearance cut-out, punch through the panel
        cable-clear-x (+ usb-c-y usb-c-cable-stickout)
        cc-round (->> (binding [*fn* 30] (cylinder 4.0 10.0 :center false))
                         (rotate (/ pi 2) [-1 0 0])
                         (translate [0 cable-clear-x usb-c-z]))
        cc-bottom (->> (cube 16.0 10.0 4.0)
                    (translate [0 (+ cable-clear-x 5.0) (- z-fighting)]))
        cable-clear (hull (translate [4.0 0 0] cc-round)
                           (translate [-4.0 0 0] cc-round)
                           cc-bottom)
        ]
   (->> (union usb-c cable-clear)
        (translate elite-c-housing-translate)
        (color cut-out-color))))

(def pro-micro-housing
  (let [; the housing is symmetrical in x
        housing-x (/ pro-micro-housing-width -2)
        ; the housing is *not* symmetrical in y, because of the USB connector
        housing-y (- (/ pro-micro-height -2) usb-housing-wall)
        ; the bottom should end up at minus usb-housing-bottom-pad, so that
        ; all the cut-outs can reference 0 z as the bottom of the PCB
        housing-z (- usb-housing-bottom-pad)
        housing (->> (cube pro-micro-housing-width
                           pro-micro-housing-height
                           usb-housing-depth
                           :center false)
                     (translate [housing-x housing-y housing-z]))

        ; the micro-controller (without the USB connector)
        mcu-depth (+ usb-housing-inner-depth z-fighting)
        mcu (->> (cube pro-micro-width pro-micro-height mcu-depth)
                 (translate [0 0 (/ usb-housing-inner-depth 2)]))
        ; header cut-outs
        header-depth (+ usb-housing-bottom-pad z-fighting)
        header-x (/ (- pro-micro-width usb-housing-header-cut-out) 2)
        header-z (/ usb-housing-bottom-pad -2)
        header (cube usb-housing-header-cut-out pro-micro-height header-depth)
        header-left (translate [(- header-x) 0 header-z] header)
        header-right (translate [header-x 0 header-z] header)
        ; the wedge cut-out is a triangular prism, to allow angled insertion.
        wedge-height (/ usb-housing-wall 2)
        wedge-depth (- usb-housing-inner-depth usb-housing-wall)
        wedge-points [[0 0 0]
                      [pro-micro-width 0 0]
                      [pro-micro-width 0 wedge-depth]
                      [0 0 wedge-depth]
                      [0 (- wedge-height) wedge-depth]
                      [pro-micro-width (- wedge-height) wedge-depth]]
        wedge-faces [[0 1 2 3] [5 4 3 2] [0 4 5 1] [0 3 4] [5 2 1]]
        wedge-x (/ pro-micro-width -2)
        wedge-y (+ (/ pro-micro-height -2) z-fighting)
        wedge-z (+ usb-housing-wall z-fighting)
        wedge (->> (polyhedron wedge-points wedge-faces)
                   (translate [wedge-x wedge-y wedge-z]))
        ; a hole to make removing the micro-controller easier
        hole-length (+ usb-housing-bottom-pad z-fighting)
        hole-y (+ (/ pro-micro-height -2) 3.0)
        hole (->> (binding [*fn* 30] (cylinder 2.0 hole-length :center false))
                  (translate [0 hole-y (- hole-length)]))
        ]
   (->> (difference housing mcu header-left header-right wedge hole)
        (translate pro-micro-housing-translate))))

(def pro-micro-cut-out
  (let [
        ; ; the micro-controller cut-out without USB connector (taller)
        ; mcu-depth (* usb-housing-inner-depth 2)
        ; mcu (->> (cube pro-micro-width pro-micro-height mcu-depth)
        ;          (translate [0 0 usb-housing-inner-depth]))
        ; Micro-USB cut-out
        usb-micro-x (/ usb-micro-conn-width -2)
        ; shift up past the PCB
        usb-micro-y (- (/ pro-micro-height 2) z-fighting)
        usb-micro-z usb-micro-conn-z-offset
        usb-micro (->> (cube usb-micro-conn-width
                             (+ usb-micro-cable-stickout (* z-fighting 2))
                             usb-micro-conn-thickness
                             :center false)
                       (translate [usb-micro-x usb-micro-y usb-micro-z]))
        ; cable clearance cut-out, punch through the panel
        cable-clear-x (+ (/ pro-micro-height 2)
                         usb-micro-cable-stickout
                         (- z-fighting))
        cable-clear-z (+ usb-micro-z 2.0)
        cc-round (->> (binding [*fn* 30] (cylinder 4.0 10.0 :center false))
                         (rotate (/ pi 2) [-1 0 0])
                         (translate [0 cable-clear-x cable-clear-z]))
        cc-bottom (->> (cube 16.0 10.0 4.0)
                    (translate [0 (+ cable-clear-x 5.0) (- z-fighting)]))
        cable-clear (hull (translate [4.0 0 0] cc-round)
                           (translate [-4.0 0 0] cc-round)
                           cc-bottom)
        ]
   (->> (union usb-micro cable-clear)
        (translate pro-micro-housing-translate)
        (color cut-out-color))))

;;;;;;;;;;;;;;;;;;;;;;
;; USB Holder Panel ;;
;;;;;;;;;;;;;;;;;;;;;;

; warning: think hard before changing these! this will mean the holder and the
; cut-out in the keyboard will be custom, so you can't just print another
; without knowing what you set these to
(def usb-housing-panel-width 34.0)
(def usb-housing-panel-height 4.0)
(def usb-housing-panel-depth 20.0)
(def usb-housing-panel-rail 1.0)

(def usb-housing-overlap (min trrs-housing-wall usb-housing-wall))
(def elite-c-min-width (- (+ trrs-housing-width elite-c-width) usb-housing-overlap))
(def pro-micro-min-width (- (+ trrs-housing-width pro-micro-width) usb-housing-overlap))

(def elite-c-diff-width (- usb-housing-panel-width elite-c-min-width))
(def pro-micro-diff-width (- usb-housing-panel-width pro-micro-min-width))

(assert (> elite-c-diff-width 0.0) "Elite-C holder would be too wide")
(assert (> pro-micro-diff-width 0.0) "Pro-Micro holder would be too wide")

(def elite-c-holder
  (let [overlap (- (/ usb-housing-overlap 2) z-fighting)
        panel-y (/ usb-housing-panel-height -2)
        panel (->> (cube usb-housing-panel-width
                         usb-housing-panel-height
                         usb-housing-panel-depth
                         :center false)
                   (translate [(- trrs-housing-width) panel-y 0]))
        rail (binding [*fn* 30] (cylinder usb-housing-panel-rail
                                          usb-housing-panel-depth
                                          :center false))
        rail-right (translate [(- usb-housing-panel-width trrs-housing-width) 0 0] rail)
        rail-left (translate [(- trrs-housing-width) 0 0] rail)
        holder (union panel
                      rail-right
                      rail-left
                      (translate [overlap 0 0] trrs-housing)
                      (translate [(- overlap) 0 0] elite-c-housing))]
  (difference holder
              (translate [overlap 0 0] trrs-cut-out)
              (translate [(- overlap) 0 0] elite-c-cut-out))
  ))

(spit "things/holder-elite-c.scad" (write-scad elite-c-holder))

(def pro-micro-holder
  (let [overlap (- (/ usb-housing-overlap 2) z-fighting)
        panel-y (/ usb-housing-panel-height -2)
        panel (->> (cube usb-housing-panel-width
                         usb-housing-panel-height
                         usb-housing-panel-depth
                         :center false)
                   (translate [(- trrs-housing-width) panel-y 0]))
        rail (binding [*fn* 30] (cylinder usb-housing-panel-rail
                                          usb-housing-panel-depth
                                          :center false))
        rail-right (translate [(- usb-housing-panel-width trrs-housing-width) 0 0] rail)
        rail-left (translate [(- trrs-housing-width) 0 0] rail)
        holder (union panel
                      rail-right
                      rail-left
                      (translate [overlap 0 0] trrs-housing)
                      (translate [(- overlap) 0 0] pro-micro-housing))]
  (difference holder
              (translate [overlap 0 0] trrs-cut-out)
              (translate [(- overlap) 0 0] pro-micro-cut-out))
  ))

(spit "things/holder-pro-micro.scad" (write-scad pro-micro-holder))
