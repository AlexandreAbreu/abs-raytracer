;; first raytracer test

(include "srfi/srfi1.scm")

;; preliminary stuff
 
(define-structure vector3d x y z)
 
(define vector3d->list
  (lambda (v)
    `(,(vector3d-x v) ,(vector3d-y v) ,(vector3d-z v))
    ))
 
(define vector3d-norm
  (lambda (v)
    (let ((x (vector3d-x v))
          (y (vector3d-y v))
          (z (vector3d-z v)))
      (flsqrt (fl+ (fl* x x) (fl* y y) (fl* z z)))
      )
    ))
 
(define vector3d-normalize
  (lambda (v)
    (let ((n (vector3d-norm v)))
      ;; TODO make sure that non div by 0
      (if (= n 0)
          (error "vector3d-normalize: division by 0")
          (apply make-vector3d (map (lambda (c) (fl/ c n)) (vector3d->list v)))
          )
      )
    )
  )
 
(define vector3d-op
  (lambda (op v1 v2)
    (make-vector3d
     (op (vector3d-x v1) (vector3d-x v2))
     (op (vector3d-y v1) (vector3d-y v2))
     (op (vector3d-z v1) (vector3d-z v2))
     )
    )
  )
 
;;
(define vector3d-
  (lambda (v1 v2)
    (vector3d-op fl- v1 v2)
    )
  )
 
;;
(define vector3d+
  (lambda (v1 v2)
    (vector3d-op fl+ v1 v2)
    )
  )
 
;;
(define vector3d*scalar
  (lambda (v scalar)
    (make-vector3d
     (fl* (vector3d-x v) scalar)
     (fl* (vector3d-y v) scalar)
     (fl* (vector3d-z v) scalar)
     )
    )
  )
 
;;
(define dot-product
  (lambda (v1 v2)
    (fl+
     (fl* (vector3d-x v1) (vector3d-x v2))
     (fl* (vector3d-y v1) (vector3d-y v2))
     (fl* (vector3d-z v1) (vector3d-z v2))
     )
    )
  )
  

;; raytracing stuff

 
;; basic defs
 
(define-structure ray direction origin)
 
(define-structure color r g b)

;; this somehow sucks but I have to repeat the same code for rgb, macro?

(define color*scalar
  (lambda (color s)
    (make-color (* (color-r color) s) (* (color-g color) s) (* (color-b color) s))
    )
  )

(define color*color
  (lambda (c1 c2)
    (make-color (* (color-r c1) (color-r c2))
                (* (color-g c1) (color-g c2))
                (* (color-b c1) (color-b c2))
                )
    )
  )                

(define color+color
  (lambda (c1 c2)
    (make-color (+ (color-r c1) (color-r c2))
                (+ (color-g c1) (color-g c2))
                (+ (color-b c1) (color-b c2))
                )
    )
  )

;; objects
 
;; sphere
 
(define-structure sphere center radius)
 
(define compute-t1-t2
  (lambda (B C delta ray X0c Y0c Z0c)
    (let ([t1 (fl* -1.0 (fl+ B (flsqrt delta)))]
          [t2 (fl- B (flsqrt delta))])
      (if (fl< t2 0.0)
          #f
          (if (fl> t1 0.0)
              t1
              t2)
          )
      )
    )
  )
 
(define hit-test-sphere
  (lambda (ray sphere)
    (let* (
           [X0c (fl- (vector3d-x (sphere-center sphere)) (vector3d-x (ray-origin ray)))]
           [Y0c (fl- (vector3d-y (sphere-center sphere)) (vector3d-y (ray-origin ray)))]
           [Z0c (fl- (vector3d-z (sphere-center sphere)) (vector3d-z (ray-origin ray)))]
           [ray-dx (vector3d-x (ray-direction ray))]
           [ray-dy (vector3d-y (ray-direction ray))]
           [ray-dz (vector3d-z (ray-direction ray))]
           [B (fl+ (fl* ray-dx X0c) (fl* ray-dy Y0c) (fl* ray-dz Z0c))]
           [C (fl+ (flexpt X0c 2.0) (flexpt Y0c 2.0) (flexpt Z0c 2.0) (fl- (flexpt (sphere-radius sphere) 2.0)))]
           [delta (fl- (fl* B B) C)]
           )
      (if (fl> delta 0.0)
          (compute-t1-t2 B C delta ray X0c Y0c Z0c)
          #f
          )
      )
    )
  )
 
(define compute-normal-sphere
  (lambda (sphere hitp)
    (vector3d-normalize
     (vector3d-
      hitp
      (sphere-center sphere))
     )
    )
  )
 
;;
 
;; TODO plane & others

(define-structure plane normal offset)

(define hit-test-plane
  (lambda (ray plane)
    (let* (
           [OA (dot-product (ray-origin ray) (plane-normal plane))]
           [Ad (dot-product (ray-direction ray) (plane-normal plane))]
           )
      (if (flzero? Ad)
          #f
          (let ([t (fl- (fl/ (fl+ (plane-offset plane) OA) Ad))])
            ;; take sign into account
            (if (fl> t 0.0)
                t
                #f
                )
            )
          )
      )
    )
  )

(define compute-normal-plane
  (lambda (plane hitp)
    (plane-normal plane)
    )
  )

 
;; raytracing part
 
(define compute-hit-point
  (lambda (ray t)
    (vector3d+
     (ray-origin ray)
     (vector3d*scalar (ray-direction ray) t)
     )
    )
  )
 
;; used in debug
(define *hitcnt* 0)
 
(define-structure scene-object
  ;; hit test function: ray: vector3d -> #f or ray distance to hit point
  hit-test-func
  ;; compute normal function: hit-point: vector3d -> normal: vector3d
  compute-normal-func
  ;;
  geometry
  ;; float
  diffuse-factor
  ;; float
  reflective-factor
  ;; color
  color
  )
 
(define make-scene-object-sphere
  (lambda (geometry diffuse reflective color)
    (make-scene-object
     hit-test-sphere
     compute-normal-sphere
     geometry
     diffuse
     reflective
     color
     )
    )
  )

(define make-scene-object-plane
  (lambda (geometry diffuse reflective color)
    (make-scene-object
     hit-test-plane
     compute-normal-plane
     geometry
     diffuse
     reflective
     color
     )
    )
  )

(define-structure light
  ;; vector3d
  position
  ;; color
  color
  )
 
(define-structure scene
  ;; scene-objects
  objects
  ;; lights
  lights
  ;; color
  ambient
  )

(define *epsilon* 0.001)
(define *maxrecursedepth* 2)

(define trace-ray
  (lambda (ray objects lights curdepth)
    (define offset-position
      (lambda (point direction)
        (vector3d+ point (vector3d*scalar direction *epsilon*))
        )
      )
    (define compute-reflected-ray
      (lambda (original-ray normal point)
        (let ([direction (vector3d-normalize
                          (vector3d-
                           original-ray
                           (vector3d*scalar normal (* 2.0 (dot-product original-ray normal)))
                           )
                          )])
          (make-ray direction (offset-position point direction))
          )
        )
      )
    (define compute-diffuse-lighting
      (lambda (object point light)
          (let* ([object-normal-func (scene-object-compute-normal-func object)]
                 [object-geometry (scene-object-geometry object)]
                 [object-color (scene-object-color object)]
                 [cn
                  ;; diffuse factor
                  (dot-product
                   (object-normal-func object-geometry point)
                   ;; from hitpoint to light
                   (vector3d-normalize (vector3d- (light-position light) point))
                   )
                  ]
                 )
            (color*scalar (light-color light) cn)
            )
          )
      )
    ;; get the object whose intersects w/ the current ray
    ;; get-ray-hit-point: ray objects -> (object or #f, t)
    (define get-ray-hit-point
      (lambda (ray objects)
        (let ([hit-object '(#f +inf.0)])
          ;; horrible side effect
          (for-each
           (lambda (object)
             (let ([t ((scene-object-hit-test-func object) ray (scene-object-geometry object))])
              (if (and t (fl<= t (cadr hit-object)))
                  ;; TODO change this
                  (set! hit-object `(,object ,t))
                  #t
                  )
              )
             )
           objects
           )
          ;;
          hit-object
          )
        )
      )
    ;; point light objects -> (object or #f, t)
    (define trace-shadow-ray
      (lambda (point light objects)
        (let* (
               [direction (vector3d-normalize (vector3d- (light-position light) point))]
               [ray (make-ray direction point)]
               )
          (get-ray-hit-point ray objects)
          )
        )
      )
    (let* (
           [hit-object-description (get-ray-hit-point ray objects)]
           [hit-object (first hit-object-description)]
           [distance-to-object (second hit-object-description)]
           )
      (if hit-object
          (let* ([t distance-to-object]
                 [point (compute-hit-point ray t)]
                 [object-normal-func (scene-object-compute-normal-func hit-object)]
                 [object-geometry (scene-object-geometry hit-object)]
                 [object-normal (object-normal-func object-geometry point)]
                 [diffuse-factor (scene-object-diffuse-factor hit-object)]
                 [affected-by-lights (filter (lambda (light) (not (first (trace-shadow-ray (offset-position point object-normal) light objects)))) lights)]
                 [reflective-factor (scene-object-reflective-factor hit-object)]
                 )
            (if (= (length affected-by-lights) 0)
                (make-color 0.0 0.0 0.0)
                (color*color
                 (scene-object-color hit-object)
                 (fold
                  (lambda (light other-light-contribution-diffuse-color)
                    (let ([light-object-diffuse-color (compute-diffuse-lighting hit-object point light)])
                      (color+color
                       other-light-contribution-diffuse-color
                       (if (and (fl>= reflective-factor) (< curdepth *maxrecursedepth*))
                           ;; reflect the ray and recurse
                           (color+color
                            (color*scalar light-object-diffuse-color diffuse-factor)
                            (color*scalar (trace-ray (compute-reflected-ray (ray-direction ray) object-normal point) objects lights (+ curdepth 1)) reflective-factor)
                            )
                           ;; diffuse only
                           (color*scalar light-object-diffuse-color diffuse-factor)
                           )
                       )
                      )
                    )
                  (make-color 0.0 0.0 0.0)
                  affected-by-lights
                 )
                 )
                )
            )
          ;; background color
          (make-color 0.0 0.0 0.0)
          )
      )
    )
  )
 
;;
 
(define save-to-ppm-file
  (lambda (filename values height width)
    (if (not (and height width))
        (error (format "saveppm: Invalid height or width: ~s ~s" height width))
        (if (not (eq? (* height width 3) (length values)))
            ;; (error (format "saveppm: Invalid height or width: ~s ~s, must be equal to number of values ~s" height width (length values)))
            (error (with-output-to-string '() (lambda () (print "saveppm: Invalid height or width: " height " " width  ", must be equal to number of values " (length values)))))
            (call-with-output-file (list path: filename append: #f)
              (lambda (f)
                (define header (string->symbol "P6"))
                (write header f)
                (newline f)
                (write width f)
                (newline f)
                (write height f)
                (newline f)
                (write 255 f)
                (newline f)
                (let ((v (list->vector values)))
                  ;; height
                  (let hloop ([h 0])
                    (if (= h height)
                        #t
                        ;; width
                        (let wloop ([w 0])
                          (if (= w width)
                              (hloop (+ 1 h))
                              (let (
                                    (index (+
                                            (* width h 3)
                                            (* 3 w)
                                            )
                                           )
                                    )
                                (if (>= index (vector-length v))
                                    ;; maybe add padding
                                    (error "blabla")
                                    (begin
                                      (write-u8 (vector-ref v index) f)
                                      (write-u8 (vector-ref v (+ index 1)) f)
                                      (write-u8 (vector-ref v (+ index 2)) f)
                                      )
                                    )
                                (wloop (+ w 1))
                                )
                              )
                          )
                        )
                    )
                  )
                )
              )
            )
        )
    )
  )
 
(define screen-width 320)
(define screen-height 200)
 
(define eye (make-vector3d 0.0 0.0 -300.0))
 
(define mid (lambda (s) (/ s 2)))

(define scene
  `(
    ,(make-scene-object-sphere
      (make-sphere (make-vector3d 150.0 -100.0 -400.0)
                   200.0)
      0.7
      0.7
      (make-color 0.9 0.6 0.3)
      )
    
    ,(make-scene-object-sphere
      (make-sphere (make-vector3d -150.0 200.0 50.0)
                   50.0)
      0.7
      0.7
      (make-color 1.0 1.0 1.0)
      )
    
    ,(make-scene-object-sphere
      (make-sphere (make-vector3d 150.0 50.0 50.0)
                   20.0)
      0.7
      0.7
      (make-color 0.3 0.6 0.0)
      )
    
    ,(make-scene-object-sphere
      (make-sphere (make-vector3d 0.0 0.0 50.0)
                   80.0)
      0.7
      0.7
      (make-color 0.3 0.6 0.9)
      )
    
    ,(make-scene-object-plane
      (make-plane (make-vector3d 0.0 1.0 0.0)
                   100.0)
      1.0
      0.0
      (make-color 0.9 0.8 0.0)
      )
    )
  )

(define lights
  `(
    ,(make-light (make-vector3d -200.0 200.0 -100.0) (make-color 1.0 0.5 0.5))
    ,(make-light (make-vector3d 300.0 200.0 -100.0) (make-color 1.0 1.0 1.0))
    ,(make-light (make-vector3d 0.0 -200.0 -50.0) (make-color 0.0 1.0 1.0))
    )
  )
 
(define float-to-255
      (lambda (normalized-value)
        (max 0 (min 255 (inexact->exact (flfloor (fl* normalized-value 255.0)))))
        )
      )

(define main-loop
  (lambda (eye-position screen-height screen-width)
     (save-to-ppm-file
      "./abs-raytracer.ppm"
      (u8vector->list
       (with-output-to-u8vector
        '()
        (lambda ()
          ;; Y
          (let loopy ([y screen-height])
            (if (= y 0)
                #t
                ;; X
                (let loopx ([x 0])
                  (define emit-color
                    (lambda (color)
                      (write-u8 (float-to-255 (color-r color)))
                      (write-u8 (float-to-255 (color-g color)))
                      (write-u8 (float-to-255 (color-b color)))
                      )
                    )
                  (if (>= x screen-width)
                      (loopy (- y 1))
                      ;; compute current ray
                      (begin
                        (emit-color (trace-ray (make-ray
 
                                                (vector3d-normalize
                                                 (vector3d-
                                                  (make-vector3d
                                                   (fixnum->flonum (- x (mid screen-width)))
                                                   (fixnum->flonum (- y (mid screen-height)))
                                                   0.0
                                                   )
                                                  eye
                                                  )
                                                 )
                                               
                                                eye
                                                )
                                               scene
                                               lights
                                               0
                                               )
                                    )
                        (loopx (+ x 1))
                        )
                      )
                  )
                )
            )
          )
        )
       )
     screen-height
     screen-width
     )
     )
  )
 
(define run
  (lambda ()
    (time
     (main-loop eye screen-height screen-width)
     )
    )
  )
