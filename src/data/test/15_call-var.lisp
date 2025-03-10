((:tree
  ("Grid3x3"))
 (:var dim () 4)
 (:var len () 7)
 (:var prop-pts () (:isa :Mesh :points))
 (:var prop-ext () (:isa :Mesh :extent))
 (:var base-grid-points ()
  10 (0 3 3) #(2 0 1) :grid-points :drop)
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   3 3 :grid-fv-counts prop :make-schema-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   3 3 :cw :grid-fv-indices prop :make-schema-prop)
  (:call (val 0 time 00)
   base-grid-points :copy-array dim len val :grid-oscillate-y
   time :as-timesample prop-pts :make-schema-prop)
  (:call (val 7 time 10)
   base-grid-points :copy-array dim len val :grid-oscillate-y
   time :as-timesample prop-pts :make-schema-prop)
  (:call (val 0 time 20)
   base-grid-points :copy-array dim len val :grid-oscillate-y :dup
   time :as-timesample prop-pts :make-schema-prop
   :swap :compute-extent prop-ext :make-schema-prop)))
