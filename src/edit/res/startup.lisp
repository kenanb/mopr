((:tree
  ("grid"))
 (:var len () 1.5)
 (:var cell-size () 1.5)
 (:var x-dim () 25)
 (:var y-dim () 10)
 (:var z-dim () 0)
 (:var grid-p ()
  cell-size z-dim y-dim x-dim :3list #(2 0 1) :grid-points :drop)
 (:iota sine-args time 50)
 (:prim ("grid")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   x-dim y-dim :grid-fv-counts prop :make-schema-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   x-dim y-dim :cw :grid-fv-indices prop :make-schema-prop)
  (:call sine-args
   grid-p :copy-array len time :grid-sine-y
   :dup time :as-timesample (:isa :Mesh :points) :make-schema-prop
   :swap :compute-extent time :as-timesample (:isa :Mesh :extent) :make-schema-prop)))
