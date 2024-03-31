((:tree
  ("Grid10x10"))
 (:var len () 1)
 (:var cell-size () 1)
 (:var x-dim () 10)
 (:var y-dim () 10)
 (:var z-dim () 0)
 (:var grid-p ()
  cell-size z-dim y-dim x-dim :3list #(2 0 1) :grid-points :drop)
 (:iota sine-args time 20)
 (:prim ("Grid10x10")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   x-dim y-dim :grid-fv-counts nil prop :make-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   x-dim y-dim :cw :grid-fv-indices nil prop :make-prop)
  (:call sine-args
   grid-p len time :grid-sine-y
   :dup time (:isa :Mesh :points) :make-prop
   :swap :compute-extent time (:isa :Mesh :extent) :make-prop)))
