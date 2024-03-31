((:tree
  ("Grid10x10"))
 (:var len () 1)
 (:var grid-p ()
  1 (0 10 10) #(2 0 1) :grid-points :drop)
 (:each sine-args time
        00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19)
 (:prim ("Grid10x10")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   10 10 :grid-fv-counts nil prop :make-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   10 10 :cw :grid-fv-indices nil prop :make-prop)
  (:call sine-args
   grid-p len time :grid-sine-y
   :dup time (:isa :Mesh :points) :make-prop
   :swap :compute-extent time (:isa :Mesh :extent) :make-prop)))
