((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   3 3 :grid-fv-counts nil prop :make-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   3 3 :ccw :grid-fv-indices nil prop :make-prop)
  (:call (prop-pts (:isa :Mesh :points)
          prop-ext (:isa :Mesh :extent))
   10 (0 3 3) #(2 1 0) :grid-points
   nil prop-ext :make-prop :swap
   nil prop-pts :make-prop)))
