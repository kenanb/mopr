((:tree
  ("Grid2x2"))
 (:prim ("Grid2x2")
  (:type :Mesh)
  (:call (:prop (:isa :Mesh :faceVertexCounts))
   2 2 :grid-fv-counts nil :prop :make-prop)
  (:call (:prop (:isa :Mesh :faceVertexIndices))
   2 2 :ccw :grid-fv-indices nil :prop :make-prop)
  (:call (:prop-pts (:isa :Mesh :points)
          :prop-ext (:isa :Mesh :extent))
   10 (0 2 2) #(2 1 0) :grid-points
   nil :prop-ext :make-prop :swap
   nil :prop-pts :make-prop)))
