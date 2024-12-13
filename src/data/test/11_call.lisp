((:tree
  ("Grid2x2"))
 (:prim ("Grid2x2")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   2 2 :grid-fv-counts prop :make-schema-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   2 2 :ccw :grid-fv-indices prop :make-schema-prop)
  (:call (prop-pts (:isa :Mesh :points)
          prop-ext (:isa :Mesh :extent))
   10 (0 2 2) #(2 1 0) :grid-points
   prop-ext :make-schema-prop :swap
   prop-pts :make-schema-prop)))
