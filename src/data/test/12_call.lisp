((:tree
  ("Grid3x2"))
 (:prim ("Grid3x2")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   3 2 :grid-fv-counts prop :make-schema-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   3 2 :ccw :grid-fv-indices prop :make-schema-prop)
  (:call (prop-pts (:isa :Mesh :points)
          prop-ext (:isa :Mesh :extent))
   10 (0 2 3) #(2 1 0) :grid-points
   prop-ext :make-schema-prop :swap
   prop-pts :make-schema-prop)))
