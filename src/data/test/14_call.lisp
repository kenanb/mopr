((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   3 3 :grid-fv-counts prop :make-schema-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   3 3 :cw :grid-fv-indices prop :make-schema-prop)
  (:call (prop-pts (:isa :Mesh :points)
          prop-ext (:isa :Mesh :extent))
   10 (0 3 3) #(2 0 1) :grid-points
   prop-ext :make-schema-prop :swap
   prop-pts :make-schema-prop)))
