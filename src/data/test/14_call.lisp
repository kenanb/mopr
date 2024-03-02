((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call :isa :Mesh :faceVertexCounts 3 3 :grid-fv-counts nil :make-prop)
  (:call :isa :Mesh :faceVertexIndices 3 3 :cw :grid-fv-indices nil :make-prop)
  (:call 10 (0 3 3) #(2 0 1) :grid-points nil :make-point-based)))
