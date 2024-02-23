((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call 3 3 :grid-fv-counts)
  (:call 3 3 :ccw :grid-fv-indices)
  (:call 10 (0 3 3) #(2 1 0) :grid-points)))
