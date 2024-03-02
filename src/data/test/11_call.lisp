((:tree
  ("Grid2x2"))
 (:prim ("Grid2x2")
  (:type :Mesh)
  (:call (:isa :Mesh :faceVertexCounts) 2 2 :grid-fv-counts nil :make-prop)
  (:call (:isa :Mesh :faceVertexIndices) 2 2 :ccw :grid-fv-indices nil :make-prop)
  (:call (:isa :Mesh :points)
         (:isa :Mesh :extent)
         10 (0 2 2) #(2 1 0) :grid-points
         :swapd nil :make-prop
         :-rot nil :make-prop
         :2list :make-group)))
