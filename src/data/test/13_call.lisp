((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call () (:isa :Mesh :faceVertexCounts) 3 3 :grid-fv-counts nil :make-prop)
  (:call () (:isa :Mesh :faceVertexIndices) 3 3 :ccw :grid-fv-indices nil :make-prop)
  (:call ()
         (:isa :Mesh :points)
         (:isa :Mesh :extent)
         10 (0 3 3) #(2 1 0) :grid-points
         :swapd nil :make-prop
         :-rot nil :make-prop
         :2list :make-group)))
