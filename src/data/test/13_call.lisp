((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (:prop (:isa :Mesh :faceVertexCounts))
   :prop 3 3 :grid-fv-counts nil :make-prop)
  (:call (:prop (:isa :Mesh :faceVertexIndices))
   :prop 3 3 :ccw :grid-fv-indices nil :make-prop)
  (:call (:prop-pts (:isa :Mesh :points)
          :prop-ext (:isa :Mesh :extent))
   :prop-pts
   :prop-ext
   10 (0 3 3) #(2 1 0) :grid-points
   :swapd nil :make-prop
   :-rot nil :make-prop
   :2list :make-group)))
