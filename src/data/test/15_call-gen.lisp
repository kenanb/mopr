((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (:isa :Mesh :faceVertexCounts) 3 3 :grid-fv-counts nil :make-prop)
  (:call (:isa :Mesh :faceVertexIndices) 3 3 :cw :grid-fv-indices nil :make-prop)
  (:call (:isa :Mesh :points)
         10 (0 3 3) #(2 0 1) :grid-points :drop
         2 2 :add :dup 2
         :mul 1 :sub 2 :mul 2 :div :swap
         :dup :drop :2dup :2drop :dupd
         :nip :dup :swapd :drop
         0 :grid-oscillate-y :2dup 00 :make-prop :-rot :copy-array
         4 7 7 :grid-oscillate-y :2dup 10 :make-prop :-rot :copy-array
         4 7 0 :grid-oscillate-y :2dup 20 :make-prop :-rot
         :compute-extent :nip (:isa :Mesh :extent) :swap nil :make-prop
         :4list :make-group)))
