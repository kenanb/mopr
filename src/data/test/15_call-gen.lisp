((:tree
  ("Grid3x3"))

 (:prim ("Grid3x3")

  (:type :Mesh)

  (:call 3 3 :grid-fv-counts)

  (:call 3 3 :cw :grid-fv-indices)

  (:call 10 (0 3 3) #(2 0 1)
   :grid-points 2 2 :add :dup 2
   :mul 1 :sub 2 :mul 2 :div :swap
   :dup :drop :2dup :2drop :dupd
   :nip :dup :swapd :drop :swap
   :test-grid-oscillate)))
