((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   3 3 :grid-fv-counts nil prop :make-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   3 3 :cw :grid-fv-indices nil prop :make-prop)
  (:call (prop-pts (:isa :Mesh :points)
          prop-ext (:isa :Mesh :extent))
   10 (0 3 3) #(2 0 1) :grid-points :drop
   2 2 :add :dup 2
   :mul 1 :sub 2 :mul 2 :div :swap
   :dup :drop :2dup :2drop :dupd
   :nip :dup :swapd :drop
   0 :grid-oscillate-y :dup 00 prop-pts :make-prop :swap :copy-array
   4 7 7 :grid-oscillate-y :dup 10 prop-pts :make-prop :swap :copy-array
   4 7 0 :grid-oscillate-y :dup 20 prop-pts :make-prop :swap
   :compute-extent nil prop-ext :make-prop
   :4list :make-group)))
