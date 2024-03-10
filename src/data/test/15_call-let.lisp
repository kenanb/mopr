((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (:prop (:isa :Mesh :faceVertexCounts))
   :prop 3 3 :grid-fv-counts nil :make-prop)
  (:call (:prop (:isa :Mesh :faceVertexIndices))
   :prop 3 3 :cw :grid-fv-indices nil :make-prop)
  (:call (:prop-pts (:isa :Mesh :points)
          :prop-ext (:isa :Mesh :extent)
          :dim 4 :len 7 :val-0 0 :val-1 7)
   :prop-pts
   10 (0 3 3) #(2 0 1) :grid-points :drop
   :dim :len :val-0 :grid-oscillate-y :2dup 00 :make-prop :-rot :copy-array
   :dim :len :val-1 :grid-oscillate-y :2dup 10 :make-prop :-rot :copy-array
   :dim :len :val-0 :grid-oscillate-y :2dup 20 :make-prop :-rot
   :compute-extent :nip :prop-ext :swap nil :make-prop)))
