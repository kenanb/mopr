((:tree
  ("Grid3x3"))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (prop (:isa :Mesh :faceVertexCounts))
   3 3 :grid-fv-counts prop :make-schema-prop)
  (:call (prop (:isa :Mesh :faceVertexIndices))
   3 3 :cw :grid-fv-indices prop :make-schema-prop)
  (:call (prop-pts (:isa :Mesh :points)
          prop-ext (:isa :Mesh :extent)
          dim 4 len 7 val-0 0 val-1 7)
   10 (0 3 3) #(2 0 1) :grid-points :drop
   dim len val-0 :grid-oscillate-y :dup 00 :as-timesample prop-pts :make-schema-prop :swap :copy-array
   dim len val-1 :grid-oscillate-y :dup 10 :as-timesample prop-pts :make-schema-prop :swap :copy-array
   dim len val-0 :grid-oscillate-y :dup 20 :as-timesample prop-pts :make-schema-prop :swap
   :compute-extent prop-ext :make-schema-prop)))
