((:group
  (:tree
   ("Grid3x3"))
  (:prim ("Grid3x3")
         (:type :Mesh)
         (:call (prop (:isa :Mesh :faceVertexCounts))
                3 3 :grid-fv-counts prop :make-schema-prop)
         (:call (prop (:isa :Mesh :faceVertexIndices))
                3 3 :cw :grid-fv-indices prop :make-schema-prop)
         (:call (prop-pts (:isa :Mesh :points)
                          prop-ext (:isa :Mesh :extent))
                10 (0 3 3) #(2 0 1) :grid-points :drop
                4 7 0 :grid-oscillate-y :dup 00 :as-timesample prop-pts :make-schema-prop :swap :copy-array
                4 7 7 :grid-oscillate-y :dup 10 :as-timesample prop-pts :make-schema-prop :swap :copy-array
                4 7 0 :grid-oscillate-y :dup 20 :as-timesample prop-pts :make-schema-prop :swap
          :compute-extent prop-ext :make-schema-prop))))
