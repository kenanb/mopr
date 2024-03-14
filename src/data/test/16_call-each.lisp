((:tree
  ("Grid3x3"))
 (:var :dim () 4)
 (:var :len () 7)
 (:var :prop-pts () (:isa :Mesh :points))
 (:var :prop-ext () (:isa :Mesh :extent))
 (:var :base-grid-points ()
  10 (0 3 3) #(2 0 1) :grid-points :drop)
 (:each :oscillation-args
        (:val 0 :time 00)
        (:val 7 :time 10)
        (:val 0 :time 20))
 (:prim ("Grid3x3")
  (:type :Mesh)
  (:call (:prop (:isa :Mesh :faceVertexCounts))
   3 3 :grid-fv-counts nil :prop :make-prop)
  (:call (:prop (:isa :Mesh :faceVertexIndices))
   3 3 :cw :grid-fv-indices nil :prop :make-prop)
  (:call :oscillation-args
   :base-grid-points :dim :len :val :grid-oscillate-y
   :dup :time :prop-pts :make-prop
   :swap :compute-extent :time :prop-ext :make-prop)))
