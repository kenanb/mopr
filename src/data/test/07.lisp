((:meta :upAxis "Y")

 (:tree
  ("a" :spec :class)
  ("b"
   ("d"
    ("e" :spec :over :alias x)
    ("f" :alias y)))
  ("c"))

 (:prim ("a"))

 (:prim ("b")
        (:type Xform))

 (:prim ("b" "d")
        (:type Xform)
        (:attr ("testToken" :interp :uniform)
         :datum :token #0A "xformOp:translate")
        (:attr "test2dMatrix2d"
         :array :matrix2d #2A ((0 1 430 -145)))
        (:attr "test3dMatrix2d"
         :array :matrix2d #3A (((0 1) (430 -145)))))

 (:prim x
        (:type Sphere)
        (:attr "radius"
         :datum :double #0A 5.0d0))

 (:prim y
        (:type Sphere)
        (:attr "radius"
         :datum :double #0A 50.0d0))

 (:prim ("c")
        (:type Mesh)
        (:meta :kind "component"))

 (:prim ("c")
        (:attr ("xformOpOrder" :interp :uniform)
         :array :token #1A (("xformOp" "translate")
                            ("xformOp" "rotateXYZ")))
        (:ns "xformOp"
             (:attr "translate"
              :datum :float3 #1A (10 0 100))
             (:attr "rotateXYZ"
              :datum :double3 #1A (33 55 4))))

 (:prim ("c")
        (:attr "extent"
         :array :float3 #2A ((-430 -145 0)
                             (430 145 0)))
        (:attr "faceVertexCounts"
         :array :int #1A (4))
        (:attr "faceVertexIndices"
         :array :int #1A (0 1 2 3))
        (:attr "points"
         :array :point3f #2A ((-430 -145 0)
                              (430 -145 0)
                              (430 145 0)
                              (-430 145 0)))))
