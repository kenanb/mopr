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
        (:prop ("testToken" :interp :uniform)
         :token #0A "xformOp:translate")
        (:prop "test2dMatrix2d"
         :array :matrix2d #2A ((0 1 430 -145)))
        (:prop "test3dMatrix2d"
         :array :matrix2d #3A (((0 1) (430 -145)))))

 (:prim x
        (:type Sphere)
        (:prop "radius"
         :double #0A 5.0d0))

 (:prim y
        (:type Sphere)
        (:prop "radius"
         :double #0A 50.0d0))

 (:prim ("c")
        (:type Mesh)
        (:meta :kind "component"))

 (:prim ("c")
        (:prop ("xformOpOrder" :interp :uniform)
         :array :token #1A (("xformOp" "translate")
                            ("xformOp" "rotateXYZ")))
        (:ns "xformOp"
             (:prop "translate"
              :float3 #1A (10 0 100))
             (:prop "rotateXYZ"
              :double3 #1A (33 55 4))))

 (:prim ("c")
        (:prop "extent"
         :array :float3 #2A ((-430 -145 0)
                             (430 145 0)))
        (:prop "faceVertexCounts"
         :array :int #1A (4))
        (:prop "faceVertexIndices"
         :array :int #1A (0 1 2 3))
        (:prop "points"
         :array :point3f #2A ((-430 -145 0)
                              (430 -145 0)
                              (430 145 0)
                              (-430 145 0)))))
