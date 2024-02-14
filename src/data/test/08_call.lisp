((:meta :upAxis "Y")

 (:call :test-tree-gen)

 (:prim :x
        (:type Sphere)
        (:prop "radius"
         :double #0A 5.0d0))

 (:prim ("c")
        (:type Mesh)
        (:meta :kind "component")))
