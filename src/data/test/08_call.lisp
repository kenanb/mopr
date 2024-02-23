((:meta :upAxis "Y")

 (:call :test-tree-gen)

 (:prim :x
        (:type :Sphere)
        (:attr "radius"
         :datum :double #0A 5.0d0))

 (:prim ("c")
        (:type :Mesh)
        (:meta :kind "component")))
