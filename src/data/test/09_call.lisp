((:meta :upAxis "Y")

 (:call :test-tree-gen)

 (:prim :x
        (:type Sphere)
        (:call :test-gen-xform-info
               #1A (10 0 100)
               #1A (33 55 4))
        (:prop "radius"
         :datum :double #0A 5.0d0))

 (:call :test-gen-cubes 2))
