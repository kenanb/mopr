((:meta :upAxis "Y")

 (:call () :test-tree-gen)

 (:prim :x
        (:type :Sphere)
        (:call ()
               #1A (10 0 100)
               #1A (33 55 4)
               :test-gen-xform-info)
        (:attr "radius"
         :datum :double #0A 5.0d0))

 (:call () 2 :test-gen-cubes))
