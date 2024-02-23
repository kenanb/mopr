((:meta)
 (:tree
  ("a" :spec :class)
  ("b"
   ("d"
    ("e" :spec :over :alias x)))
  ("c"))
 (:prim ("c")
        (:type :Cube))
 (:prim ("b")
        (:type :Xform))
 (:prim ("b" "d")
        (:type :Xform))
 (:prim x
        (:type :Sphere))
 (:prim ("a")))
