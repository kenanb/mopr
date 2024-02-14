((:meta)

 (:tree
  ("a" :spec :class)
  ("b"
   ("d"
    ("e" :spec :over :alias x)))
  ("c"))

 (:prim x
        (:type Sphere))

 (:prim ("a"))

 (:prim ("b")
        (:type Xform))

 (:prim ("b" "d")
        (:type Xform))

 (:prim ("c")
        (:type Cube)))
