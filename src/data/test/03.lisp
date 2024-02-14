((:meta)

 (:tree
  ("a" :spec :class)
  ("b"
   ("d"
    ("e" :spec :over)))
  ("c"))

 (:prim ("a"))

 (:prim ("b")
        (:type Xform))

 (:prim ("b" "d")
        (:type Xform))

 (:prim ("b" "d" "e")
        (:type Sphere))

 (:prim ("c")
        (:type Cube)))
