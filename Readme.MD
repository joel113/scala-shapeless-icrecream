# Scala Shapeless Icecream

```
[info] /Users/joel/src/scala-shapeless-icecream/src/main/scala/Main.scala:68:42: hlist.this.Prepend.hnilPrepend0 is not a valid implicit value for shapeless.ops.hlist.Prepend.Aux[shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil,Unaligned] because:
[info] typing TypeApply reported errors for the implicit tree: type arguments [shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil] do not conform to method hnilPrepend0's type parameter bounds [P <: shapeless.HList,S <: shapeless.HNil]
```

```
shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out

shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil

P <: shapeless.HList

S <: shapeless.HNil

```