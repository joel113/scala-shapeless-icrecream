# Scala Shapeless Icecream

## Shapeless

https://github.com/milessabin/shapeless

https://github.com/underscoreio/shapeless-guide

https://books.underscore.io/shapeless-guide/shapeless-guide.html

https://github.com/typelevel/shapeless-3

https://typelevel.org/

https://www.tobyhobson.com/posts/shapeless/

http://caryrobbins.com/dev/merging-case-classes/

https://stackoverflow.com/questions/31681784/using-shapeless-scala-to-merge-the-fields-of-two-different-case-classes

https://jto.github.io/articles/getting-started-with-shapeless/

https://www.baeldung.com/scala/generic-programming

## Logs

```
[info] Icecream.scala:67:42: shapeless.this.Generic.materialize is not a valid implicit value for shapeless.Generic.Aux[Icecream.IceCreamV1,V] because:
[info] type parameters weren't correctly instantiated outside of the implicit tree: inferred type arguments [String :: Int :: Boolean :: shapeless.HNil,Nothing] do not conform to method materializeCoproduct's type parameter bounds [V <: shapeless.Coproduct,R <: shapeless.Coproduct]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: shapeless.this.LabelledGeneric.materializeCoproduct is not a valid implicit value for shapeless.LabelledGeneric.Aux[Icecream.IceCreamV1,ARepr] because:
[info] hasMatchingSymbol reported error: could not find implicit value for parameter gen: shapeless.Generic.Aux[Icecream.IceCreamV1,V]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: shapeless.this.Generic.materialize is not a valid implicit value for shapeless.Generic.Aux[Icecream.IceCreamV2c,V] because:
[info] type parameters weren't correctly instantiated outside of the implicit tree: inferred type arguments [String :: Boolean :: Int :: Int :: shapeless.HNil,Nothing] do not conform to method materializeCoproduct's type parameter bounds [V <: shapeless.Coproduct,R <: shapeless.Coproduct]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: shapeless.this.LabelledGeneric.materializeCoproduct is not a valid implicit value for shapeless.LabelledGeneric.Aux[Icecream.IceCreamV2c,BRepr] because:
[info] hasMatchingSymbol reported error: could not find implicit value for parameter gen: shapeless.Generic.Aux[Icecream.IceCreamV2c,V]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: hlist.this.Prepend.hnilPrepend1 is not a valid implicit value for shapeless.ops.hlist.Prepend.Aux[shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil,Unaligned] because:
[info] typing TypeApply reported errors for the implicit tree: type arguments [shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil] do not conform to method hnilPrepend1's type parameter bounds [P <: shapeless.HNil,S <: shapeless.HList]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: hlist.this.Prepend.hnilPrepend0 is not a valid implicit value for shapeless.ops.hlist.Prepend.Aux[shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil,Unaligned] because:
[info] typing TypeApply reported errors for the implicit tree: type arguments [shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil] do not conform to method hnilPrepend0's type parameter bounds [P <: shapeless.HList,S <: shapeless.HNil]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: hlist.this.Remove.recurse is not a valid implicit value for shapeless.ops.hlist.Remove.Aux[shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.HNil,shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int],(shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int], OutT)] because:
[info] hasMatchingSymbol reported error: could not find implicit value for parameter r: shapeless.ops.hlist.Remove.Aux[shapeless.HNil,shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int],(shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int], OutT)]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: hlist.this.Remove.recurse is not a valid implicit value for shapeless.ops.hlist.Remove.Aux[shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.HNil,shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int],(shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int], R)] because:
[info] hasMatchingSymbol reported error: could not find implicit value for parameter r: shapeless.ops.hlist.Remove.Aux[shapeless.HNil,shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int],(shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int], OutT)]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]

```

## Implicit Log Investigation

No idea.

## StackOverflow

https://stackoverflow.com/questions/72588264/scala-shapeless-no-implicits-found-case-class-migration-adding-new-field

Scala Shapeless No Implicits Found Case Class Migration Adding New Field

In order to get familiar with shapeless, I did investigate into the examples of the [shapeless-guide](https://books.underscore.io/shapeless-guide/shapeless-guide.html). I am especially interested in case class migrations. Therefore, I did dive into the case study of the case class migration section of the [shapeless-guide](https://books.underscore.io/shapeless-guide/shapeless-guide.html).

Unfortunately, the code of the case class migration case study is not compiling as the last conversion where a field is added is failing as no implicits can be found.

I am trying to understand the issue with the code. However, understanding the issue is quite difficult and I failed so far. Hence, i would like to ask whether somebody can give an hint.

I tried also several compilers, i.e. scala 2.12, scala 2.13.

I did run the compiler with the `-Xlog-implicits` flag which gives several error messages:

```
[info] Icecream.scala:67:42: shapeless.this.Generic.materialize is not a valid implicit value for shapeless.Generic.Aux[Icecream.IceCreamV1,V] because:
[info] type parameters weren't correctly instantiated outside of the implicit tree: inferred type arguments [String :: Int :: Boolean :: shapeless.HNil,Nothing] do not conform to method materializeCoproduct's type parameter bounds [V <: shapeless.Coproduct,R <: shapeless.Coproduct]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: shapeless.this.LabelledGeneric.materializeCoproduct is not a valid implicit value for shapeless.LabelledGeneric.Aux[Icecream.IceCreamV1,ARepr] because:
[info] hasMatchingSymbol reported error: could not find implicit value for parameter gen: shapeless.Generic.Aux[Icecream.IceCreamV1,V]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: shapeless.this.Generic.materialize is not a valid implicit value for shapeless.Generic.Aux[Icecream.IceCreamV2c,V] because:
[info] type parameters weren't correctly instantiated outside of the implicit tree: inferred type arguments [String :: Boolean :: Int :: Int :: shapeless.HNil,Nothing] do not conform to method materializeCoproduct's type parameter bounds [V <: shapeless.Coproduct,R <: shapeless.Coproduct]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: shapeless.this.LabelledGeneric.materializeCoproduct is not a valid implicit value for shapeless.LabelledGeneric.Aux[Icecream.IceCreamV2c,BRepr] because:
[info] hasMatchingSymbol reported error: could not find implicit value for parameter gen: shapeless.Generic.Aux[Icecream.IceCreamV2c,V]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: hlist.this.Prepend.hnilPrepend1 is not a valid implicit value for shapeless.ops.hlist.Prepend.Aux[shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil,Unaligned] because:
[info] typing TypeApply reported errors for the implicit tree: type arguments [shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil] do not conform to method hnilPrepend1's type parameter bounds [P <: shapeless.HNil,S <: shapeless.HList]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: hlist.this.Prepend.hnilPrepend0 is not a valid implicit value for shapeless.ops.hlist.Prepend.Aux[shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil,Unaligned] because:
[info] typing TypeApply reported errors for the implicit tree: type arguments [shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.ops.hlist.ZipWithKeys.hnilZipWithKeys.Out,shapeless.labelled.FieldType[Symbol @@ String("name"),String] :: shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.labelled.FieldType[Symbol @@ String("inCone"),Boolean] :: shapeless.HNil] do not conform to method hnilPrepend0's type parameter bounds [P <: shapeless.HList,S <: shapeless.HNil]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: hlist.this.Remove.recurse is not a valid implicit value for shapeless.ops.hlist.Remove.Aux[shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int] :: shapeless.HNil,shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int],(shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int], OutT)] because:
[info] hasMatchingSymbol reported error: could not find implicit value for parameter r: shapeless.ops.hlist.Remove.Aux[shapeless.HNil,shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int],(shapeless.labelled.FieldType[Symbol @@ String("numCherries"),Int], OutT)]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
[info]                                          ^
[info] Icecream.scala:67:42: hlist.this.Remove.recurse is not a valid implicit value for shapeless.ops.hlist.Remove.Aux[shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int] :: shapeless.HNil,shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int],(shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int], R)] because:
[info] hasMatchingSymbol reported error: could not find implicit value for parameter r: shapeless.ops.hlist.Remove.Aux[shapeless.HNil,shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int],(shapeless.labelled.FieldType[Symbol @@ String("numWaffles"),Int], OutT)]
[info]   IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
```
The full code is as following:

```
  case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)
  case class IceCreamV2a(name: String, inCone: Boolean)
  case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
  case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)

  trait Migration[A, B] {
    def apply(a: A): B
  }

  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration.apply(a)
  }

  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
    new Monoid[A] {
      def empty = zero
      def combine(x: A, y: A): A = add(x, y)
    }

  implicit val hnilMonoid: Monoid[HNil] = createMonoid[HNil](HNil)((x, y) => HNil)

  implicit def emptyHList[K <: Symbol, H, T <: HList](
     implicit
     hMonoid: Lazy[Monoid[H]],
     tMonoid: Monoid[T]
   ): Monoid[FieldType[K, H] :: T] =
    createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) {
      (x, y) =>
        field[K](hMonoid.value.combine(x.head, y.head)) ::
          tMonoid.combine(x.tail, y.tail)
    }

  implicit def genericMigration[
    A, B, ARepr <: HList, BRepr <: HList,
    Common <: HList, Added <: HList, Unaligned <: HList
  ](
     implicit
     aGen: LabelledGeneric.Aux[A, ARepr],
     bGen: LabelledGeneric.Aux[B, BRepr],
     inter: hlist.Intersection.Aux[ARepr, BRepr, Common],
     diff: hlist.Diff.Aux[BRepr, Common, Added],
     monoid: Monoid[Added],
     prepend: hlist.Prepend.Aux[Added, Common, Unaligned],
     align: hlist.Align[Unaligned, BRepr]
   ): Migration[A, B] =
    new Migration[A, B] {
      def apply(a: A): B = bGen.from(align(prepend(monoid.empty, inter(aGen.to(a)))))
    }

  IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2a]
  // res14: IceCreamV2a = IceCreamV2a(Sundae,true)

  IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2b]
  // res15: IceCreamV2b = IceCreamV2b(Sundae,true,1)

  IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c] // <- fails as implicit can not be found
  // res16: IceCreamV2c = IceCreamV2c(Sundae,true,1,0)
```

[1]: https://books.underscore.io/shapeless-guide/shapeless-guide.html

[1]: https://books.underscore.io/shapeless-guide/shapeless-guide.html
