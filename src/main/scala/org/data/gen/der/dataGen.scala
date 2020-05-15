package org.data.gen.der

object dataGen {

  trait Generator[A] {
    def gen: A
  }

  object basicGenerators {

    implicit val stringGen: Generator[String] = new Generator[String] {

      def gen: String = "random"

    }

    implicit val intGen: Generator[Int] = new Generator[Int] {

      def gen: Int = 0

    }

    implicit val doubleGen: Generator[Double] = new Generator[Double] {

      def gen: Double = 0.0

    }
  }

  object typeParsers {

    import shapeless._
    import basicGenerators._

    implicit val hnilGen: Generator[HNil] = new Generator[HNil] {
      def gen: HNil = HNil
    }

    implicit def hconsGen[H: Generator, T <: HList: Generator]
        : Generator[H :: T] =
      new Generator[H :: T] {
        def gen: H :: T = {
          implicitly[Generator[H]].gen :: implicitly[Generator[T]].gen
        }
      }

    implicit def init[A, R <: HList](
        //entry point from where data type A -> HList
        implicit generic: Generic.Aux[A, R],
        result: Generator[R] //call the implicit generator : hconsGen based on HList
    ): Generator[A] = new Generator[A] {
      def gen: A = generic.from(result.gen) // map all resolutions on A type
    }
  }

}
