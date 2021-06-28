![Vapor Logo](docs/logo.png)

<details>
<summary><strong>Table of Contents</strong></summary>

- [Vapors](#vapors)
- [Getting Started](#getting-started)
  * [Setup](#setup)
  * [Complex FactTypes](#complex-facttypes)
  * [Using a FactType Enumeration](#using-a-facttype-enumeration)
- [Terminology and Problem Statement](#terminology-and-problem-statement)
  * [Types of Expressions](#types-of-expressions)
  * [Expression Algebra](#expression-algebra)
  * [Fact Filter DSL](#fact-filter-dsl)
    + [DSL Operators](#dsl-operators)
      - [TerminalFactsExp Builders](#terminalfactsexp-builders)
      - [WhereBuilder Operators](#wherebuilder-operators)
  * [FactTypeSets](#facttypesets)
- [Implementation](#implementation)
  * [Interpreters](#interpreters)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>

</details>

# Vapors

The Vapors library provides an embedded-DSL for writing expressions that look similar to the Scala collections library
and can be interpreted to produce a function that computes the result given a set of source facts.

These expressions are applicative and can be optimized, evaluated, serialized, and interpreted in various ways
using a tagless final recursive visitor.

This library is built on top of Scala's `cats` library for typeclass definitions. See the
[Implementation](#implementation) section for more details.

You might want to also check out these [helpful slides on the library design](
https://docs.google.com/presentation/d/1eMGqw19Kba3Mw7ie36opsmgimFLrvTGfeMtPXkLMuQU/edit) for more details.
_(The slides are a little bit out of date, but the basic ideas are the same)_

# Getting Started

## Setup

1.  **Add it to your `build.sbt`**

    ```sbt
    libraryDependencies += "com.rallyhealth" %% "vapors" % "0.16.0"
    ```

2.  **Define your fact types.**

    Every fact type must have a unique name as well as a Scala type. You can define a `FactType` with any Scala type you
    want, so long as it defines an `Order`. This is so the facts can be pulled from the `FactTable` in a reasonable
    order.

    ```scala
    object FactTypes {
      val DateOfBirth = FactType[LocalDate]("DateOfBirth")
      val Role = FactType[Role]("Role")
    }
    
    sealed trait Role
    object Role {
      case object Admin extends Role
      case object User extends Role
      
      implicit val order: Order[Role] = Order.reverse[Role] {
        Order.by {
          case Admin => 1
          case User => 2
        }
      }
    }
    ```
    
    _Typically, if you have it, you'll order things by timestamp. Stay tuned for a data type that encapsulates
    `Timestamped` facts._
    
3.  **Craft your expressions.**

    You must start with either a `RootExpr` or a logical operator (such as `and` / `or`). You will typically
    build a `RootExpr` by filtering to a specific type of fact and computing something from the facts found.
    Since every `FactType` can have multiple instances defined in the `FactTable`, they must also define an `Order`,
    so that the facts can come out of the `FactTable` in some natural ordering:
    
    ```scala
    import cats.implicits._
    import com.rallyhealth.vapors.dsl._
    
    val isAdmin = {
      factsOfType(FactTypes.Role).exists(_.value === Role.Admin)
    }
    val isOver18 = {
      factsOfType(FactTypes.DateOfBirth).exists { fact =>
        dateDiff(fact.value, today, const(ChronoUnit.YEARS)) >= 18
      }
    }
    ```
    
4.  **Feed your facts and expression into the evaluator.** 

    Assuming you have these facts:
    
    ```scala
    import FactTypes._
    
    val dob = FactTypes.DateOfBirth(LocalDate.of(1980, 1, 1))
    val adminRole = FactTypes.Role(Role.Admin)
    val userRole = FactTypes.Role(Role.User)
    
    val facts = FactTable(
      dob,
      adminRole,
      userRole
    )
    ```
    
    You can then evaluate the expression to get the output and the `Evidence` that was used to prove the result.  
    
    ```scala
    import com.rallyhealth.vapors.dsl._
    
    val isAdminResult = eval(facts) {
      isAdmin
    }
    assert(isAdminResult.output.value)
    assert(isAdminResult.output.evidence == Evidence(adminRole))
    
    val combinedResult = evalWithFacts(facts) {
      and(isAdmin, isOver18)
    }
    assert(combinedResult.output.value)
    assert(combinedResult.output.evidence == Evidence(adminRole, dob))
    ```
    
## Complex FactTypes

The type of operators that are available for a type are dependent on the instances of cats typeclasses
that are available in implicit scope. So for example, if you want to be able to use `<`, `>=`, `===`, etc
then you need to define an `Order` instance for your type.

```scala
case class FeetAndInches(feet: Int, inches: Int)
object FeetAndInches {
  implicit val order: Order[FeetAndInches] = Order.fromLessThan { (l, r) =>
    l.feet < r.feet || (l.feet == r.feet && l.inches < r.inches)
  }
}
```

# Terminology and Problem Statement

In essence, every expression can be optimized and evaluated to produce a function:
```
type Evaluate[A, P] = Expr[FactTable, A, P] => (FactTable => ExprResult[A, P])
```

Note that the type `P` is the captured parameter that can be used when further analyzing
the `ExprResult` and how it came up with the output value and evidence.

```
type Analyze[G[_], A, P] = ExprResult[A, P] => G[A] 
```

## Expression Algebra

<table>
<tr>
  <th>Expr</th>
  <th>DSL Example</th>
  <th>Description</th>
</tr>
<tr>
  <td><code>ConstOutput[V, R, P]</code></td>
  <td><code>const(1)</code></td>
  <td>
    Ignores the input and just returns a constant value.
  </td>
</tr>
<tr>
  <td><code>ReturnInput[V, P]</code></td>
  <td>
    <code>concat(input, input)</code><br/>
    where the input is some collection type that can be concatenated with itself.
  </td>
  <td>
    The identity function. It just returns the input to the expressions. This is vital for the inner workings of the
    builder-style DSL. In an ideal world these would be optimized out of the resulting expression evaluators.
    <br/>
    <b>These are not often used directly.</b>
  </td>
</tr>
<tr>
  <td><code>Embed[V, R, P]</code></td>
  <td>
    <code>concat(stringToList, embed(const(List(1))))</code><br/>
    <br/>
    where <code>stringToList</code> is some <code>Expr[String, List[Int], P]</code>
  </td>
  <td>
    Ignores the input and just passes the <code>FactTable</code> as the result. The only type of expressions that can be
    embedded are <code>RootExpr</code>s which only depend on the <code>FactTable</code>.<br/>
    In order to embed a constant at the same level, you can use the builder's <code>.embedConst()</code> method.
    <br/>
    <b>NOTE: Typically these are applied automatically by implicit conversion</b>
  </td>
</tr>
<tr>
  <td><code>WithFactsOfType[T, R, P]</code></td>
  <td>
    <code>valuesOfType(Age).exists(_ > 21)</code><br/>
    <br/>
    where <code>Age</code> is some <code>FactType[Int]</code>
  </td>
  <td>
    Selects all the facts that have a <code>FactType</code> in the given <code>FactTypeSet</code>. The result of the
    expression will be a <code>FoldableExprBuilder</code> that can be filtered, mapped, etc to produce some
    typed computation result.
  </td>
</tr>
<tr>
  <td><code>Definition[P]</code></td>
  <td>N / A</td>
  <td>
    Not an expression node you can construct, but rather a sealed trait that hides the type parameters so that multiple
    definitions can be put into a list. The only subclass is the <code>Expr.Define</code> node.
  </td>
</tr>
<tr>
  <td><code>Define[M[_] : Foldable, T, P]</code></td>
  <td><code>define(Age).from(const(30))</code></td>
  <td>
    Creates a <code>Definition</code> for a given <code>FactType</code> with a given <code>Expr</code> node. These can
    be used in the <code>UsingDefinitions</code> node.
  </td>
</tr>
<tr>
  <td><code>UsingDefinitions[V, R, P]</code></td>
  <td><code>usingDefinitions(ageDef) { valuesOfType(Age).exists(_ > 21) }</code></td>
  <td>
    Adds the given definitions to the <code>FactTable</code> and makes them available to the expression provided in the
    following block. 
  </td>
</tr>
<tr>
  <td><code>And[V, R, P]</code></td>
  <td><code>and(const(true), const(false))</code></td>
  <td>
    Combine expressions that return a value for which there is an <code>Conjunction</code> and combines the results
    using the given definition for conjunction. <code>Evidence</code> defines <code>Conjunction</code> by returning
    the union of the evidence of all expressions that are falsy if any expressions are falsy, otherwise the union
    of all the evidence of all the given expressions that are truthy.
  </td>
</tr>
<tr>
  <td><code>Or[V, R, P]</code></td>
  <td><code>or(const(true), const(false))</code></td>
  <td>
    Combine expressions that return a value for which there is an <code>Disjunction</code> and combines the results
    using the given definition for disjunction. <code>Evidence</code> defines <code>Disjunction</code> by returning
    the union of the evidence of all expressions that are truthy if any expressions are truthy, otherwise the union
    of all the evidence of all the given expressions that are falsy.
  </td>
</tr>
<tr>
  <td><code>Not[V, R, P]</code></td>
  <td><code>not(const(true))</code></td>
  <td>
    Convert a falsy expression to a truthy expression (or vice versa) using a provided concept of <code>Negation</code>
    The <code>Evidence</code> never be impacted by this operation.
  </td>
</tr>
<tr>
  <td><code>When[V, R, P]</code></td>
  <td><code>when(const(true)).thenReturn(const(1)).elseReturn(const(0))</code></td>
  <td>
    Evaluates the first subexpression of the given sequence of condition branch expressions that evaluates to
    <code>true</code>.
  </td>
</tr>
<tr>
  <td><code>SelectFromOutput[V, S, R, P]</code></td>
  <td><code>const(Map("example" -> "value")).withOutputValue.get(_.at("example"))</code></td>
  <td>
    Select a field from a product type, map, or sequence. Returns either an <code>Option</code> or strict value
    depending on the type of <code>Indexed</code> instance is available for the type.
  </td>
</tr>
<tr>
  <td><code>FilterOutput[V, M[_] : Foldable : FunctorFilter, R, P]</code></td>
  <td><code>valuesOfType(Age).filter(_ > 21)</code></td>
  <td>
    Keeps elements of the given <code>Functor</code> that match the given expression and discards the others. 
  </td>
</tr>
<tr>
  <td><code>MapOutput[V, M[_] : Foldable : Functor, U, R, P]</code></td>
  <td><code>const(List(1, 2, 3, 4)).withOutputFoldable.map(_ * 2)</code></td>
  <td>
    For every value in the given <code>Functor</code>, apply the given expression.
  </td>
</tr>
<tr>
  <td><code>GroupOutput[V, M[_] : Foldable, U : Order, K, P]</code></td>
  <td><code>valuesOfType(FactTypes.Prediction).groupBy(_.get(_.select(_.model)))</code></td>
  <td>
    Group the values of the <code>Foldable</code> by key located at the given <code>NamedLens</code> and return the
    <code>MapView</code>.
  </td>
</tr>
<tr>
  <td><code>SortOutput[V, M[_], R, P]</code></td>
  <td><code>const(List(2, 4, 3, 1)).withOutputFoldable.sorted</code></td>
  <td>
    Sort the values using the given <code>ExprSorter</code> -- either a given natural <code>Order[R]</code> of the
    return type or the <code>Order</code> of a field selected by a given <code>NamedLens</code>).
  </td>
</tr>
<tr>
  <td><code>ConcatOutput[V, M[_] : MonoidK, R, P]</code></td>
  <td><code>concat(const(List(1, 2)), const(List(3, 4)))</code></td>
  <td>
    Concatenates the output of the given expressions that return the same <code>MonoidK[M]</code> to produce a single
    <code>M[R]</code> with all elements of all the monoids in the given order.
  </td>
</tr>
<tr>
  <td><code>FoldOutput[V, M[_] : Foldable, R : Monoid, P]</code></td>
  <td><code>const(List("hello", "world")).withOutputFoldable.fold</code></td>
  <td>
    Folds the output of the given expressions into a single value of the same type, given a <code>Monoid[R]</code>.
  </td>
</tr>
<tr>
  <td><code>WrapOutputSeq[V, R, P]</code></td>
  <td><code>wrapSeq(const(1), const(2), const(3), const(4))</code></td>
  <td>
    Wraps the output of the sequence of given expressions into a sequence of the results.
  </td>
</tr>
<tr>
  <td><code>WrapOutputHList[V, L &lt;: HList, R, P]</code></td>
  <td><code>wrap(const(1), const("two")).zippedToShortest.asTuple</code></td>
  <td>
    Wraps the output of a heterogeneous list of given expressions into an <code>HList</code> of the return types.
  </td>
</tr>
<tr>
  <td><code>ZipOutput[V, M[_] : Align : FunctorFilter, L &lt;: HList, R, P]</code></td>
  <td><code></code></td>
  <td>
    Zips a heterogeneous list of expressions into a single <code>HList</code> of the results.
  </td>
</tr>
<tr>
  <td><code>OutputIsEmpty[V, M[_] : Foldable, R, P]</code></td>
  <td><code></code></td>
  <td>
    Returns <code>true</code> if the output of the expression is an empty <code>Foldable</code>, otherwise
    <code>false</code>.
  </td>
</tr>
<tr>
  <td><code>TakeFromOutput[V, M[_] : Traverse : TraverseFilter, R, P]</code></td>
  <td><code></code></td>
  <td>
    Takes a given number of elements from the front of the traversable result.
  </td>
</tr>
<tr>
  <td><code>ExistsInOutputV, M[_] : Foldable, U, P]</code></td>
  <td><code>valuesOfType(Age).exists(_ > 18)</code></td>
  <td>
    Returns <code>true</code> if there exists an element in the given <code>Foldable</code> for which the given
    predicate expression returns <code>true</code>, otherwise returns <code>false</code>.
  </td>
</tr>
<tr>
  <td><code>AddOutputs[V, R : Addition, P]</code></td>
  <td><code>const(1) + const(1)</code></td>
  <td>
    Adds the output of the given expressions using the definition of <code>Addition</code> provided for the output type.
  </td>
</tr>
<tr>
  <td><code>SubtractOutputs[V, R : Subtraction, P]</code></td>
  <td><code>const(1) - const(1)</code></td>
  <td>
    Subtracts the output of the given expressions using the definition of <code>Subtration</code> provided for the
    output type.
  </td>
</tr>
<tr>
  <td><code>MultiplyOutputs[V, R : Multiplication, P]</code></td>
  <td><code>const(1) * const(1)</code></td>
  <td>
    Multiplies the output of the given expressions using the definition of <code>Multiplication</code> provided for the
    output type.
  </td>
</tr>
<tr>
  <td><code>DivideOutputs[V, R : Division, P]</code></td>
  <td><code>const(1) / const(1)</code></td>
  <td>
    Multiplies the output of the given expressions using the definition of <code>Multiplication</code> provided for the
    output type.
  </td>
</tr>
<tr>
  <td><code>NegativeOutput[V, R : Negative, P]</code></td>
  <td><code>-const(1)</code></td>
  <td>
    Converts the output of the given expression using the definition of <code>Negative</code> to negate the value.<br/>
    <br/>
    Note: This is different than <code>Negation</code> in that it is an arithmetic operation not a logical operation.
  </td>
</tr>
<tr>
  <td><code>OutputWithinSet[V, R, P]</code></td>
  <td><code>const(1) in Set(1, 2, 3)</code></td>
  <td>
    Returns <code>true</code> if the output of the expression is found in the given <code>Set[R]</code> of values,
    otherwise <code>false</code>.
  </td>
</tr>
<tr>
  <td><code>OutputWithinWindow[V, R, P]</code></td>
  <td><code>const(1) <= 2</code></td>
  <td>
    Returns <code>true</code> if the output of the expression is contained by the given <code>Window[R]</code>,
    otherwise <code>false</code>.
  </td>
</tr>
</table>

## Expression Type Aliases

<table>
<tr>
  <th>Type</th>
  <th>Description</th>
  <th>Definition</th>
</tr>
<tr>
  <td><code>Expr[V, R, P]</code></td>
  <td>The super class of all expression nodes.</td>
  <td>N / A</td>
</tr>
<tr>
  <td><code>RootExpr[R, P]</code></td>
  <td>
    An expression that can operate on a <code>FactTable</code> alone to produce a value of type `R`.
    Typically built using a <code>factsOfType</code> expression builder.
  </td>
  <td>
    <code>Expr[FactTable, R, P]</code>
  </td>
</tr>
<tr>
  <td><code>Definition[P]</code></td>
  <td>
    A sealed trait that contains a <code>FactType</code> and its appropriately typed definition. The only subclass of
    this type is the <code>Expr.Define</code> expression node. 
  </td>
  <td>
    <code>[M[_], T] ==> Expr.Define[M, T, P] => Expr[FactTable, M[T], P]</code>
  </td>
</tr>
<tr>
  <td><code>CondExpr[V, P]</code></td>
  <td>
    A conditional expression that returns a <code>Boolean</code>.
  </td>
  <td>
    <code>Expr[T, Boolean, P]</code>
  </td>
</tr>
</table>

## FactTypeSets

Since `FactType`s have both a Scala type parameter and a `String` name, it is possible to handle multiple facts with
the same Scala type, but different names. Typically, you should only do this if these `FactType`s are all defined
with the same underlying meaning (although, they may differ in souce or quality). In this case, you can filter an
expression to a `FactTypeSet`.

## Expression Builders

In order to make building the `Expr` algebra easier, we use expression builders (`ExprBuilder[V, M[_], U, P]`). An
expression builder carries type information from the output of one expression node into a function that can be used
to build an expression that depends on it as input. These functions are designed to look like standard Scala collections
except that they operate over an "Applicative" data structure, rather than an eagerly evaluated monadic collection.

To get started, you must import the DSL:

```scala
import com.rallyhealth.vapors.dsl._
```

There are two types of builders: `ValExprBuilder` and `FoldableExprBuilder` (does not always require a `Foldable`
instance, but it is the most common typeclass constraint of the operations).

You use the `ValExprBuilder` for operations on a single value, like comparison operations, selecting fields, etc.
The also implicitly convert to an `Expr` of the same type when needed.

```scala
val a: ValExprBuilder[FactTable, Int, Unit] = const(1).withOutputValue
val b: ValExprBuilder[FactTable, Boolean, Unit] = a >= 1
val c: ValExprBuilder[FactTable, LocalDate, Unit] = const(LocalDate.now()).withOutputValue
val d: ValExprBuilder[FactTable, Int, Unit] = c.get(_.select(_.getYear))
val e: Expr[FactTable, Int, Unit] = d // implicitly converted
```

You use the `FoldableExprBuilder` to perform operations that can produce other foldable structures or fold the values
into a `ValExprBuilder`. These do not always implicitly convert properly to an `Expr`. There is more work to be done
here to make this builder syntax more seemless.

```scala
val f: FoldableExprBuilder[FactTable, Seq, Int, Unit] = const(Seq(1, 2, 3, 4)).withOutputFoldable
val g: FoldableExprBuilder[FactTable, Seq, Int, Unit] = f.filter(_ < 2).map(_ * 2)
val h: ValExprBuilder[FactTable, Boolean, Unit] = g.exists(_ === 8)
```

As you can see, the builders are designed to look like the Scala collections libraries, but are implemented using
typeclass definitions from the `cats-core` library.

### Example

The following a more complete example of a set of facts and an evaluated query.

```scala
import cats.Order
import com.rallyhealth.vapors.dsl._

case class SemVer(major: Int, minor: Int, patch: Int)
object SemVer {
  implicit val order: Order[SemVer] = Order.fromLessThan { (l, r) =>
    l.major < r.major || l.major == r.major && l.minor < r.minor || l.major == r.major && l.minor == r.minor && l.patch < r.patch
  }
}

case class PredictionModel(
  modelName: String,
  modelVersion: SemVer,
  score: Double
)

object FactTypes {
  val DateOfBirth = FactType[LocalDate]("date_of_birth")
  val HeightFt = FactType[FeetAndInches]("height_ft")
  val WeightLbs = FactType[Int]("weight_lbs")
  val Prediction = FactType[PredictionModel]("prediction")
}

object JoeSchmoe {
  val height = FactTypes.HeightFt(FeetAndInches(5, 8))
  val weight = FactTypes.WeightLbs(260)
  val dateOfBirth = FactTypes.DateOfBirth(LocalDate.of(1980, 1, 1))
  val predictWeightloss = FactTypes.Prediction(PredictionModel("weightloss", SemVer(2, 0, 1), 0.85))
  val facts = Facts(height, weight, dateOfBirth, predictWeightloss)
}

object Example {
  val query: RootExpr[Boolean, Unit] = {
    or(
      and(
        valuesOfType(FactTypes.WeightLbs).exists(_ > 250),
        valuesOfType(FactTypes.HeightFt).exists(_ < FeetAndInches(6, 0))
      ),
      and(
        valuesOfType(FactTypes.WeightLbs).exists(_ > 300),
        valuesOfType(FactTypes.HeightFt).exists(_ >= FeetAndInches(6, 0))
      ),
      valuesOfType(FactTypes.Prediction).exists { prediction =>
        and(
          prediction.get(_.select(_.modelName)) === "weightloss",
          prediction.get(_.select(_.score)) > 0.8,
          prediction.get(_.select(_.modelVersion)) >= SemVer(2, 0, 0)
        )
      }
    )
  }
  val rs = eval(JoeSchmoe.facts)(query)
  assert(rs.isTrue)
  assert(rs.matchingFacts == Facts(JoeSchmoe.height, JoeSchmoe.weight, JoeSchmoe.predictWeightloss))
}
```

# Implementation

## Interpreters

In order to convert from an `Expr` into something useful, we must interpret the expression.

The main interpreter is the `InterpretExprAsResultFn` which converts an `Expr[V, R, P]` into a function
`ExprInput[V, P] => ExprResult[V, R, P]`, which can then be evaluated by providing the appropriate input. The
standard `eval` requires a `RootExpr` which takes a `FactTable` as input and produces some arbitrary output. You
can get both the value

We provide a tagless-final encoded `Expr.Visitor` that you can extend to interpret `Expr` expressions built by our
embedded DSL. By extending this interface, you must handle each case of the `Expr` algebra in separate methods.

For example, we can interpret an `Expr` as `Json` by providing an interpreter that looks like:

```scala
import io.circe.Json

object VisitExprAsJson {
  type G[_] = Json
}

class VisitExprAsJson[V] extends Expr.Visitor[VisitExprAsJson.G, V, Unit] {
  override def visitConstOutput[R](expr: ConstOutput[V, R, P]): Json = Json.fromString(expr.value.toString)
  // ... implement all other visitX methods to produce Json
}
```

If you want to handle all expressions similarly and don't need any of the typeclass instances, you can use the
`VisitGenericExprWithProxyFn` and just define the `visitGeneric` method.

```scala
object VisitExprAsString {
  type G[_] = String
}

class VisitExprAsString[V] extends VisitGenericExprWithProxyFn[V, Unit, VisitExprAsString.G] {
  override def visitGeneric[U, R](
    expr: Expr[U, R, P],
    input: ExprInput[U],
  ): String = s"$input -> $expr"
}
```
