namespace GasDay.Core

open System
open GasDay.Core.Functional
open System.Diagnostics.CodeAnalysis
open FSharpx
open SafetyFirst
open SafetyFirst.Numbers

[<AutoOpen>]
module Interval = 

  [<AutoOpen>]
  module ErrorTypes =
    type SeparatedIntervals = SeparatedIntervals of string
    let unionSeparatedErr = SeparatedIntervals <| "The union does not exist because the intervals are not connected."

  type IntersectionType =
    ///  <remarks>
    ///  <para> This: (...)......... </para>
    ///  <para> That: ..(...)....... </para>
    ///  <para> OR: </para>                        
    ///  <para> This: ....(...)..... </para>
    ///  <para> That: ..(...)....... </para>
    ///  </remarks>
    | Overlapping

    ///  <remarks>
    ///  <para> This: ....(...)..... </para>
    ///  <para> That: ..(......).... </para>
    ///  </remarks>
    | SubInterval

    ///  <remarks>
    ///  <para> This: (......)...... </para>
    ///  <para> That: ..(...)....... </para>
    ///  </remarks>
    | SuperInterval

    ///  <remarks>
    ///  <para> This: ..(......).... </para>
    ///  <para> That: ..(......).... </para>
    ///  </remarks>
    | Identical 

  type IntervalRelationship = 
    ///  <remarks>
    ///  <para> This: (...)......... </para>
    ///  <para> That: .......(...).. </para>
    ///  <para> OR: </para>
    ///  <para> This: .......(...).. </para>
    ///  <para> That: (...)......... </para>
    ///  <para> OR: </para>
    ///  <para> This: .......(...).. </para>
    ///  <para> That: ..(...)....... </para>
    ///  </remarks>
    | Disjoint  
    | Intersecting of IntersectionType

  ///Positive uses compare, and Negative uses (compare * -1)
  type ComparisonSign = Positive | Negative

  ///An Incrementor describes the set of valid elements on the provided type 'a, as well as how to iterate between these elements. 
  type Incrementor<'a> = 
    abstract member increment: ('a * int -> 'a)
    abstract member diff: 'a -> 'a -> int
    abstract member isValidElement: ('a -> bool)
    abstract member toString: 'a -> 'a -> string

  type PivotPartition =
  | Exclusive
  | InclusiveLower
  | InclusiveUpper
  | InclusiveBoth

  ///Operates on the assumption that the type 'b will only have one valid instance, with exactly one kind of incrementor.
  type PositiveInterval<'a, 'b when 'a : equality and 'a : comparison and 'b :> Incrementor<'a>> private (lowerBound : 'a, upperBound : 'a, incrementor : 'b, comparisonSign : ComparisonSign) =

    /// This comparison is an intentional shadow of the default F# comparison,
    /// allowing us to invert the sign of compare based on the direction of the Incrementor.Increment
    let compare a b = 
      match comparisonSign with
      | Positive -> compare a b
      | Negative -> compare a b |> ((*) -1)

    let (=%) a b = compare a b = 0
    let (<%) a b = compare a b < 0
    let (>%) a b = compare a b > 0
    let (<=%) a b = compare a b <= 0
    let (>=%) a b = compare a b >= 0
    let ``max%`` a b = if a >=% b then a else b
    let ``min%`` a b = if a <% b then a else b

    let memoizedIncrement = 
      let appliedIncrement = (fun a i -> incrementor.increment(a, i))
      Cache.memoize2 appliedIncrement
    
    let memoizedElements = 
      seq { 
        for i in 0 .. incrementor.diff upperBound lowerBound ->
          incrementor.increment(lowerBound, i)
      }
      |> Seq.cache


    let length = lazy (memoizedElements |> Seq.length)
    let (|+|) element i = memoizedIncrement element i

    member this.LowerBound = lowerBound
    member this.UpperBound = upperBound
    member private this.Incrementor = incrementor
    member private this.ComparisonSign = comparisonSign


    member this.Enum = 
      memoizedElements.GetEnumerator()
    
    interface System.Collections.Generic.IEnumerable<'a> with
      member this.GetEnumerator(): System.Collections.Generic.IEnumerator<'a> = this.Enum
      [<ExcludeFromCodeCoverage>]
      member this.GetEnumerator(): System.Collections.IEnumerator = this.Enum :> System.Collections.IEnumerator
  
    [<ExcludeFromCodeCoverage>]
    member this.NonEmpty = memoizedElements |> NonEmpty.verify |> Option.unless "Attempting to evaluate a PostiveInterval as a NonEmpty seq of values failed to produce a NonEmpty seq of values - PositiveInterval's implementation has failed a guarantee it is supposed to provide."
    
    ///Operates on the assumption that the type 'b will only have one valid instance, with exactly one kind of incrementor.
    override this.Equals(that) =
      match that with
      | :? PositiveInterval<'a, 'b> as posInterval -> 
        posInterval.LowerBound = this.LowerBound
        && 
        posInterval.UpperBound = this.UpperBound
      | _ -> false
    

    override this.GetHashCode() = (this.LowerBound, this.UpperBound, incrementor.GetType()).GetHashCode()

    interface System.IComparable with
      member this.CompareTo(obj) =
        match obj with
        | :? PositiveInterval<'a, 'b> as that -> 
          match (this.LowerBound = that.LowerBound, this.UpperBound = that.UpperBound) with
          | (true, true) -> 0
          | (false, _) -> (compare this.LowerBound that.LowerBound)
          | (_, false) -> (compare this.UpperBound that.UpperBound)
        | _ -> raise (ArgumentException("Object is not a PositiveInterval"))
    
    [<ExcludeFromCodeCoverage>]
    override this.ToString() = incrementor.toString this.LowerBound this.UpperBound

    /// <summary>
    /// Categorizes the relationship of this Interval to the provided Interval in terms of a IntervalRelationship.
    /// </summary>
    /// <remarks>
    /// This is a directional relationship - for example, if this is a SuperInterval of that, then that is necessarily a SubInterval of this.
    /// Take care to use the correct direction.
    /// </remarks>
    member this.RelationshipTo (that:PositiveInterval<'a, 'b>) : IntervalRelationship =
      if (this.LowerBound <% that.LowerBound && this.UpperBound <% that.LowerBound) || (this.LowerBound >% that.UpperBound && this.UpperBound >% that.UpperBound) then Disjoint
      else 
        let intersectionType = 
          match (compare this.LowerBound that.LowerBound, compare this.UpperBound that.UpperBound) with
          | (lowerBounds, upperBounds) when lowerBounds = 0 && upperBounds = 0 -> Identical
          | (lowerBounds, upperBounds) when (lowerBounds <= 0 && upperBounds > 0) || (lowerBounds < 0 && upperBounds >= 0) -> SuperInterval
          | (lowerBounds, upperBounds) when (lowerBounds >= 0 && upperBounds < 0) || (lowerBounds > 0 && upperBounds <= 0) -> SubInterval
          | _ -> Overlapping
        Intersecting intersectionType
        
    /// <summary>
    /// Compares the calling interval to the provided interval, returning a list of exactly 0 to 2 intervals, 
    /// representing the values that exist in the provided interval that do not exist in the calling interval.
    /// </summary>
    member this.RelativeComplementIn (that:PositiveInterval<'a, 'b>) =
      [
        if that.LowerBound < this.LowerBound then 
          yield PositiveInterval.Create(that.LowerBound, this.LowerBound |+| -1, this.Incrementor)
        if that.UpperBound > this.UpperBound then
          yield PositiveInterval.Create(this.UpperBound |+| 1, that.UpperBound, this.Incrementor)
      ] |> List.choose id

    /// <summary>
    /// Splits this interval into a pair of Intervals around a given element.
    /// If the pivot element is in the set of valid elements for this interval,
    /// a PivotPartition may optionally be provided to specify which of the lower
    /// and upper intervals this element is included in.
    /// </summary>
    member this.Partition (element:'a, ?pivotPartition:PivotPartition) : (Interval<'a, 'b> * Interval<'a, 'b>)=
      let pivotPartition = defaultArg pivotPartition PivotPartition.InclusiveBoth
      if this.Contains(element) then
        let pivotLower, pivotUpper = 
          match pivotPartition with
          | Exclusive       -> (element |+| -1, element |+| 1)
          | InclusiveLower  -> (element       , element |+| 1)
          | InclusiveUpper  -> (element |+| -1, element      )
          | InclusiveBoth   -> (element       , element      )
        (
          Interval<'a,'b>.Create <||| (this.LowerBound, pivotLower, this.Incrementor),
          Interval<'a,'b>.Create <||| (pivotUpper, this.UpperBound, this.Incrementor)
        )
      else 
        let (lessThanPivot, greaterThanPivot) = this |> Array.ofSeq |> Array.partition(fun i -> i < element)
        match (lessThanPivot, greaterThanPivot) with
        //Note that the Empty, Empty case should never be hit by the constraints placed on it in the above code.
        | (Empty, _)           -> (EmptyInterval, Interval.PositiveInterval this)
        | (_, Empty)            -> (Interval.PositiveInterval this, EmptyInterval)
        | (NotEmpty lesser, NotEmpty greater) -> 
          (
          Interval<'a,'b>.Create <||| (this.LowerBound, lesser |> Array.NonEmpty.max, this.Incrementor),
          Interval<'a,'b>.Create <||| (greater |> Array.NonEmpty.min, this.UpperBound, this.Incrementor)
          )


    /// <summary>
    /// Returns true if the specified Interval is strictly adjacent to this Interval.
    /// Two Intervals are adjacent if they are disjoint, and the Union and Convex Hull of the Sets of both intervals are the same. 
    /// i.e. Two Intervals are adjacent if they are disjoint, but there exist no elements between them.
    /// </summary>
    member this.IsAdjacentTo (that:PositiveInterval<'a, 'b>) =
      this.RelationshipTo that = Disjoint 
      &&
      (
        (memoizedIncrement this.UpperBound 1) =% that.LowerBound 
        || 
        (memoizedIncrement that.UpperBound 1) =% this.LowerBound
      )

    /// <summary>
    /// Returns true if the specified Interval intersects this interval or if it is adjacent to 
    /// this Interval (separated only by a single increment).
    /// <c>UnionWith</c> will return a SeparateIntervals Error if and only if this returns false.
    /// </summary>
    member this.ConnectedTo (that:PositiveInterval<'a, 'b>) =
      this.Intersects that || this.IsAdjacentTo that
    
    /// <summary>
    /// See <c>ConnectedTo</c>.
    /// Returns true if the specified Interval intersects this interval or if it is adjacent to 
    /// this Interval (separated only by a single increment).
    /// <c>UnionWith</c> will return a SeparateIntervals Error if and only if this returns false.
    /// </summary>
    [<ExcludeFromCodeCoverage>]
    member this.CanUnionWith that = this.ConnectedTo that

    /// <summary>
    /// Returns the smallest Interval that contains all the elements of both this Interval and the specified Interval, and any elements in between.
    /// Similar to <c>UnionWith'</c>, but includes elements in between the two intervals, if the Intervals are separated.
    /// </summary>
    member this.ConvexHull (that:PositiveInterval<'a, 'b>) =
      let unionLowerBound = ``min%`` this.LowerBound that.LowerBound
      let unionUpperBound = ``max%`` this.UpperBound that.UpperBound
      new PositiveInterval<'a, 'b>(unionLowerBound, unionUpperBound, this.Incrementor, this.ComparisonSign)

    /// <summary>
    /// Returns true if this Interval contains any elements contained in the specified Interval.
    /// Returns false, otherwise.
    /// </summary>
    member this.Intersects (that:PositiveInterval<'a, 'b>) = 
      match this.RelationshipTo that with
      | Intersecting _ -> true
      | _ -> false
        
    /// <summary>
    /// Returns the subset Interval of elements that are contained in both this Interval and the specified Interval.
    /// Returns an empty Interval if the Intervals do not share any elements.
    /// </summary>
    member this.IntersectionWith (that:PositiveInterval<'a, 'b>) =
      // Theoretical basis: https://scicomp.stackexchange.com/a/26260/32219
      match this.RelationshipTo that with
      | Disjoint -> Interval.EmptyInterval
      | Intersecting _ -> 
        let intersectionLowerBound = ``max%`` this.LowerBound that.LowerBound
        let intersectionUpperBound = ``min%`` this.UpperBound that.UpperBound
        Interval.PositiveInterval (new PositiveInterval<'a, 'b>(intersectionLowerBound, intersectionUpperBound, this.Incrementor, this.ComparisonSign))

    /// <summary>
    /// Returns the smallest Interval that contains all the elements of both this Interval and the specified Interval, and no other elements.
    /// Returns a SeparateIntervals Error if the Intervals are separated (see <c>ConnectedTo</c>).
    /// Also see <c>ConnectTo</c>, which forms an Interval that contains and connects two separated Intervals.
    /// </summary>
    member this.UnionWithSafe (that:PositiveInterval<'a, 'b>) =
      if not (this.ConnectedTo that) then Error unionSeparatedErr
      else 
        let unionLowerBound = ``min%`` this.LowerBound that.LowerBound
        let unionUpperBound = ``max%`` this.UpperBound that.UpperBound
        Ok <| new PositiveInterval<'a, 'b>(unionLowerBound, unionUpperBound, this.Incrementor, this.ComparisonSign)

    /// <summary>
    /// See <c>UnionWithSafe</c>.
    /// </summary>
    [<ExcludeFromCodeCoverage>]
    member this.UnionWith' that = this.UnionWithSafe that

    /// <summary>
    /// Returns true if the specified element is in the set of values allowed by the incrementor, AND
    /// is between or equal to the lower and upper bound. 
    /// </summary>
    member this.Contains element =
      this.Incrementor.isValidElement element
      && element <=% this.UpperBound
      && element >=% this.LowerBound
    
    /// <summary>
    /// Adjusts the bounds of this PositiveInterval by N units to each corresponding bound.
    /// A "unit" is defined by the Increment function, where Incrememt this N would produce a shift of N units.
    /// </summary>
    /// <param name="lowerBoundAdjustment">The number of discrete units by which the LowerBound will be adjusted. </param>
    /// <param name="upperBoundAdjustment">The number of discrete units by which the UpperBound will be adjusted. </param>
    member this.Resize (lowerBoundAdjustment, upperBoundAdjustment) =
      let (newLowerBound , newUpperBound) = (memoizedIncrement this.LowerBound lowerBoundAdjustment, memoizedIncrement this.UpperBound upperBoundAdjustment)
      if newLowerBound >% newUpperBound then Interval.EmptyInterval
      else
        Interval.PositiveInterval (new PositiveInterval<'a, 'b>(newLowerBound, newUpperBound, this.Incrementor, this.ComparisonSign))
    
    /// <summary>
    /// Adjusts the bounds of this PositiveInterval by N units to each corresponding bound.
    /// A "unit" is defined by the Increment function, where Incrememt this N would produce a shift of N units.
    /// This adjustment is guaranteed to produce a PositiveInterval, as noted by the input types.
    /// </summary>
    /// <param name="lowerBoundAdjustment">The number of discrete units by which the LowerBound will be adjusted. </param>
    /// <param name="upperBoundAdjustment">The number of discrete units by which the UpperBound will be adjusted. </param>
    member this.ResizePositive ((NegativeInt lowerBoundAdjustment), (PositiveInt upperBoundAdjustment)) =
      let (newLowerBound , newUpperBound) = (memoizedIncrement this.LowerBound lowerBoundAdjustment, memoizedIncrement this.UpperBound upperBoundAdjustment)
      PositiveInterval.Create(newLowerBound, newUpperBound, this.Incrementor)
      |> Option.unless 
        ("We couldn't resize a positiveInterval despite constraints which should guarantee success. \n" 
        + "Consider checking NegativeInt, PositiveInt, and PositiveInterval's definitions to verify that these assumptions hold")

    /// Returns the length of the PositiveInterval, which may not always be equal to UpperBound - LowerBound, depending on how the Increment is defined.
    member this.Length = length |> Lazy.force 

    /// <summary>
    /// Divides a PositiveInterval into a series of smaller intervals, each containing a number of elements equal to chunkSize. 
    /// If the provided Interval does not evenly divide by the chunkSize, the last Interval produced will contain fewer than chunkSize elements.
    /// For example, an Interval <1 to 10> SplitBySize 3 produces Intervals <1 to 3>, <4 to 6>, <7 to 9>, and <10 to 10>
    /// </summary>
    member this.SplitBySize (PositiveInt chunkSize) = 

      let completeInterval (lowerBound, upperBound) = new PositiveInterval<'a, 'b>(lowerBound, upperBound, this.Incrementor, this.ComparisonSign)
      let truncatedInterval (lowerBound, _upperBound) = new PositiveInterval<'a, 'b>(lowerBound, this.UpperBound, this.Incrementor, this.ComparisonSign)
      
      //Sequences are used here to preserve lazy evaluation.
      InfiniteSeq.init (MaxElements 1_000_000) (fun idx ->
        let lowerBound = memoizedIncrement this.LowerBound (idx * chunkSize)
        let upperBound = memoizedIncrement this.LowerBound (((idx + 1) * chunkSize) - 1)
        let bounds = (lowerBound, upperBound)


        if upperBound <=% this.UpperBound 
          then Some (completeInterval bounds)
        elif lowerBound <=% this.UpperBound
          then Some (truncatedInterval  bounds)
        else None
      )
      |> InfiniteSeq.takeWhileLazy (Option.isSome) 
      |> Seq.map (Result.unless "Application hung while processing an Interval.  Already processed 1,000,000 elements.")
      |> Seq.choose id

    /// <summary>
    ///   <para> 
    ///     An Interval describes a complete, closed, inclusive series of discrete elements between two points. 
    ///   </para> <para>
    ///     An Interval is defined by its lower and upper bounds, as well as an Incrementor containing two functions:
    ///     an "increment" which describes how to iterate through a set of values, and an "isValid" function to determine valid elements.
    ///     If the bounds of an Interval are the same, the Interval will contain one value, which is equal to that bound.
    ///     Intervals are generally PositiveIntervals, but may be EmptyIntervals, if their bounds shrink such that the LowerBound > the UpperBound.
    ///     Some members may only be run on PositiveIntervals, requiring pattern matching on an Interval to run.
    ///
    ///     For example, an Interval describing all integers would have an increment of element +1 and an isValid which always returns true, since this Interval can accept any integers.
    ///
    ///     For example, an Interval describing all evens would have an increment of element +2 and an isValid of element%2 = 0, rejecting other integers.
    ///
    ///     For example, an Interval describing all Saturdays would have an increment of DateTime.AddDays(7) and an isValid which compares the day of week of a given DateTime to a known dayOfWeek.
    /// </para>
    /// </summary>
    /// <remarks> 
    ///   If describing a custom increment function, bear in mind that the increment must be able to increment any value of 'a appropriately.
    ///   Operates on the assumption that the type 'b will only have one valid instance, with exactly one kind of incrementor.
    /// </remarks>
    static member Create (lowerBound : 'a, upperBound : 'a, incrementor : 'b) : PositiveInterval<'a, 'b> option= 
      /// We need to determine how the increment function wants to handle comparison 
      /// so we can identify when calling increment creates an Interval that is invalid or empty.
      /// For example, if I want to create an interval of descending integers from 4 to 1,
      /// I would expect that a valid instance of this interval should have a lowerbound that is > the upperbound. 
      /// i.e. a valid descending interval would be 4 .. 1, while 1 .. 4 should be invalid. 
      /// We determine the direction the interval increments on construction based on the following assumption:
      ///
      /// Given an element N of type 'a, and an element M produced by incrementing N 1 discrete unit,
      /// An interval with N as the lowerbound and M as the upperbound is considered valid.
      ///
      /// From this valid interval, we can determine if comparing bounds with the default compare method
      /// describes valid intervals or invalid(Empty) intervals. If it does not, we invert the default comparison.
      let (validLowerBound, validUpperBound) = (lowerBound, incrementor.increment (lowerBound, 1))
      let validComparisonResult = Math.Sign (compare validUpperBound validLowerBound)
      let inputComparisonResult = Math.Sign (compare upperBound lowerBound)
      let comparisonSign = if validComparisonResult > 0 then Positive else Negative
      match (validComparisonResult = inputComparisonResult || inputComparisonResult = 0, incrementor.isValidElement lowerBound, incrementor.isValidElement upperBound) with
      | (true, true, true) -> Some (new PositiveInterval<'a,'b>(lowerBound, upperBound, incrementor, comparisonSign))
      | _ -> None

  /// <summary>
  ///   <para> 
  ///     An Interval describes a complete, closed, inclusive series of discrete elements between two points. 
  ///   </para> <para>
  ///     An Interval is defined by its lower and upper bounds, as well as an Incrementor containing two functions:
  ///     an "increment" which describes how to iterate through a set of values, and an "isValid" function to determine valid elements.
  ///     If the bounds of an Interval are the same, the Interval will contain one value, which is equal to that bound.
  ///     Intervals are generally PositiveIntervals, but may be EmptyIntervals, if their bounds shrink such that the LowerBound > the UpperBound.
  ///     Some members may only be run on PositiveIntervals, requiring pattern matching on an Interval to run.
  ///
  ///     For example, an Interval describing all integers would have an increment of element +1 and an isValid which always returns true, since this Interval can accept any integers.
  ///
  ///     For example, an Interval describing all evens would have an increment of element +2 and an isValid of element%2 = 0, rejecting other integers.
  ///
  ///     For example, an Interval describing all Saturdays would have an increment of DateTime.AddDays(7) and an isValid which compares the day of week of a given DateTime to a known dayOfWeek.
  /// </para>
  /// </summary>
  /// <remarks> 
  ///   If describing a custom increment function, bear in mind that the increment must be able to increment any value of 'a appropriately.
  ///   Operates on the assumption that the type 'b will only have one valid instance, with exactly one kind of incrementor.
  /// </remarks>
  and Interval<'a, 'b when 'a : equality and 'a : comparison and 'b :> Incrementor<'a>> =
    | EmptyInterval 
    | PositiveInterval of PositiveInterval<'a, 'b>

    ///   <summary>
    ///     <para> 
    ///       An Interval describes a complete, closed, inclusive series of discrete elements between two points. 
    ///     </para> <para>
    ///       An Interval is defined by its lower and upper bounds, as well as an Incrementor containing two functions:
    ///       an "increment" which describes how to iterate through a set of values, and an "isValid" function to determine valid elements.
    ///       If the bounds of an Interval are the same, the Interval will contain one value, which is equal to that bound.
    ///       Intervals are generally PositiveIntervals, but may be EmptyIntervals, if their bounds shrink such that the LowerBound > the UpperBound.
    ///       Some members may only be run on PositiveIntervals, requiring pattern matching on an Interval to run.
    ///
    ///       For example, an Interval describing all integers would have an increment of element +1 and an isValid which always returns true, since this Interval can accept any integers.
    ///
    ///       For example, an Interval describing all evens would have an increment of element +2 and an isValid of element%2 = 0, rejecting other integers.
    ///
    ///       For example, an Interval describing all Saturdays would have an increment of DateTime.AddDays(7) and an isValid which compares the day of week of a given DateTime to a known dayOfWeek.
    ///   </para>
    ///   </summary>
    ///   <remarks> 
    ///     If describing a custom increment function, bear in mind that the increment must be able to increment any value of 'a appropriately.
    ///     Operates on the assumption that the type 'b will only have one valid instance, with exactly one kind of incrementor.
    ///   </remarks>
    static member Create lowerBound upperBound incrementor =
      match PositiveInterval.Create (lowerBound, upperBound, incrementor) with
      | Some p -> PositiveInterval p
      | None -> EmptyInterval


    /// <summary>
    /// Returns true if the specified element is in the set of values allowed by the incrementor, AND
    /// is between or equal to the lower and upper bound. 
    /// </summary>
    member this.Contains element =
      match this with
      | EmptyInterval -> false
      | PositiveInterval p -> p.Contains element

    /// <summary>
    /// Divides an Interval into a series of smaller Intervals, each containing a number of elements equal to chunkSize. 
    /// If the provided Interval does not evenly divide by the chunkSize, the last Interval produced will contain fewer than chunkSize elements.
    /// For example, an Interval <1 to 10> SplitBySize 3 produces Intervals <1 to 3>, <4 to 6>, <7 to 9>, and <10 to 10>
    /// If called on an empty Interval, produces an empty Seq.
    /// </summary>
    member this.SplitBySize chunkSize : Interval<'a, 'b> seq =
      match this with
      | EmptyInterval -> Seq.empty
      | PositiveInterval p -> p.SplitBySize chunkSize |> Seq.map PositiveInterval
  
    /// <summary>
    /// Returns the length of this Interval. Depending on the definition of Increment, this may or may not correspond to UpperBound - LowerBound
    /// </summary>
    [<ExcludeFromCodeCoverage>]
    member this.Length = 
      match this with
      | EmptyInterval -> 0
      | PositiveInterval p -> p.Length

    /// <summary>
    /// Returns true if this Interval is a PositiveInterval
    /// </summary>
    [<ExcludeFromCodeCoverage>]
    member this.IsPositive = 
      match this with
        | PositiveInterval p -> true
        | EmptyInterval -> false
    
    /// <summary>
    /// Returns true if this Interval is an EmptyInterval
    /// </summary>
    [<ExcludeFromCodeCoverage>]
    member this.IsEmpty = 
      match this with
        | PositiveInterval p -> false
        | EmptyInterval -> true

    member this.Enum = 
        match this with
        | EmptyInterval -> Seq.empty.GetEnumerator()
        | PositiveInterval p -> p.Enum

    interface System.Collections.Generic.IEnumerable<'a> with
      member this.GetEnumerator(): System.Collections.Generic.IEnumerator<'a> = this.Enum
      [<ExcludeFromCodeCoverage>]
      member this.GetEnumerator(): System.Collections.IEnumerator = this.Enum :> System.Collections.IEnumerator

    [<ExcludeFromCodeCoverage>]
    override this.ToString() = 
      match this with 
      | EmptyInterval -> "EmptyInterval"
      | PositiveInterval p -> p.ToString()
      
    /// <summary>
    /// Returns true if the specified Interval intersects this interval or if it is adjacent to 
    /// this Interval (separated only by a single increment).
    /// <c>UnionWith</c> will return a SeparateIntervals Error if and only if this returns false.
    /// </summary>
    member this.CanUnionWith that =
      match (this, that) with
      | (EmptyInterval, _) | (_, EmptyInterval) -> true
      | (PositiveInterval p1, PositiveInterval p2) -> p1.CanUnionWith p2

    /// <summary>
    /// Returns true if the specified Interval intersects this interval or if it is adjacent to 
    /// this Interval (separated only by a single increment).
    /// <c>UnionWith</c> will return a SeparateIntervals Error if and only if this returns false.
    /// </summary>
    member this.ConnectedTo that = 
      match (this, that) with
      | (EmptyInterval, _) | (_, EmptyInterval) -> true
      | (PositiveInterval this, PositiveInterval that) -> this.ConnectedTo that

    /// <summary>
    /// Returns the smallest Interval that contains all the elements of both this Interval and the specified Interval, and any elements in between.
    /// Similar to <c>UnionWith'</c>, but includes elements in between the two Intervals, if the Intervals are separated <c>PositiveIntervals</c>.
    /// </summary>
    member this.ConvexHull that =
      match (this, that) with
      | (EmptyInterval, interval) | (interval, EmptyInterval) -> interval
      | (PositiveInterval p1, PositiveInterval p2) -> PositiveInterval (p1.ConvexHull p2)

    /// <summary>
    /// Returns the smallest Interval that contains all the elements of both this Interval and the specified Interval, and any elements in between.
    /// Alias of ConvexHull.
    /// </summary>
    member this.ConnectTo that = this.ConvexHull that

    /// <summary>
    /// Returns true if this Interval has the same incrementor as the provided Interval, and if both Intervals share at least one element.
    /// </summary>
    member this.Intersects that =
      match (this, that) with
      | (EmptyInterval, _) | (_, EmptyInterval) -> false
      | (PositiveInterval p1, PositiveInterval p2) -> p1.Intersects p2
         
    /// <summary>
    /// Returns the subset Interval of elements that are contained in both this Interval and the specified Interval.
    /// Returns an empty Interval if the Intervals do not share any elements.
    /// </summary>
    member this.IntersectionWith that =
      match (this, that) with
      | (EmptyInterval, _) | (_, EmptyInterval) -> EmptyInterval
      | (PositiveInterval p1, PositiveInterval p2) -> p1.IntersectionWith p2

    /// <summary>
    /// Returns true if the specified Interval is strictly adjacent to this Interval.
    /// Two Intervals are adjacent if they do not overlap, and no valid elements exist between them.
    /// Returns false if the Intervals are separate from each other.
    /// </summary>
    member this.IsAdjacentTo that =
      match (this, that) with
      | (EmptyInterval, _) | (_, EmptyInterval) -> false
      | (PositiveInterval this, PositiveInterval that) -> this.IsAdjacentTo that

    /// <summary>
    /// Categorizes the relationship of this Interval to the provided Interval in terms of a IntervalRelationship.
    /// </summary>
    /// <remarks>
    /// This is a directional relationship - for example, if this is a SuperInterval of that, then that is necessarily a SubInterval of this.
    /// Take care to use the correct direction.
    /// </remarks>
    member this.RelationshipTo that = 
      match (this, that) with
      | (EmptyInterval, EmptyInterval) -> Intersecting Identical
      | (EmptyInterval, PositiveInterval that) -> Intersecting SubInterval
      | (PositiveInterval this, EmptyInterval) -> Intersecting SuperInterval
      | PositiveInterval this, PositiveInterval that -> this.RelationshipTo(that)

    /// <summary>
    /// Compares the calling interval to the provided interval, returning a list of exactly 0 to 2 intervals, 
    /// representing the values that exist in the provided interval that do not exist in the calling interval.
    /// </summary>
    member this.RelativeComplementIn (that:Interval<'a, 'b>) : PositiveInterval<'a, 'b> list=
      match (this, that) with
      | PositiveInterval thisP, PositiveInterval thatP -> thisP.RelativeComplementIn(thatP)
      | EmptyInterval, PositiveInterval thatP -> [thatP]
      | PositiveInterval thisP, EmptyInterval -> []
      | EmptyInterval, EmptyInterval -> []

    /// <summary>
    /// Splits this interval into a pair of Intervals around a given element.
    /// PivotPartition determines which of the resulting intervals to include the pivot element in.
    /// PivotPartition defaults to include the given element in both output intervals.
    /// </summary>
    member this.Partition (element:'a, ?pivotPartition:PivotPartition) = 
      let pivotPartition = defaultArg pivotPartition PivotPartition.InclusiveBoth
      match this with 
      | EmptyInterval -> (EmptyInterval, EmptyInterval)
      | PositiveInterval thisP -> thisP.Partition(element, pivotPartition)


    /// <summary>
    /// Adjusts the bounds of this PositiveInterval by N units to each corresponding bound.
    /// A "unit" is defined by the Increment function, where Incrememt this N would produce a shift of N units.
    /// </summary>
    /// <param name="lowerBoundAdjustment">The number of discrete units by which the LowerBound will be adjusted. </param>
    /// <param name="upperBoundAdjustment">The number of discrete units by which the UpperBound will be adjusted. </param>
    member this.Resize ((lowerBoundAdjustment, upperBoundAdjustment) as adjustments) = 
      match this with 
      | EmptyInterval -> EmptyInterval
      | PositiveInterval this -> this.Resize adjustments 

    /// <summary>
    /// Returns the smallest Interval that contains all the elements of both this Interval and the specified Interval, and no other elements.
    /// Returns a SeparateIntervals Error if the Intervals are separated (see <c>ConnectedTo</c>).
    /// Also see <c>ConnectTo</c>, which forms an Interval that contains and connects two separated Intervals.
    /// </summary>
    member this.UnionWithSafe that =
      match (this, that) with
      | (EmptyInterval, interval) | (interval, EmptyInterval) -> Ok <| interval
      | (PositiveInterval p1, PositiveInterval p2) -> p1.UnionWithSafe p2 |> Result.map PositiveInterval

    /// <summary>
    /// See <c>UnionWithSafe</c>.
    /// </summary>
    [<ExcludeFromCodeCoverage>]
    member this.UnionWith' that = this.UnionWithSafe that

  let (|EmptyInterval|PositiveInterval|) interval = 
    match interval with
    | EmptyInterval -> EmptyInterval
    | PositiveInterval p -> PositiveInterval p

  let EmptyInterval = EmptyInterval

  let IntervalFromPositive interval = PositiveInterval interval

  [<ExcludeFromCodeCoverage>]
  module Interval =

    /// <summary>
    /// Attempts to unwrap an Interval into a PositiveInterval, returning None if the provided Interval is Empty.
    /// </summary>
    let asPositive interval = 
      match interval with
      | EmptyInterval -> None
      | PositiveInterval p -> Some p
    
    /// <summary>
    /// Returns true if the specified element falls within the bounds of the Interval. 
    /// Returns false if called on an EmptyInterval
    /// </summary>
    let inline contains element (interval : Interval<'a, 'b>) = interval.Contains element 

    /// <summary>
    /// Adjusts the bounds of this PositiveInterval by N units to each corresponding bound.
    /// A "unit" is defined by the Increment function, where Increment this N would produce a shift of N units.
    /// </summary>
    /// <param name="lowerBoundAdjustment">The number of units by which the LowerBound will be adjusted. </param>
    /// <param name="upperBoundAdjustment">The number of units by which the UpperBound will be adjusted. </param>
    let inline resize (left, right) (interval : Interval<'a, 'b>) = interval.Resize (left, right)

    /// <summary>
    /// Divides an Interval into a series of smaller Intervals, each containing a number of elements equal to chunkSize. 
    /// If the provided Interval does not evenly divide by the chunkSize, the last Interval produced will contain fewer than chunkSize elements.
    /// For example, an Interval <1 to 10> SplitBySize 3 produces Intervals <1 to 3>, <4 to 6>, <7 to 9>, and <10 to 10>
    /// If called on an empty Interval, will produce an empty Seq.
    /// </summary>
    let inline splitBySize chunkSize (interval : Interval<'a, 'b>) = interval.SplitBySize chunkSize

    /// <summary>
    /// Returns the length of this Interval. Depending on the definition of Increment, this may or may not correspond to UpperBound - LowerBound
    /// </summary>
    let inline length (interval : Interval<'a, 'b>) = interval.Length

    /// <summary>
    /// Categorizes the relationship of this Interval to the provided Interval in terms of a IntervalRelationship.
    /// </summary>
    /// <remarks>
    /// This is a directional relationship - for example, if this is a SuperInterval of that, then that is necessarily a SubInterval of this.
    /// Take care to use the correct direction.
    /// </remarks>
    let inline relationshipTo   (leftInterval : Interval<'a, 'b>) (rightInterval : Interval<'a, 'b>) = leftInterval.RelationshipTo(rightInterval)

    /// <summary>
    /// Compares the parameter "callingInterval to the parameter "comparedInterval", returning a list of exactly 0 to 2 intervals, 
    /// representing the values that exist in "comparedInterval" that do not exist in "callingInterval".
    /// </summary>
    let inline relativeComplementIn (comparedInterval : Interval<'a, 'b>) (callingInterval : Interval<'a, 'b>) = callingInterval.RelativeComplementIn(comparedInterval)

    /// <summary>
    /// Splits this interval into a pair of Intervals around a given element.
    /// PivotPartition determines which of the resulting intervals to include the pivot element in.
    /// </summary>
    let inline partition element pivot (interval: Interval<'a,'b>) = interval.Partition(element, pivot)

    /// <summary>
    /// Splits this interval into a pair of Intervals around a given element.
    /// The pivoted element will be included in both output intervals.
    /// </summary>
    let inline partitionInclusive element (interval: Interval<'a,'b>) = interval.Partition(element)

    /// <summary>
    /// Returns true if the specified Interval is strictly adjacent to this Interval.
    /// Two Intervals are adjacent if they are disjoint, and the Union and Convex Hull of the Sets of both intervals are the same. 
    /// i.e. Two Intervals are adjacent if they are disjoint, but there exist no elements between them.
    /// </summary>
    let inline isAdjacentTo     (leftInterval : Interval<'a, 'b>) (rightInterval : Interval<'a, 'b>) = leftInterval.IsAdjacentTo(rightInterval)

    /// <summary>
    /// Returns true if the specified Interval intersects this interval or if it is adjacent to 
    /// this Interval (separated only by a single increment).
    /// <c>UnionWith</c> will return a SeparateIntervals Error if and only if this returns false.
    /// </summary>
    let inline connectedTo      (leftInterval : Interval<'a, 'b>) (rightInterval : Interval<'a, 'b>) = leftInterval.ConnectedTo(rightInterval)

    /// <summary>
    /// Returns true if there exists an Interval containing only the elements of this and the specified Interval.
    /// Returns false if no such Interval exists.
    /// <c>UnionWith</c> will return a SeparateIntervals Error if and only if this returns false.
    /// </summary>
    let inline canUnionWith     (leftInterval : Interval<'a, 'b>) (rightInterval : Interval<'a, 'b>) = leftInterval.CanUnionWith(rightInterval)

    /// <summary>
    /// Returns the smallest Interval that contains all the elements of both this Interval and the specified Interval, and any elements in between.
    /// Similar to <c>UnionWith'</c>, but includes elements in between the two Intervals, if the Intervals are separated <c>PositiveIntervals</c>.
    /// </summary>
    let inline convexHull       (leftInterval : Interval<'a, 'b>) (rightInterval : Interval<'a, 'b>) = leftInterval.ConvexHull(rightInterval)

    /// <summary>
    /// Returns true if this Interval has the same incrementor as the provided Interval, and if both Intervals share at least one element.
    /// </summary>
    let inline intersects       (leftInterval : Interval<'a, 'b>) (rightInterval : Interval<'a, 'b>) = leftInterval.Intersects(rightInterval)

    /// <summary>
    /// Returns the subset Interval of elements that are contained in both this Interval and the specified Interval.
    /// Returns an empty Interval if the Intervals do not share any elements.
    /// </summary>
    let inline intersectionWith (leftInterval : Interval<'a, 'b>) (rightInterval : Interval<'a, 'b>) = leftInterval.IntersectionWith(rightInterval)

    /// <summary>
    /// Returns the smallest Interval that contains all the elements of both this Interval and the specified Interval, and no other elements.
    /// Returns a SeparateIntervals Error if the Intervals are separated (see <c>ConnectedTo</c>).
    /// Also see <c>ConnectTo</c>, which forms an Interval that contains and connects two separated Intervals.
    /// </summary>
    let inline unionWithSafe    (leftInterval : Interval<'a, 'b>) (rightInterval : Interval<'a, 'b>) = leftInterval.UnionWithSafe(rightInterval)

    let upperBound = function
      | EmptyInterval -> None
      | PositiveInterval p -> Some p.UpperBound

    let lowerBound = function
      | EmptyInterval -> None
      | PositiveInterval p -> Some p.LowerBound

    let empty = EmptyInterval

  [<ExcludeFromCodeCoverage>] [<RequireQualifiedAccess>]
  module PositiveInterval = 
    /// <summary>
    /// Adjusts the bounds of this PositiveInterval by N units to each corresponding bound.
    /// A "unit" is defined by the Increment function, where Incrememt this N would produce a shift of N units.
    /// </summary>
    /// <param name="lowerBoundAdjustment">The number of units by which the LowerBound will be adjusted. </param>
    /// <param name="upperBoundAdjustment">The number of units by which the UpperBound will be adjusted. </param>
    let inline resize (left, right) (interval : PositiveInterval<'a, 'b>) = interval.Resize (left, right)

    /// <summary>
    /// Adjusts the bounds of this PositiveInterval by N units to each corresponding bound.
    /// A "unit" is defined by the Increment function, where Incrememt this N would produce a shift of N units.
    /// This adjustment is guaranteed to produce a PositiveInterval, as noted by the input types.
    /// </summary>
    /// <param name="lowerBoundAdjustment">The number of discrete units by which the LowerBound will be adjusted. </param>
    /// <param name="upperBoundAdjustment">The number of discrete units by which the UpperBound will be adjusted. </param>
    let inline resizePositive (left, right) (interval : PositiveInterval<'a, 'b>) = interval.ResizePositive (left, right)

    let lowerBound (interval : PositiveInterval<_,_>) = interval.LowerBound
    let upperBound (interval : PositiveInterval<_,_>) = interval.UpperBound
    let bounds interval = lowerBound interval, upperBound interval
