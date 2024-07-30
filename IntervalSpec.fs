namespace GasDay.Core.Specs

open System
open System.Diagnostics.CodeAnalysis
open NUnit.Framework
open Swensen.Unquote
open SafetyFirst
open SafetyFirst.Numbers
open NodaTime

open GasDay.Core
open GasDay.Core.Interval
open GasDay.Core.Interval.Interval
open GasDay.Core.CoreIntervals.Interval
open GasDay.Core.CoreIntervals

module IntervalSpec =

  [<ExcludeFromCodeCoverage>]
  let makePositiveInterval (lower, upper, increment) = 
    Interval.Create <||| (lower, upper, increment)
    |> Interval.asPositive
    |> Option.unless "Bad test setup.  This interval is not positive"
  
  let intLowerValue = 4
  let intUpperValue = 12

  let dateLowerValue = new LocalDate(2000, 1, 1)
  let dateUpperValue = new LocalDate(2000, 2, 1)

  let dateTimeLowerValue = new DateTime(2000, 1, 1)
  let dateTimeUpperValue = new DateTime(2000, 2, 1)

  let sampleIntInterval = Interval.Create <||| (intLowerValue, intUpperValue, Ints.Consecutive)
  let sampleLocalDateInterval = Interval.Create <||| (dateLowerValue, dateUpperValue, LocalDates.ByDay)

  let samplePositiveIntInterval = sampleIntInterval |> Interval.asPositive |> Option.unless "Bad Test Setup"
  let samplePositiveLocalDateInterval = sampleLocalDateInterval |> Interval.asPositive |> Option.unless "Bad Test Setup"

  let sampleEmptyIntInterval = Interval.Create <||| (intUpperValue, intLowerValue, Ints.Consecutive)
  let sampleEmptyLocalDateInterval = Interval.Create <||| (dateUpperValue, dateLowerValue, LocalDates.ByDay)

  [<ExcludeFromCodeCoverage>]
  type BackwardsInt = BackwardsInt with 
    interface Incrementor<int> with 
      member this.increment = fun (a, b) -> (a - b)
      member this.diff a b = b - a
      member this.isValidElement = fun _ -> true
      member this.toString lower upper = sprintf "BackwardsInt between %i and %i" lower upper

  [<Test>]
  let ``can construct a PositiveInterval from the constructor method`` () = 
    let testCreate = Interval.Create <||| (intLowerValue, intUpperValue, Consecutive)
    test <@ Seq.toList <| testCreate = [intLowerValue .. intUpperValue]@>

  [<Test>]
  let ``can construct an EmptyInterval from the constructor method`` () =
    let testCreate = Interval.Create <||| (intUpperValue, intLowerValue, Consecutive)
    test <@testCreate = Interval.EmptyInterval@>
  
  [<Test>]
  let ``catches invalid increment and bound pairs, and instead returns an emptyInterval`` () =
    
    let testCreate = Interval.Create <||| (intLowerValue, intUpperValue, BackwardsInt)
    let testCreate2 = Interval.Create <||| (intUpperValue, intLowerValue, BackwardsInt)
    let testCreate3 = Interval.Create <||| (intUpperValue, intLowerValue, Consecutive)
    test <@testCreate.IsEmpty@>
    test <@testCreate2.IsPositive@>
    test <@testCreate3.IsEmpty@>

  [<Test>]
  let ``can enumerate an Interval as a Set`` () = 
    let interval1 = Interval.LocalDates.Of (LocalDate (2000, 1, 15))
    let interval2 = Interval.LocalDates.Between (LocalDate (2000, 1, 15)) (LocalDate (2000, 1, 19))
    let interval3 = EmptyInterval
    let interval1Set = interval1 |> Set.ofSeq
    let interval2Set = interval2 |> Set.ofSeq
    let interval3Set = interval3 |> Set.ofSeq

    test <@ interval1Set = new Set<LocalDate> [|(LocalDate (2000, 1, 15))|]@>
    test <@ interval2Set = new Set<LocalDate> [|(LocalDate (2000, 1, 15)); (LocalDate (2000, 1, 16)); (LocalDate (2000, 1, 17)); (LocalDate (2000, 1, 18)); (LocalDate (2000, 1, 19))|]@>
    test <@ interval3Set = Set.empty@>



  [<Test>]
  let ``can detect if a value exists in an interval`` () = 
    let containedInt = 6                               
    let notContainedInt = 3
    let containedLocalDate = LocalDate(2000, 1, 15)    
    let notContainedLocalDate = LocalDate(2000, 2, 15)
    let containedDateTime = DateTime(2000, 1, 15) 
    let notContainedDateTime = DateTime(2000, 2, 15)

    test <@ sampleIntInterval.Contains (containedInt)@>
    test <@ samplePositiveIntInterval.Contains (containedInt) @> 
    test <@ not <| samplePositiveIntInterval.Contains(notContainedInt) @> 
    test <@ not <| sampleIntInterval.Contains(notContainedInt) @> 
    test <@ not <| (sampleEmptyIntInterval.Contains(notContainedInt) || sampleEmptyIntInterval.Contains(containedInt)) @>

    test 
      <@ 
        samplePositiveLocalDateInterval.Contains (containedLocalDate)
        &&
        (not <| samplePositiveLocalDateInterval.Contains(notContainedLocalDate))        
        &&
        (not <| (sampleEmptyLocalDateInterval.Contains(notContainedLocalDate) || sampleEmptyLocalDateInterval.Contains(containedLocalDate)))            
      @>

  [<Test>]
  let ``can find the intersection of two intervals`` () =
    let interval (x, y) = Ints.Consecutive.Between x y
    let interval1 = interval (0, 10)
    let interval2sAndTheirIntersectionsWith1 = [
      (interval (-4, 0),  interval (0, 0))
      (interval (-4, 5),  interval (0, 5))
      (interval (-4, 10), interval (0, 10))
      (interval (-4, 12), interval (0, 10))

      (interval (0, 0),   interval (0, 0))
      (interval (0, 5),   interval (0, 5))
      (interval (0, 10),  interval (0, 10))
      (interval (0, 14),  interval (0, 10))

      (interval (5, 5),   interval (5, 5))
      (interval (5, 7),   interval (5, 7))
      (interval (5, 10),  interval (5, 10))
      (interval (5, 14),  interval (5, 10))

      (interval (10, 10), interval (10, 10))
      (interval (10, 14), interval (10, 10))
    ]

    let interval2sNonIntersecting = [
      interval (-4, -2)
      interval (-2, -1)
      interval (-1, -1)
      interval (11, 11)
      interval (11, 12)
      interval (12, 14)
    ]

    for (interval2, intersection) in interval2sAndTheirIntersectionsWith1 do
      test <@ interval1.Intersects interval2 @> 
      test <@ interval1.IntersectionWith interval2 = intersection @>

    for interval2 in interval2sNonIntersecting do
      test <@ not (interval1.Intersects interval2) @>
      test <@ interval1.IntersectionWith interval2 = EmptyInterval @>

    test <@ not (interval1.Intersects EmptyInterval) && interval1.IntersectionWith EmptyInterval = EmptyInterval @>
    test <@ not (EmptyInterval.Intersects interval1) && EmptyInterval.IntersectionWith interval1 = EmptyInterval @>
    test <@ not (EmptyInterval.Intersects EmptyInterval) && EmptyInterval.IntersectionWith EmptyInterval = EmptyInterval @>

  [<Test>]
  let ``can resize an interval outwards`` () = 
    let expectedNewSampleIntInterval = Interval.Create <||| (intLowerValue - 1, intUpperValue + 1, Ints.Consecutive)
    let expectedNewSampleLocalDateInterval = Interval.Create <||| (dateLowerValue.PlusDays(-1), dateUpperValue.PlusDays(1), LocalDates.ByDay)  
    
    let expandedInt = sampleIntInterval |> Interval.resize (-1, 1)
    let expandedLocalDate = sampleLocalDateInterval |> Interval.resize (-1, 1)

    let expandedPosInt = samplePositiveIntInterval |> PositiveInterval.resize (-1, 1)
    let expandedPosLocalDate = samplePositiveLocalDateInterval |> PositiveInterval.resize (-1, 1)

    test <@expandedInt =expectedNewSampleIntInterval@>
    test <@expandedLocalDate = expectedNewSampleLocalDateInterval@>
    test <@(EmptyInterval |> Interval.resize (-1, 1) = EmptyInterval)@>

    test <@expandedPosInt = expectedNewSampleIntInterval@>
    test <@expandedPosLocalDate = expectedNewSampleLocalDateInterval@>

  [<Test>]
  let ``can resize an interval inwards`` () = 
    let expectedNewSampleIntInterval = Interval.Create <||| (intLowerValue + 1, intUpperValue - 1, Ints.Consecutive)
    let expectedNewSampleLocalDateInterval = Interval.Create <||| (dateLowerValue.PlusDays(1), dateUpperValue.PlusDays(-1), LocalDates.ByDay)
    
    let expandedInt = sampleIntInterval |> Interval.resize (1, -1)
    let expandedLocalDate = sampleLocalDateInterval |> Interval.resize (1, -1)

    let expandedPosInt = samplePositiveIntInterval |> PositiveInterval.resize (1, -1)
    let expandedPosLocalDate = samplePositiveLocalDateInterval |> PositiveInterval.resize (1, -1)

    test <@expandedInt = expectedNewSampleIntInterval@>
    test <@expandedLocalDate = expectedNewSampleLocalDateInterval@>
    test <@(EmptyInterval |> Interval.resize (1, -1) = EmptyInterval)@>

    test <@expandedPosInt = expectedNewSampleIntInterval@>
    test <@expandedPosLocalDate = expectedNewSampleLocalDateInterval@>

  [<Test>]
  let ``can resize an interval as PositiveInterval`` () =
    let expectedNewSampleIntInterval = Interval.Create <||| (intLowerValue - 1, intUpperValue + 1, Ints.Consecutive) |> Interval.asPositive |> Option.unless "Bad Test Setup"
    let expectedNewSampleLocalDateInterval = Interval.Create <||| (dateLowerValue.PlusDays(-1), dateUpperValue.PlusDays(1), LocalDates.ByDay) |> Interval.asPositive |> Option.unless "Bad TestSetup"


    test <@samplePositiveIntInterval |> PositiveInterval.resizePositive (-1 |> NegativeInt.assume, 1 |> PositiveInt.assume) = expectedNewSampleIntInterval@>
    test <@samplePositiveLocalDateInterval |> PositiveInterval.resizePositive (-1 |> NegativeInt.assume, 1 |> PositiveInt.assume) = expectedNewSampleLocalDateInterval@>


  [<Test>]
  let ``can resize an interval to an emptyInterval`` () = 
    
    let expandedInt = sampleIntInterval |> Interval.resize (30, -30)
    let expandedLocalDate = sampleLocalDateInterval |> Interval.resize (30, -30)

    let expandedPosInt = samplePositiveIntInterval |> PositiveInterval.resize (30, -30)
    let expandedPosLocalDate = samplePositiveLocalDateInterval |> PositiveInterval.resize (30, -30)

    test <@expandedInt = Interval.EmptyInterval@>
    test <@expandedLocalDate = Interval.EmptyInterval@>

    test <@expandedPosInt = Interval.EmptyInterval@>
    test <@expandedPosLocalDate = Interval.EmptyInterval@>
    
  [<Test>]
  let ``can find the union of two intervals, or connect them if they are separated`` () =
    let interval (x, y) = Ints.Consecutive.Between x y
    let interval1 = interval (0, 10)
    let interval2sConnectedAndTheirUnionsWith1 = [
      (interval (-4, -1), interval (-4, 10))
      (interval (-4, 0),  interval (-4, 10))
      (interval (-4, 5),  interval (-4, 10))
      (interval (-4, 10), interval (-4, 10))
      (interval (-4, 12), interval (-4, 12))

      (interval (-1, -1), interval (-1, 10))

      (interval (0, 0),   interval (0, 10))
      (interval (0, 5),   interval (0, 10))
      (interval (0, 10),  interval (0, 10))
      (interval (0, 14),  interval (0, 14))

      (interval (5, 5),   interval (0, 10))
      (interval (5, 7),   interval (0, 10))
      (interval (5, 10),  interval (0, 10))
      (interval (5, 14),  interval (0, 14))

      (interval (10, 10), interval (0, 10))
      (interval (10, 14), interval (0, 14))

      (interval (11, 11), interval (0, 11))
      (interval (11, 14), interval (0, 14))
    ]

    let interval2sNonConnectedAndTheirConnectionsWith1 = [
      (interval (-4, -2), interval (-4, 10))
      (interval (-3, -2), interval (-3, 10))
      (interval (-2, -2), interval (-2, 10))
      (interval (12, 12), interval (0, 12))
      (interval (12, 13), interval (0, 13))
      (interval (12, 14), interval (0, 14))
    ]

    for (interval2, union) in interval2sConnectedAndTheirUnionsWith1 do
      test <@ interval1.CanUnionWith interval2 @> 
      test <@ interval1.UnionWith' interval2 = Ok union @>

    for (interval2, connection) in interval2sNonConnectedAndTheirConnectionsWith1 do
      test <@ not (interval1.CanUnionWith interval2) @>
      test <@ interval1.UnionWith' interval2 = Error unionSeparatedErr @>
      test <@ interval1.ConnectTo interval2 = connection @>

    test 
      <@ 
        interval1.CanUnionWith EmptyInterval
        && interval1.UnionWith' EmptyInterval = Ok interval1
        && interval1.ConnectTo EmptyInterval = interval1
      @>
    test 
      <@ 
        EmptyInterval.CanUnionWith interval1
        && EmptyInterval.UnionWith' interval1 = Ok interval1
        && EmptyInterval.ConnectTo interval1 = interval1
      @>
    test 
      <@ 
        EmptyInterval.CanUnionWith EmptyInterval
        && EmptyInterval.UnionWith' EmptyInterval = Ok EmptyInterval
        && EmptyInterval.ConnectTo EmptyInterval = EmptyInterval
      @>

  [<Test>]
  let ``can chunk an interval into aligned pieces`` () = 
    let alignedIntInterval =    Interval.Create <||| (3, 11, Ints.Consecutive)
    let alignedIntChunk1 =      Interval.Create <||| (3, 5, Ints.Consecutive)
    let alignedIntChunk2 =      Interval.Create <||| (6, 8, Ints.Consecutive)
    let alignedIntChunk3 =      Interval.Create <||| (9, 11, Ints.Consecutive)
    let expectedIntOutput = [alignedIntChunk1; alignedIntChunk2; alignedIntChunk3]
    let testIntOutput = alignedIntInterval |> (Interval.splitBySize (PositiveInt.assume 3)) |> Seq.toList
    
    let alignedLocalDateInterval =  Interval.Create <||| (LocalDate(2000, 1, 3),  LocalDate(2000, 1, 11), LocalDates.ByDay)
    let alignedLocalDateChunk1 =    Interval.Create <||| (LocalDate(2000, 1, 3),  LocalDate(2000, 1, 5) , LocalDates.ByDay)
    let alignedLocalDateChunk2 =    Interval.Create <||| (LocalDate(2000, 1, 6),  LocalDate(2000, 1, 8) , LocalDates.ByDay)
    let alignedLocalDateChunk3 =    Interval.Create <||| (LocalDate(2000, 1, 9),  LocalDate(2000, 1, 11), LocalDates.ByDay)
    let expectedLocalDateOutput = [alignedLocalDateChunk1; alignedLocalDateChunk2; alignedLocalDateChunk3]
    let testLocalDateOutput = alignedLocalDateInterval |> (Interval.splitBySize (PositiveInt.assume 3))

    test <@expectedIntOutput = testIntOutput@>
    test <@expectedLocalDateOutput = (testLocalDateOutput |> Seq.toList)@>
  
  [<Test>]
  let ``can chunk an interval into misaligned pieces`` () = 
    let alignedIntInterval =  Interval.Create <||| (3, 12, Ints.Consecutive)
    let alignedIntChunk1 =    Interval.Create <||| (3, 5, Ints.Consecutive)
    let alignedIntChunk2 =    Interval.Create <||| (6, 8, Ints.Consecutive)
    let alignedIntChunk3 =    Interval.Create <||| (9, 11, Ints.Consecutive)
    let alignedIntChunk4 =    Interval.Create <||| (12, 12, Ints.Consecutive)
    let expectedIntOutput = [alignedIntChunk1; alignedIntChunk2; alignedIntChunk3; alignedIntChunk4]
    let testIntOutput = alignedIntInterval |> (Interval.splitBySize (PositiveInt.assume 3)) |> Seq.toList
    
    let alignedLocalDateInterval =  Interval.Create <||| (LocalDate(2000, 1, 3),   LocalDate(2000, 1, 12), LocalDates.ByDay)
    let alignedLocalDateChunk1 =    Interval.Create <||| (LocalDate(2000, 1, 3),   LocalDate(2000, 1, 5),  LocalDates.ByDay)
    let alignedLocalDateChunk2 =    Interval.Create <||| (LocalDate(2000, 1, 6),   LocalDate(2000, 1, 8),  LocalDates.ByDay)
    let alignedLocalDateChunk3 =    Interval.Create <||| (LocalDate(2000, 1, 9),   LocalDate(2000, 1, 11), LocalDates.ByDay)
    let alignedLocalDateChunk4 =    Interval.Create <||| (LocalDate(2000, 1, 12),  LocalDate(2000, 1, 12), LocalDates.ByDay)
    let expectedLocalDateOutput = [alignedLocalDateChunk1; alignedLocalDateChunk2; alignedLocalDateChunk3; alignedLocalDateChunk4]
    let testLocalDateOutput = alignedLocalDateInterval |> (Interval.splitBySize (PositiveInt.assume 3)) |> Seq.toList

    test <@expectedIntOutput = testIntOutput@>
    test <@expectedLocalDateOutput = testLocalDateOutput @>

  [<Test>]
  let ``chunking by size on an empty interval returns an empty seq`` =
    test <@sampleEmptyIntInterval.SplitBySize(PositiveInt.assume 3) = Seq.empty@>

  [<Test>]
  let ``calculates length with Intervals with Increments more than the base difference`` =
    test <@(Interval.Create <||| (2,6, Ints.Evens)).Length = 3 @>
    test <@(EmptyInterval).Length = 0 @>

  [<Test>]
  let ``can evaluate a range as an enum`` () =
    test <@samplePositiveIntInterval |> Seq.toList = [4;5;6;7;8;9;10;11;12] @>

  [<Test>]
  let ``intervals are comparable`` () =
    let interval1 =   samplePositiveIntInterval.Resize(0  , 0 )
    let interval2 =   samplePositiveIntInterval.Resize(-1 , 1 )
    let interval3 =   samplePositiveIntInterval.Resize(1  , -1)
    let interval35 =  samplePositiveIntInterval.Resize(0  , 1 ) 
    let interval4 =   Interval.EmptyInterval
    let interval1Clone = IntervalFromPositive samplePositiveIntInterval

    let completeIntervalList = [interval1; interval2; interval3; interval35; interval4]
    let intervalListWithDuplicates = [interval1; interval1; interval1; interval2; interval3; interval35; interval4; interval4;]
    let expectedIntervalSet = 
      Set.empty
      |> Set.add interval1
      |> Set.add interval2
      |> Set.add interval3
      |> Set.add interval35
      |> Set.add interval4

    test <@completeIntervalList |> Set.ofList = expectedIntervalSet@>
    test <@intervalListWithDuplicates |> Set.ofList = expectedIntervalSet@>
    test <@interval1 = interval1Clone@>
    raises <@(samplePositiveIntInterval :> IComparable).CompareTo("String")@>

  [<Test>]
  let ``intervals are equatable`` () =
    let interval = Interval.Create <||| (1, 4, Ints.Consecutive)
    let intervalClone = Interval.Create <||| (1, 4, Ints.Consecutive)
    let positiveInterval = Ints.Consecutive.Between 1 4 |> Interval.asPositive |> Option.unless "Bad Test Setup"
    let differentIncrementInterval = Interval.Create <||| (2, 6, Ints.Evens)
    let differentLBInterval = Interval.Create <||| (0, 4, Ints.Consecutive)
    let differentUBInterval = Interval.Create <||| (1, 5, Ints.Consecutive)
    let misalignedInterval = Interval.Create <||| (2, 5, Ints.Consecutive)
    let emptyInterval = EmptyInterval

    test <@ interval = interval @>
    test <@ interval = intervalClone @>
    test <@ positiveInterval = positiveInterval @>
    test <@ not (positiveInterval.Equals("string")) @>
    test <@ not (interval.Equals(differentIncrementInterval)) @>
    test <@ not (interval = differentLBInterval) @>
    test <@ not (interval = differentUBInterval) @>
    test <@ not (interval = misalignedInterval) @>
    test <@ not (interval = EmptyInterval) @>
    test <@ EmptyInterval = EmptyInterval @>
    test <@ not (interval.Equals("String")) @>

    test <@ interval.GetHashCode() = interval.GetHashCode()@>
    test <@ interval.GetHashCode() = intervalClone.GetHashCode()@>
    test <@ not (positiveInterval.GetHashCode() = interval.GetHashCode())@>
    test <@ not (interval.GetHashCode() = differentIncrementInterval.GetHashCode()) @>
    test <@ not (interval.GetHashCode() = differentLBInterval.GetHashCode())@>


  [<Test>]
  let ``Can identify different relationships between intervals`` () =
    let baseInterval =                  Interval.Ints.Between 5     10    // ., ., ., .,[5, 6, 7, 8, 9, 10],.., .., .., .., .. )
    let lesserDisjointInterval =        Interval.Ints.Between 1     3     //[1, 2, 3],., ., ., ., ., ., .., .., .., .., .., .. )
    let greaterDisjointInterval =       Interval.Ints.Between 12    15    // ., ., ., ., ., ., ., ., ., .., ..,[12, 13, 14, 15])
    let lesserAdjacentInterval =        Interval.Ints.Between 1     4     //[1, 2 ,3, 4],., ., ., ., ., .., .., .., .., .., .. )
    let greaterAdjacentInterval =       Interval.Ints.Between 11    15    // ., ., ., ., ., ., ., ., ., ..,[11, 12, 13, 14, 15])
    let lesserOneSharedElemInterval =   Interval.Ints.Between 1     5     //[1, 2 ,3, 4, 5],., ., ., ., .., .., .., .., .., .. )
    let greaterOneSharedElemInterval =  Interval.Ints.Between 10    15    // ., ., ., ., ., ., ., ., .,[10, 11, 12, 13, 14, 15])
    let lesserLargeIntersectInterval =  Interval.Ints.Between 1     7     //[1, 2 ,3, 4, 5, 6, 7],., ., .., .., .., .., .., .. )
    let greaterLargeIntersectInterval = Interval.Ints.Between 7     15    // ., ., ., ., ., .,[7, 8, 9, 10, 11, 12, 13, 14, 15])
    let subsetInterval =                Interval.Ints.Between 6     9     // ., ., ., ., .,[6, 7, 8, 9] .., .., .., .., .., .. )
    let subsetIntervalAlignedLesser =   Interval.Ints.Between 5     9     // ., ., ., .,[5, 6, 7, 8, 9] .., .., .., .., .., .. )
    let subsetIntervalAlignedGreater =  Interval.Ints.Between 6     10    // ., ., ., ., .,[6, 7, 8, 9, 10],.., .., .., .., .. )

    test <@baseInterval.RelationshipTo                 baseInterval                  = Intersecting Identical      @>
    test <@baseInterval.RelationshipTo                 lesserDisjointInterval        = Disjoint                    @>
    test <@baseInterval.RelationshipTo                 greaterDisjointInterval       = Disjoint                    @>
    test <@baseInterval.RelationshipTo                 lesserAdjacentInterval        = Disjoint                    @>
    test <@baseInterval.RelationshipTo                 greaterAdjacentInterval       = Disjoint                    @>
    test <@baseInterval.RelationshipTo                 lesserOneSharedElemInterval   = Intersecting Overlapping    @>
    test <@baseInterval.RelationshipTo                 greaterOneSharedElemInterval  = Intersecting Overlapping    @>
    test <@baseInterval.RelationshipTo                 lesserLargeIntersectInterval  = Intersecting Overlapping    @>
    test <@baseInterval.RelationshipTo                 greaterLargeIntersectInterval = Intersecting Overlapping    @>
    test <@baseInterval.RelationshipTo                 subsetInterval                = Intersecting SuperInterval  @>
    test <@baseInterval.RelationshipTo                 subsetIntervalAlignedLesser   = Intersecting SuperInterval  @>
    test <@baseInterval.RelationshipTo                 subsetIntervalAlignedGreater  = Intersecting SuperInterval  @>
    test <@subsetInterval.RelationshipTo               baseInterval                  = Intersecting SubInterval    @>
    test <@subsetIntervalAlignedLesser.RelationshipTo  baseInterval                  = Intersecting SubInterval    @>
    test <@subsetIntervalAlignedGreater.RelationshipTo baseInterval                  = Intersecting SubInterval    @>
    test <@baseInterval.RelationshipTo                 EmptyInterval                 = Intersecting SuperInterval  @>
    test <@EmptyInterval.RelationshipTo                baseInterval                  = Intersecting SubInterval    @>
    test <@EmptyInterval.RelationshipTo                EmptyInterval                 = Intersecting Identical      @>

  [<Test>]
  let ``Can detect connected intervals`` () =
    let baseInterval =                  Interval.Ints.Between 5     10    // ., ., ., .,[5, 6, 7, 8, 9, 10],.., .., .., .., .. )
    let lesserDisjointInterval =        Interval.Ints.Between 1     3     //[1, 2, 3],., ., ., ., ., ., .., .., .., .., .., .. )
    let greaterDisjointInterval =       Interval.Ints.Between 12    15    // ., ., ., ., ., ., ., ., ., .., ..,[12, 13, 14, 15])
    let lesserAdjacentInterval =        Interval.Ints.Between 1     4     //[1, 2 ,3, 4],., ., ., ., ., .., .., .., .., .., .. )
    let greaterAdjacentInterval =       Interval.Ints.Between 11    15    // ., ., ., ., ., ., ., ., ., ..,[11, 12, 13, 14, 15])
    let lesserOneSharedElemInterval =   Interval.Ints.Between 1     5     //[1, 2 ,3, 4, 5],., ., ., ., .., .., .., .., .., .. )
    let greaterOneSharedElemInterval =  Interval.Ints.Between 10    15    // ., ., ., ., ., ., ., ., .,[10, 11, 12, 13, 14, 15])
    let lesserLargeIntersectInterval =  Interval.Ints.Between 1     7     //[1, 2 ,3, 4, 5, 6, 7],., ., .., .., .., .., .., .. )
    let greaterLargeIntersectInterval = Interval.Ints.Between 7     15    // ., ., ., ., ., .,[7, 8, 9, 10, 11, 12, 13, 14, 15])
    let subsetInterval =                Interval.Ints.Between 6     9     // ., ., ., ., .,[6, 7, 8, 9] .., .., .., .., .., .. )
    let subsetIntervalAlignedLesser =   Interval.Ints.Between 5     9     // ., ., ., .,[5, 6, 7, 8, 9] .., .., .., .., .., .. )
    let subsetIntervalAlignedGreater =  Interval.Ints.Between 6     10    // ., ., ., ., .,[6, 7, 8, 9, 10],.., .., .., .., .. )

    test <@baseInterval.ConnectedTo                  baseInterval                    = true   @>
    test <@baseInterval.ConnectedTo                  lesserDisjointInterval          = false  @>
    test <@baseInterval.ConnectedTo                  greaterDisjointInterval         = false  @>
    test <@baseInterval.ConnectedTo                  lesserAdjacentInterval          = true   @>
    test <@baseInterval.ConnectedTo                  greaterAdjacentInterval         = true   @>
    test <@baseInterval.ConnectedTo                  lesserOneSharedElemInterval     = true  @>
    test <@baseInterval.ConnectedTo                  greaterOneSharedElemInterval    = true  @>
    test <@baseInterval.ConnectedTo                  lesserLargeIntersectInterval    = true  @>
    test <@baseInterval.ConnectedTo                  greaterLargeIntersectInterval   = true  @>
    test <@baseInterval.ConnectedTo                  subsetInterval                  = true  @>
    test <@baseInterval.ConnectedTo                  subsetIntervalAlignedLesser     = true  @>
    test <@baseInterval.ConnectedTo                  subsetIntervalAlignedGreater    = true  @>
    test <@subsetInterval.ConnectedTo                baseInterval                    = true  @>
    test <@subsetIntervalAlignedLesser.ConnectedTo   baseInterval                    = true  @>
    test <@subsetIntervalAlignedGreater.ConnectedTo  baseInterval                    = true  @>
    test <@baseInterval.ConnectedTo                  EmptyInterval                   = true  @>
    test <@EmptyInterval.ConnectedTo                 EmptyInterval                   = true  @>


  [<Test>]
  let ``Can detect adjacent intervals`` () =
    let baseInterval =                  Interval.Ints.Between 5     10    // ., ., ., .,[5, 6, 7, 8, 9, 10],.., .., .., .., .. )
    let lesserDisjointInterval =        Interval.Ints.Between 1     3     //[1, 2, 3],., ., ., ., ., ., .., .., .., .., .., .. )
    let greaterDisjointInterval =       Interval.Ints.Between 12    15    // ., ., ., ., ., ., ., ., ., .., ..,[12, 13, 14, 15])
    let lesserAdjacentInterval =        Interval.Ints.Between 1     4     //[1, 2 ,3, 4],., ., ., ., ., .., .., .., .., .., .. )
    let greaterAdjacentInterval =       Interval.Ints.Between 11    15    // ., ., ., ., ., ., ., ., ., ..,[11, 12, 13, 14, 15])
    let lesserOneSharedElemInterval =   Interval.Ints.Between 1     5     //[1, 2 ,3, 4, 5],., ., ., ., .., .., .., .., .., .. )
    let greaterOneSharedElemInterval =  Interval.Ints.Between 10    15    // ., ., ., ., ., ., ., ., .,[10, 11, 12, 13, 14, 15])
    let lesserLargeIntersectInterval =  Interval.Ints.Between 1     7     //[1, 2 ,3, 4, 5, 6, 7],., ., .., .., .., .., .., .. )
    let greaterLargeIntersectInterval = Interval.Ints.Between 7     15    // ., ., ., ., ., .,[7, 8, 9, 10, 11, 12, 13, 14, 15])
    let subsetInterval =                Interval.Ints.Between 6     9     // ., ., ., ., .,[6, 7, 8, 9] .., .., .., .., .., .. )
    let subsetIntervalAlignedLesser =   Interval.Ints.Between 5     9     // ., ., ., .,[5, 6, 7, 8, 9] .., .., .., .., .., .. )
    let subsetIntervalAlignedGreater =  Interval.Ints.Between 6     10    // ., ., ., ., .,[6, 7, 8, 9, 10],.., .., .., .., .. )

    test <@baseInterval.IsAdjacentTo                 baseInterval                    = false  @>
    test <@baseInterval.IsAdjacentTo                 lesserDisjointInterval          = false  @>
    test <@baseInterval.IsAdjacentTo                 greaterDisjointInterval         = false  @>
    test <@baseInterval.IsAdjacentTo                 lesserAdjacentInterval          = true   @>
    test <@baseInterval.IsAdjacentTo                 greaterAdjacentInterval         = true   @>
    test <@baseInterval.IsAdjacentTo                 lesserOneSharedElemInterval     = false  @>
    test <@baseInterval.IsAdjacentTo                 greaterOneSharedElemInterval    = false  @>
    test <@baseInterval.IsAdjacentTo                 lesserLargeIntersectInterval    = false  @>
    test <@baseInterval.IsAdjacentTo                 greaterLargeIntersectInterval   = false  @>
    test <@baseInterval.IsAdjacentTo                 subsetInterval                  = false  @>
    test <@baseInterval.IsAdjacentTo                 subsetIntervalAlignedLesser     = false  @>
    test <@baseInterval.IsAdjacentTo                 subsetIntervalAlignedGreater    = false  @>
    test <@subsetInterval.IsAdjacentTo               baseInterval                    = false  @>
    test <@subsetIntervalAlignedLesser.IsAdjacentTo  baseInterval                    = false  @>
    test <@subsetIntervalAlignedGreater.IsAdjacentTo baseInterval                    = false  @>
    test <@baseInterval.IsAdjacentTo                 EmptyInterval                   = false  @>
    test <@EmptyInterval.IsAdjacentTo                EmptyInterval                   = false  @>

  [<Test>]
  let ``Can correctly calculate a Convex Hull`` () = 

    let baseInterval =                  Interval.Ints.Between 5     10    // ., ., ., .,[5, 6, 7, 8, 9, 10],.., .., .., .., .. )
    let lesserDisjointInterval =        Interval.Ints.Between 1     3     //[1, 2, 3],., ., ., ., ., ., .., .., .., .., .., .. )
    let greaterDisjointInterval =       Interval.Ints.Between 12    15    // ., ., ., ., ., ., ., ., ., .., ..,[12, 13, 14, 15])
    let lesserAdjacentInterval =        Interval.Ints.Between 1     4     //[1, 2 ,3, 4],., ., ., ., ., .., .., .., .., .., .. )
    let greaterAdjacentInterval =       Interval.Ints.Between 11    15    // ., ., ., ., ., ., ., ., ., ..,[11, 12, 13, 14, 15])
    let lesserOneSharedElemInterval =   Interval.Ints.Between 1     5     //[1, 2 ,3, 4, 5],., ., ., ., .., .., .., .., .., .. )
    let greaterOneSharedElemInterval =  Interval.Ints.Between 10    15    // ., ., ., ., ., ., ., ., .,[10, 11, 12, 13, 14, 15])
    let lesserLargeIntersectInterval =  Interval.Ints.Between 1     7     //[1, 2 ,3, 4, 5, 6, 7],., ., .., .., .., .., .., .. )
    let greaterLargeIntersectInterval = Interval.Ints.Between 7     15    // ., ., ., ., ., .,[7, 8, 9, 10, 11, 12, 13, 14, 15])
    let subsetInterval =                Interval.Ints.Between 6     9     // ., ., ., ., .,[6, 7, 8, 9] .., .., .., .., .., .. )
    let subsetIntervalAlignedLesser =   Interval.Ints.Between 5     9     // ., ., ., .,[5, 6, 7, 8, 9] .., .., .., .., .., .. )
    let subsetIntervalAlignedGreater =  Interval.Ints.Between 6     10    // ., ., ., ., .,[6, 7, 8, 9, 10],.., .., .., .., .. )

    test <@baseInterval.ConvexHull                   baseInterval                    = baseInterval                @>
    test <@baseInterval.ConvexHull                   lesserDisjointInterval          = Interval.Ints.Between 1 10  @>
    test <@baseInterval.ConvexHull                   greaterDisjointInterval         = Interval.Ints.Between 5 15  @>
    test <@baseInterval.ConvexHull                   lesserAdjacentInterval          = Interval.Ints.Between 1 10  @>
    test <@baseInterval.ConvexHull                   greaterAdjacentInterval         = Interval.Ints.Between 5 15  @>
    test <@baseInterval.ConvexHull                   lesserOneSharedElemInterval     = Interval.Ints.Between 1 10  @>
    test <@baseInterval.ConvexHull                   greaterOneSharedElemInterval    = Interval.Ints.Between 5 15  @>
    test <@baseInterval.ConvexHull                   lesserLargeIntersectInterval    = Interval.Ints.Between 1 10  @>
    test <@baseInterval.ConvexHull                   greaterLargeIntersectInterval   = Interval.Ints.Between 5 15  @>
    test <@baseInterval.ConvexHull                   subsetInterval                  = baseInterval                @>
    test <@baseInterval.ConvexHull                   subsetIntervalAlignedLesser     = baseInterval                @>
    test <@baseInterval.ConvexHull                   subsetIntervalAlignedGreater    = baseInterval                @>
    test <@subsetInterval.ConvexHull                 baseInterval                    = baseInterval                @>
    test <@subsetIntervalAlignedLesser.ConvexHull    baseInterval                    = baseInterval                @>
    test <@subsetIntervalAlignedGreater.ConvexHull   baseInterval                    = baseInterval                @>
    test <@baseInterval.ConvexHull                   EmptyInterval                   = baseInterval                @>
    test <@EmptyInterval.ConvexHull                  EmptyInterval                   = EmptyInterval               @>

  [<Test>]
  let ``Detects relative complement between intervals`` () =
    let baseInterval = Interval.Ints.Between 5 10
    let basePositiveInterval = baseInterval |> Interval.asPositive |> Option.unless "Bad Test setup"
    let largerOnLowEnd = Interval.Ints.Between 3 10
    let largerOnUpperEnd = Interval.Ints.Between 5 12
    let largerOnBothEnds = Interval.Ints.Between 3 12

    let lowerComplement = Interval.Ints.Between 3 4 |> Interval.asPositive |> Option.unless "Bad test setup"
    let upperComplement = Interval.Ints.Between 11 12 |> Interval.asPositive |> Option.unless "Bad test setup"

    test <@ baseInterval |> Interval.relativeComplementIn baseInterval = [] @>

    //If the target is larger, this should produce a relative complement.
    test <@ baseInterval |> Interval.relativeComplementIn largerOnLowEnd = [lowerComplement] @>
    test <@ baseInterval |> Interval.relativeComplementIn largerOnUpperEnd = [upperComplement] @>
    test <@ baseInterval |> Interval.relativeComplementIn largerOnBothEnds |> set = ([lowerComplement; upperComplement] |> set) @>

    //If the target is smaller, always returns nothing.
    test <@ largerOnLowEnd   |> Interval.relativeComplementIn baseInterval = [] @>
    test <@ largerOnUpperEnd |> Interval.relativeComplementIn baseInterval = [] @>
    test <@ largerOnBothEnds |> Interval.relativeComplementIn baseInterval = [] @>

    test <@ baseInterval |> Interval.relativeComplementIn EmptyInterval = [] @>
    test <@ EmptyInterval |> Interval.relativeComplementIn baseInterval = [basePositiveInterval]  @>
    test <@ EmptyInterval |> Interval.relativeComplementIn EmptyInterval = []  @>

  [<Test>]
  let ``Partitions Intervals into appropriate sub-intervals`` () =
    let ``5 through 15`` = Interval.Ints.Between 5 15
    let ``Evens 0 through 10`` = Interval.Ints.Evens.Between 0 10
    let ``5 through 9``   = Interval.Ints.Between 05 09
    let ``5 through 10``  = Interval.Ints.Between 05 10
    let ``11 through 15`` = Interval.Ints.Between 11 15
    let ``10 through 15`` = Interval.Ints.Between 10 15

    let ``Evens 0 through 4`` = Interval.Ints.Evens.Between 0 4
    let ``Evens 6 through 10`` = Interval.Ints.Evens.Between 6 10
    
    test <@EmptyInterval |> Interval.partition 10 PivotPartition.Exclusive       = (EmptyInterval, EmptyInterval) @>
    test <@EmptyInterval |> Interval.partition 10 PivotPartition.InclusiveLower  = (EmptyInterval, EmptyInterval) @>
    test <@EmptyInterval |> Interval.partition 10 PivotPartition.InclusiveUpper  = (EmptyInterval, EmptyInterval) @>
    test <@EmptyInterval |> Interval.partition 10 PivotPartition.InclusiveBoth   = (EmptyInterval, EmptyInterval) @>
    test <@EmptyInterval |> Interval.partitionInclusive 10                       = (EmptyInterval, EmptyInterval) @>

    test <@``5 through 15`` |> Interval.partition 10 PivotPartition.Exclusive        = (``5 through 9``  , ``11 through 15``)@>
    test <@``5 through 15`` |> Interval.partition 10 PivotPartition.InclusiveLower   = (``5 through 10`` , ``11 through 15``)@>
    test <@``5 through 15`` |> Interval.partition 10 PivotPartition.InclusiveUpper   = (``5 through 9``  , ``10 through 15``)@>
    test <@``5 through 15`` |> Interval.partition 10 PivotPartition.InclusiveBoth    = (``5 through 10`` , ``10 through 15``)@>
    test <@``5 through 15`` |> Interval.partitionInclusive 10                        = (``5 through 10`` , ``10 through 15``)@>

    test <@``5 through 15`` |> Interval.partitionInclusive -5 = (EmptyInterval, ``5 through 15``)@>
    test <@``5 through 15`` |> Interval.partitionInclusive 20 = (``5 through 15``, EmptyInterval)@>

    test <@``Evens 0 through 10`` |> Interval.partitionInclusive 05 = (``Evens 0 through 4``, ``Evens 6 through 10``) @>
    test <@``Evens 0 through 10`` |> Interval.partitionInclusive -5 = (EmptyInterval, ``Evens 0 through 10``) @>
    test <@``Evens 0 through 10`` |> Interval.partitionInclusive 15 = (``Evens 0 through 10``, EmptyInterval) @>
