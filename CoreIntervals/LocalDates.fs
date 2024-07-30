namespace GasDay.Core.CoreIntervals.Interval

  open NodaTime
  open System.Diagnostics.CodeAnalysis
  open SafetyFirst
  open GasDay.Core
  open GasDay.Core.Interval

  [<AutoOpen>] //This helps type Signatures look nicer - the "LocalDates" of "LocalDates.ByDay" is less relevant information when you're in "Interval<LocalDate, LocalDates.ByDay>"
  [<ExcludeFromCodeCoverage>] //Everything in here should be definitional or an alias.
  module public LocalDates =
  
    ///The default unit of a LocalDates Interval - LocalDates separated by one calendar day. 
    ///LocalDates.ByDay.Of may be shortened to simply LocalDates.Of to reflect the common use case of this unit type, as with other methods of this incrementor
    //Unit: One Calendar day, as described by LocalDate
    type ByDay = ByDay with 

      interface Incrementor<LocalDate> with 
        member this.increment = (fun ((d:LocalDate), i) -> d.PlusDays(i))
        member this.diff a b = Period.DaysBetween(b, a)
        member this.isValidElement = fun (d:LocalDate) -> true //LocalDate's most granular level is ByDay
        member this.toString lowerBound upperBound = (sprintf "Interval<LocalDate, ByDay>[%A .. %A]" lowerBound upperBound)
      
      //For each of these constructor methods, we want to be able to call them using Type.Incrementor.X. 
      //For example, both C# and F# should be able to call LocalDates.ByDay.Of().
      //F#, on seeing Type.Incrementor, automatically interprets this as a reference to the single instance of the Incrementor type, and thus only accepts interface members.
      //C#, on seeing Type.Incrementor, automatically interprets this as a reference to the type, and could only call Type.Incrementor.X if said X is a static member. 
      //The naming pattern below allows the static member, expected by C#, to be called from C# with a different compiledName, but hidden in F#.
      //C# shouldn't be able to see the local interface member, but if it does, the name should indicate this this method should not be used.
      [<CompiledName("$Of_NotUsedByC#")>]
      member this.Of localDate = Interval<LocalDate, ByDay>.Create <||| (localDate, localDate, ByDay)

      [<CompiledName("$OfPositive_NotUsedByC#")>]
      member this.OfPositive localDate = Interval<LocalDate, ByDay>.Create <||| (localDate, localDate, ByDay) |> Interval.asPositive |> Option.unless "LocalDate.ByDay no longer allows all values allowed in the LocalDate type."

      [<CompiledName("Of")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Of_CS localDate = ByDay.ByDay.Of localDate

      [<CompiledName("OfPositive")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member OfPositive_CS localDate = ByDay.ByDay.OfPositive localDate
      
      [<CompiledName("$Around_NotUsedByC#")>]
      member this.Around localDate (expandLower, expandUpper) = 
        (this.Of localDate).Resize(expandLower, expandUpper) 

      [<CompiledName("Around")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Around_CS localDate expansions = ByDay.ByDay.Around localDate expansions

      [<CompiledName("$AroundPositive_NotUsedByC#")>]
      member this.AroundPositive localDate (expandLower, expandUpper) = 
        (this.OfPositive localDate).ResizePositive(expandLower, expandUpper) 

      [<CompiledName("AroundPositive")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member AroundPositive_CS localDate expansions = ByDay.ByDay.AroundPositive localDate expansions

      [<CompiledName("$Between_NotUsedByC#")>]
      member this.Between lowerDate upperDate = 
        Interval<LocalDate, ByDay>.Create <||| (lowerDate, upperDate, ByDay)

      [<CompiledName("Between")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Between_CS lowerDate upperDate = ByDay.ByDay.Between lowerDate upperDate

    let Of localDate = ByDay.ByDay.Of localDate
    let Between lowerLocalDate upperLocalDate = ByDay.ByDay.Between lowerLocalDate upperLocalDate
    let Around localDate expansions = ByDay.ByDay.Around localDate expansions

namespace GasDay.Core.CoreIntervals.PositiveInterval

  open System.Diagnostics.CodeAnalysis
  open GasDay.Core.CoreIntervals.Interval.LocalDates

  [<AutoOpen>]
  [<ExcludeFromCodeCoverage>]
  module LocalDates =
    let Of i = ByDay.ByDay.OfPositive i