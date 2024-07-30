namespace GasDay.Core.CoreIntervals.Interval

  open GasDay.Core
  open System.Diagnostics.CodeAnalysis
  open SafetyFirst
  open GasDay.Core.Interval
  open GasDay.Core.Interval.Interval

  [<AutoOpen>] //This helps type Signatures look nicer - the "Ints" of "Ints.Consecutive" is less relevant information when you're in "Interval<int, Ints.Consecutive>"
  [<ExcludeFromCodeCoverage>] //Everything in here should be definitional or an alias.
  module Ints =
    
    ///The default unit of an Ints Interval - Integers by one. 
    ///Ints.Consecutive.Of and other incrementor methods may be shortened to simply Ints.Of to reflect the common use case of this incrementor type.
    //Unit: One Integer
    type Consecutive = Consecutive with 
      interface Incrementor<int> with 
        member this.increment = (fun ((d:int), i) -> d + i)
        member this.diff a b = a - b
        member this.isValidElement = fun (i:int) -> true //This describes the most granular level of integers.
        member this.toString (lowerBound:int) (upperBound:int) = (sprintf "Interval<int, Consecutive>[%i .. %i]" lowerBound upperBound)

      //For each of these constructor methods, we want to be able to call them using Type.Incrementor.X. 
      //For example, both C# and F# should be able to call LocalDates.ByDay.Of().
      //F#, on seeing Type.Incrementor, automatically interprets this as a reference to the single instance of the Incrementor type, and thus only accepts interface members.
      //C#, on seeing Type.Incrementor, automatically interprets this as a reference to the type, and could only call Type.Incrementor.X if said X is a static member. 
      //The naming pattern below allows the static member, expected by C#, to be called from C# with a different compiledName, but hidden in F#.
      //C# shouldn't be able to see the local interface member, but if it does, the name should indicate this this method should not be used.

      [<CompiledName("$Of_NotUsedByC#")>]
      member this.Of i = Interval<int, Consecutive>.Create <||| (i, i, Consecutive)

      [<CompiledName("$OfPositive_NotUsedByC#")>]
      member this.OfPositive i = Interval<int, Consecutive>.Create <||| (i, i, Consecutive) |> Interval.asPositive |> Option.unless "Int.Consecutive no longer allows all values allowed in the int type."

      [<CompiledName("Of")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Of_CS i = Consecutive.Consecutive.Of i

      [<CompiledName("OfPositive")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member OfPositive_CS i = Consecutive.Consecutive.OfPositive i

      [<CompiledName("$Around_NotUsedByC#")>]
      member this.Around i (expandLower, expandUpper) = 
        (this.Of i).Resize(expandLower, expandUpper) 

      [<CompiledName("Around")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Around_CS i expansions = Consecutive.Consecutive.Around i expansions

      [<CompiledName("$AroundPositive_NotUsedByC#")>]
      member this.AroundPositive i (expandLower, expandUpper) = 
        (this.OfPositive i).ResizePositive(expandLower, expandUpper)

      [<CompiledName("AroundPositive")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member AroundPositive_CS i expansions = Consecutive.Consecutive.AroundPositive i expansions

      [<CompiledName("$Between_NotUsedByC#")>]
      member this.Between lowerInt upperInt = Interval<int, Consecutive>.Create <||| (lowerInt, upperInt, Consecutive)

      [<CompiledName("Between")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Between_CS lowerInt upperInt = Consecutive.Consecutive.Between lowerInt upperInt

    //Consecutive is the default, so we forward these construction methods.
    let Of i = Consecutive.Consecutive.Of i
    let Between loweri upperi = Consecutive.Consecutive.Between loweri upperi
    let Around i expansions = Consecutive.Consecutive.Around i expansions

    ///Unit: Every other integer, only valid for even integers
    type Evens = Evens with 
      interface Incrementor<int> with
        member this.increment = (fun ((d:int), i) -> d + (2*i))
        member this.diff a b = (a - b) / 2
        member this.isValidElement = (fun (i:int) -> i%2 = 0)
        member this.toString (lowerBound:int) (upperBound:int) = (sprintf "Interval<int, Evens>[%i .. %i]" lowerBound upperBound)

      //For each of these constructor methods, we want to be able to call them using Type.Incrementor.X. 
      //For example, both C# and F# should be able to call LocalDates.ByDay.Of().
      //F#, on seeing Type.Incrementor, automatically interprets this as a reference to the single instance of the Incrementor type, and thus only accepts interface members.
      //C#, on seeing Type.Incrementor, automatically interprets this as a reference to the type, and could only call Type.Incrementor.X if said X is a static member. 
      //The naming pattern below allows the static member, expected by C#, to be called from C# with a different compiledName, but hidden in F#.
      //C# shouldn't be able to see the local interface member, but if it does, the name should indicate this this method should not be used.

      [<CompiledName("$Of_NotUsedByC#")>]
      member this.Of i = Interval<int, Evens>.Create <||| (i, i, Evens) 

      [<CompiledName("Of")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Of_CS i = Evens.Evens.Of i

      [<CompiledName("$Around_NotUsedByC#")>]
      member this.Around i (expandLower, expandUpper) = 
        match (this.Of i) with 
        | PositiveInterval p -> p.Resize(expandLower, expandUpper) 
        | EmptyInterval -> EmptyInterval

      [<CompiledName("Around")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Around_CS i expansions = Evens.Evens.Around i expansions

      [<CompiledName("$Between_NotUsedByC#")>]
      member this.Between lowerInt upperInt = Interval<int, Evens>.Create <||| (lowerInt, upperInt, Evens)

      [<CompiledName("Between")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Between_CS lowerInt upperInt = Evens.Evens.Between lowerInt upperInt


    ///Unit: Every other integer, only valid for odd integers
    type Odds = Odds with 
      interface Incrementor<int> with
        member this.increment = (fun ((d:int), i) -> d + (2*i))
        member this.diff a b = (a - b) / 2
        member this.isValidElement = (fun (i:int) -> i%2 = 1)
        member this.toString (lowerBound:int) (upperBound:int) = (sprintf "Interval<int, Odds>[%i .. %i]" lowerBound upperBound)

      //For each of these constructor methods, we want to be able to call them using Type.Incrementor.X. 
      //For example, both C# and F# should be able to call LocalDates.ByDay.Of().
      //F#, on seeing Type.Incrementor, automatically interprets this as a reference to the single instance of the Incrementor type, and thus only accepts interface members.
      //C#, on seeing Type.Incrementor, automatically interprets this as a reference to the type, and could only call Type.Incrementor.X if said X is a static member. 
      //The naming pattern below allows the static member, expected by C#, to be called from C# with a different compiledName, but hidden in F#.
      //C# shouldn't be able to see the local interface member, but if it does, the name should indicate this this method should not be used.

      [<CompiledName("$Of_NotUsedByC#")>]
      member this.Of i = Interval<int, Odds>.Create <||| (i, i, Odds) 

      [<CompiledName("Of")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Of_CS i = Odds.Odds.Of i

      [<CompiledName("$Around_NotUsedByC#")>]
      member this.Around i (expandLower, expandUpper) = 
        match (this.Of i) with 
        | PositiveInterval p -> p.Resize(expandLower, expandUpper) 
        | EmptyInterval -> EmptyInterval

      [<CompiledName("Around")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Around_CS i expansions = Odds.Odds.Around i expansions

      [<CompiledName("$Between_NotUsedByC#")>]
      member this.Between lowerInt upperInt = Interval<int, Odds>.Create <||| (lowerInt, upperInt, Odds)

      [<CompiledName("Between")>]
      [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
      static member Between_CS lowerInt upperInt = Odds.Odds.Between lowerInt upperInt


namespace GasDay.Core.CoreIntervals.PositiveInterval

  open System.Diagnostics.CodeAnalysis
  open GasDay.Core.CoreIntervals.Interval.Ints

  [<AutoOpen>] //This helps type Signatures look nicer - the "Ints" of "Ints.Consecutive" is less relevant information when you're in "Interval<int, Ints.Consecutive>"
  [<ExcludeFromCodeCoverage>] //Everything in here should be definitional or an alias.
  module Ints =
    let Of i = Consecutive.Consecutive.OfPositive i