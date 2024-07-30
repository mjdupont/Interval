namespace GasDay.Core.Functional

open System
open System.Collections.Concurrent
open System.Diagnostics.CodeAnalysis

open FSharpx


/// <summary>
/// copied from https://github.com/louthy/language-ext/blob/master/LanguageExt.Core/Prelude/Prelude_Memoize.cs
/// the LanguageExt version will throw an OutOfMemoryException when external memory pressure is high.  This fixes that
/// </summary>
type private OnFinalise<'V> (onFinalize : unit -> unit, value : 'V) =
  override this.Finalize () = onFinalize ()
  member this.Value = value


/// <summary>
/// copied from https://github.com/louthy/language-ext/blob/master/LanguageExt.Core/Prelude/Prelude_Memoize.cs
/// the LanguageExt version will throw an OutOfMemoryException when external memory pressure is high.  This fixes that
/// Used internally by the memo function.  It wraps a concurrent dictionary that has 
/// its value objects wrapped in a WeakReference<OnFinalise<...>>
/// The OnFinalise type is a private class within WeakDict and does nothing but hold
/// the value and an Action to call when its finalised.  So when the WeakReference is
/// collected by the GC, it forces the finaliser to be called on the OnFinalise object,
/// which in turn executes the action which removes it from the ConcurrentDictionary.  
/// That means that both the key and value are collected when the GC fires rather than 
/// just the value.  Mitigates memory leak of keys.
/// </summary>
[<ExcludeFromCodeCoverage>]
type private WeakDict<'key, 'value> () =

  let inner = new ConcurrentDictionary<'key, WeakReference<OnFinalise<'value>>> ()

  let newRef (key:'key) value =
    new WeakReference<OnFinalise<'value>> (
      new OnFinalise<'value> (
        (fun () -> ignore (inner.TryRemove(key))),
        value
      )
    )

  member this.TryGetValue key = 
    match inner.TryGetValue(key) with
    | (true, res) ->
      match res.TryGetTarget () with
      | (true, target) -> Some target.Value
      | (false, _) -> None
    | (false, _) -> None

  member this.GetOrAdd (key:'key, addFunc) =
    let res = inner.GetOrAdd(key, Func<'key, WeakReference<OnFinalise<'value>>>(fun _ -> 
      let ans = addFunc key
      newRef key ans))

    match res.TryGetTarget () with
    | (true, target) -> target.Value
    | (false, _) ->
      let ans = addFunc key
      let upd = newRef key ans
      ignore (inner.AddOrUpdate(key, upd, Func<'key, WeakReference<OnFinalise<'value>>, WeakReference<OnFinalise<'value>>>(fun _ _ -> upd)))
      ans


module Cache =
    
  /// <summary>
  /// Returns a <c>'a -> 'b</c> that wraps <c>f</c>.  Each time the resulting
  /// <c>'a -> 'b</c> is called with a new value, its result is memoized (cached).
  /// Subsequent calls use the memoized value.  
  /// 
  /// Remarks: 
  ///     Thread-safe and memory-leak safe.  
  /// </summary>
  /// <remarks>
  /// copied from https://github.com/louthy/language-ext/blob/master/LanguageExt.Core/Prelude/Prelude_Memoize.cs
  /// the LanguageExt version will throw an OutOfMemoryException when external memory pressure is high.  This fixes that
  /// </remarks>
  let memoize (f : 'a -> 'b) =
    let cache = new WeakDict<'a, 'b>()
    let syncMap = new ConcurrentDictionary<'a, obj>()

    fun inp ->
      match cache.TryGetValue(inp) with
      | Some x -> x
      | None ->
        let sync = syncMap.GetOrAdd(inp, new obj());
        let res = 
          lock sync (fun _ -> 
            cache.GetOrAdd(inp, f)
          )
        ignore (syncMap.TryRemove(inp))
        res

  let memoize2 f =
    let memoizedAndTupled = memoize (uncurry f)
    curry memoizedAndTupled
