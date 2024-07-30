# Interval
A selection of helper code from MEA's library to resolve issues we had had when using similar approaches, such as a Range library and/or sequence expresssions.
Graciously provided by my team lead Nathan Wilson(https://github.com/ntwilson), as an example of code I was the primary author of that I wrote for work applications.

Nearly all of this code was written by me, starting ~2019. The exception here is `Cache.fs`, which was written mostly by Nathan. 

The objective of this helper code was to handle an interval of data; a set of values that are iterable, ordered, and whose iteration can have arbitrary distances assigned.

At the time this was written we found this particularly useful as Nodatime provided neither the `DateInterval` type, nor the `.GetEnumerator()`, `.Intersection()`, or `.Union()` members. 

Our most common use case was generating ranges of dates via either `LocalDates.Between` or `LocalDates.Around (x, y)`. Comparisons and `ConvexHull` were also used fairly frequently. `Ints` was used much less commonly but did occasionally provide some value.

The core logic is in `Interval.fs`, but generally the modules we used were `CoreIntervals.LocalDates.fs` and `CoreIntervals.Ints.fs`. These describe and wrap `Incrementor`s in usable chunks. 

Looking at this in retrospect I'd probably have done a few things differently. It would be worth considering replacing this code with NodaTime native functions as tech debt, to leverage the advantages of having this functionality maintained by the open source community. That being said, when I left, this code had required almost no maintenance and worked well for what we used it for, so that would be low priority tech debt.