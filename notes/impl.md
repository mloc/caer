weird implementation notes

--

Mapping DM's refcounted garbage collector to a tracing one is a bit tricky.

DM:
    Sleeping in Del() - anywhere - cancels the whole fibre.
    If Del() writes a reference to itself somewhere, this reference is nulled once ..() is hit.
        This is the trickiest bit- I'm not sure we can reasonably emulate this.
        Java handles resurrection with a two-pass method, but only runs the finalizer (Del()) *once*. 

caer proposed:
    /datum/proc/Del() is the "terminator" proc. This builtin sets a death flag on the datum.
    When a datum is marked for deletion by the GC, it's put into a finalization queue.
    A finalizer fibre is spawned for each datum in the finalization queue, calling datum.Del().
    A finalizer fibre cannot be suspended- if this happens, it is cancelled, causing a log on debug builds.
    If the datum is marked as dead after the finalizer fibre completes (i.e. the terminator was reached),
     it is placed in a freeing queue.
    After each full GC cycle, the all datums in the freeing queue are freed.
