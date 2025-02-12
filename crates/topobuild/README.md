# Topobuild

This is another exploration into my build system. I'm going to attempt
to implement a dependency graph, which can provide elements in build order.

## Build Graphs

Given a DAG of build dependencies, there's basically one thing we
need it to do. Yield the next dependency that *can* be built, given
the dependencies that we have already built.

Let's say that we have the set of all dependencies that we
will need to produce the objective. My current mindset requires
that this be known up front (certain build systems, such as
Nix I believe can side step this requirement using IFD, and such,
for more info, read "Build Systems a la carte"). However,
for this use case, let's keep it "simple" and make sure that
we know both the whole set of dependencies *and* the way that
they depend on each other.

Let's say that a *build step* is
`(dependency, hashset of indexes of dependencies on this dependency)`.

So now let's assume that we can figure out a transitive reduction of
our DAG. This provides us with the *minimal* set of dependencies that
we'll need to compute dependency graph. 

So here's where it gets intricate, let's assume that we want a structure
that is at least reasonably convenient time complexity wise. If we did a
list, one option we could do, is, after every build step, we
could iterate over every other build step, and search the list
and remove the element. While this would work, in a graph of
1000 dependencies, we might iterate over 5000+ edges to find what, 3 connections?

This tells me that I feel like we *should* be able to do better than that,
and my goal for this project is to be very performance minded.

One thing we could do is compute the linearized DAG, and then store them in
chunks and then only search the most leading chunks, and surely this
will actually improve efficiently a lot. However, this does seem a
tad rough on cache coherency / is sorta allocation heavy (I could
easily see this causing 20k+ allocations), and while my senses
tell me in a fast allocator, that that won't take crazy long, it doesn't
seem very good, so this probably isn't my first choice for what we'll do.

Hmm, what *if*, instead, we maintain a backwards index into every dependency
as well. So every *build step* is actually describle as

 1. dependency 
 2. hashset of indexes of dependencies on this dependency
 3. vector of indexes of dependents on this dependency

Now I believe we have the conditions that we're after,
I can say, "can I build this yet", by asking if 2 is empty. If we complete
building this dependency, then we can iterate over the elements in (3), and
cancel them out of future builds. This gives us O(1) perf for checking if an
element can be built right now, and O(k) where k is the number of
reverse dependencies when this dependency has finished building to update the
graph. So that's a very reasonable time complexity, I think that's a great
approach to go for.

## Yielding Elements

So, despite what I've written above, I've still not fully solved how to
quickly yield the *next* thing that can be built. So that's really a problem,
I'm not exactly sure how I would do that right away. Currently I can imagine
searching the rest of the vector to see, but that seems wildly inefficient.


Okay, but what if instead, when we do our walk, we place each dependency which
has zero requirements into it's own list. Now when we must yield something,
we can simply yield it from that place. This preserves all the properties
that we're after.

1. We can yield a "ready-to-build" build step, or report that
   one is not ready yet in O(1) time.

2. We can create new "ready-to-build" steps in `O(k)` time, where
   `k` is the number of reverse dependencies for our dependency.

Our time complexity for an entire build is `O(n + e)`, so even though
we're parallelized, we have an optimal time complexity, because we're
at space complexity. We can't express a DAG in less space by excluding
either nodes or edges. So our algorithm is therefore optimal, and is
*probably* worth implementing.


## Determinism.

Here's a real question, is our build order deterministic? I would argue
it is definitely *not*. This spells some trouble for us, because
while, in a perfect world, all our
builds would be encapsulated in a perfect sandbox, we don't have that,
so build order absolutely *could* influence outcome, even though we really
never want it to. This is deeply problematic. Howver, in a multithreaded
build, I guess they can race anyway, so I don't think this sort of
determinism is really something that we can control for anyway. I feel
like this is still a slight weakening, but I think it's worth it for
the extra speed.


