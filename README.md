# Segment Tree in Haskell
A segment tree is a useful data structure that can handle range queries and updates in $O(\log n)$ time.

Using Haskell’s type classes we can create a more polymorphic segment tree that can create a segment tree out of any monoid, that is any element with a binary operation and identity element.
