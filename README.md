Async IO
--------

A small library for performing asynchronous IO operations. Built atop Haskell's
existing Control.Concurrent library, it adds basic support for a fork-join
model using futures, as well as allowing the concatenation of IO actions into
combined asynchronous tasks.
