Text-Based Adventure Domain Specific Language
=============================================

This is a DSL based on Matthew Flatt's CACM article *Creating Languages in
Racket*. I modified it to be slightly easier to read the source code: the
"everywhere" concept was removed, the syntax for defining verbs, items (called
"things" in the original version), and places has changed.

This is probably best accompanied with my DSL lecture available at
https://lambda.mines.edu

To run the example::

   $ racket game.rkt
