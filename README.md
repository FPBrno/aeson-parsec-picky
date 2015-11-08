Picky JSON Parser
=================

JSON parser with nice error messages and little
more strict syntax (whitespace-wise). Based on
[Aeson](http://hackage.haskell.org/package/aeson) and
[Parsec](http://hackage.haskell.org/package/parsec).

Interacting with user
---------------------

JSON being nice readable text-based format seems good candidate
for occasionally created by user. While Aeson provides really
super-optimized parsers, their error messages are not very helpful.
Creating larger JSON object by hand can be frustrating when you
make even small mistake.

While this parses is not optimized for speed, it tries to produce
nice and helpful error messages. (This library uses Parsec library.)

And next step in helping your user is not allowing him or her to
learn wrong things just look at the following piece of code (be
warned - there are trailing spaces there):

~~~ .json
{ "name"   :   
   ,   

"Hal"
}
~~~

That (in my opinion) is something one would not like to see in files
users of his or hers tool produces. So why not forbid that? This
library does not allow such things while still allowing to make
the input more airy.

Composability
-------------

This library was written with re-usability in mind. Parsers it
provides does not consume any spaces before of after JSON values
and therefore they are more easily reusable for your own projects.

Parsing to Aeson data types
---------------------------

Aeson library is nice to work with with large ecosystem of useful
libraries. So why not join them and avoid reinventing the wheel?
