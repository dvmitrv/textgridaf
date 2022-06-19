# textgridaf

`textgridaf` provides a parser-serializer pair for Praat [TextGrid] files,
built on top of [Angstrom] and [Faraday].

[TextGrid]: <https://www.fon.hum.uva.nl/praat/manual/TextGrid_file_formats.html>
[Angstrom]: <https://github.com/inhabitedtype/angstrom>
[Faraday]: <https://github.com/inhabitedtype/faraday>



## Encoding and supported formats

The parser accepts input in ASCII, UTF-8, or UTF-16 (BE or LE), while the
serializer currently only outputs UTF-8.  Both the parser and the serializer
accept (and produce, respectively) the _short_ and _full_ TextGrid formats.



## Time position representation

Praat uses double-precision floating point values to encode positions in time.
This can lead to situations where infinitesimal intervals are created
to maintain TextGrid's _adjacency_ invariant.

In order to provide greater numerical stability, `textgridaf` provides two
implementations for the parser-serializer pair: one using double-precision
floating point values to represent offsets in seconds, and one using integers
to represent offsets in microseconds.

