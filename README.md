# hydrant

Very simple markup combinators for constructing HTML.

- No typeclasses
- Few intermediate datatypes
- Nothing but Text, Builders and newtypes
- No knowledge of any HTML standards
- Tag, attribute, tree construction combinators
- Entity escaping functions
- No pretty-printing, as injecting whitespace alters the rendering semantics
  - Feel free to use one as a post-processor though
  - Maybe hydrant could provide one as an optional thing
