# elm representer

- [x] try some real code and see how it looks
- [ ] show to matthieu for feedback
- [ ] probably write some end to end type tests
- [ ] things exposed need to be normalised as well really
- [x] output as elm code, so easier to se thast going on
- [x] let statements
- [x] case statements
- [x] lamda
- [x] Pattern, and uses of it in other already mostly done types¨
- [ ] Value constructors in TypeDeclaration
- [ ] RecordSetters
- [ ] maybe TypeAnnotation in Type alias? Maybe this isn't required and is just a documentation thing. Probably we should just strip it out as its a comment.
- [ ]  mybe qualifiedNameRef, related to module names / type names. Probablt not needed as only only relevant for types imported from other modules, and probably there aren't any custom ones of those in elm exercism
- [ ] maybe Expression.FunctionOrValue moduleName. Probably not for same reason as above
- maybe others
- maybe function signature  / type annotation - although lets ignore for now

i am just outputting cedd for the identifiers at the moment, need to keep an incrementing id and output identifier 1 etc. This is probably a good first step.
- pass a dictionary around with mapping from original name 

then I want to include context, so that a variable called 's' in one context gets a different identifier to an 's' in another contet