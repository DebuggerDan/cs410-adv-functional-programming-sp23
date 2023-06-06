# CS410 [Adv. Functional Programming]- Project #1, Dan Jang

## Objective: *Implement a simple dictionary compression algorithm that takes an input text-file and displays an compressed output*.

### Language: *Haskell* (proj1.hs)

---

Libraries
---------

1. System.IO
2. System.Environment(getArgs)
3. **Data.List.Split(split, oneOf)**
   1. May require install, e.g. *cabal installl --lib split*
4. Data.Maybe(fromJust)
5. Data.List(sortBy, groupBy, sort)
6. Data.Either(rights)
7. Data.Char(isSpace))
8. Data.Map.Strict
9. Data.Set


#### ***How To Use:***

**Command-line usage**: runhaskell proj1.hs [options: 'compress' or 'decompress'] [text file path here!]

For the option argument, you may also enter any other string besides above, except whitespaces, to have the program automatically both compress & decompress (assuming input text file starts off un-compressed)!
