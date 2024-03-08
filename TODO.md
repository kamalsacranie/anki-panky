# General TODOs

- [ ] Users should be able to have a panky json config which allows modification
  of all the default JSON settings.
- [x] Fix the maybe types in our JSON objects
- [x] Progress form using the default deck now to using multiple decks
- [ ] Check what a filtered deck is
- [x] We need to create a new monad wherever we use genInfo
- [ ] Integrate pandoc crossref (nvm we can use the internal cross ref
  functionality extension)
- [x] Fix bug where if we comment out the yaml header, the cards does not
  process properly
- [x] make everything lazy
- [ ] Assign hard-coded model values to models as they are otherwise duplicated
  importing the deck again after regeneration.
- [x] Allow notes to be tagged in the markdown file
- [ ] Look into builtin pandoc xrefs extension
- [ ] homogenise the mili epoch times
- [ ] Finish type implementations for JSON objects
- [x] Decouple the renaming of deck from deck rendering
- [ ] Look how to properly use the maybe monad so I can do error handling more
  nicerer
- [ ] Parse CLI args more efficiently
- [x] Use special html to have tags
- [x] Make a monad which has the config for the programme as specified from the
  command line. Lonnnngggggg
