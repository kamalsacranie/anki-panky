# General TODOs

- [ ] Users should be able to have a panky json config which allows modification
  of all the default JSON settings.
- [ ] Fix the maybe types in our JOSN objects
- [ ] Progress form using the default deck now to using multiple decks
- [ ] Check what a filtered deck is
- [x] We need to create a new monad wherever we use genInfo
- [ ] Integrate pandoc crossref
- [ ] Fix bug where if we comment out the yaml header, the cards does not
  process properly

# How are we going to handle nested cards

We can either only accept one folder which is in a deck hierarchy structure, or
we can accept a bunch of file which we will then put on the same level. I think
we should prioritise implementing the deck hierarchy options.

Then if we want to name the level of the deck, we are going to have to use a
dotfile which is named the name of the deck. For example, we would use ".This is
the name of the deck" as a file in the directory to name the deck.
