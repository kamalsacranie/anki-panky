<!-- TODO: Make Logo With Project Title underneath -->

Anki Panky is a **Pandoc Markdown** to [Anki](https://apps.ankiweb.net)
***hierarchical*** flashcard deck generator.

> Basically, most stuff you can do in Pandoc translates into HTML into your
> cards!!!!! How good

# Two-minute sales pitch

I really *don't* like to do any reasonable amount of typing-centric work outside
of my [Personal Development
Environment](https://youtu.be/QMVIJhC9Veg?si=hbYKs0gk7xSrLfuw). As such, I find
the process of making decks in Anki's GUI to be an insanely hard task which I
almost certainly never end up doing. There is just too much friction for me to
want to invest the time to do so. I really *love* writing Markdown notes about
anything that I'm learning. I feel **productive as shit**, I can use **version
control**, and I can use any ecosystem I plugins my heart desires because at the
end of the day, I'm just writing text in a code editor. But probably the most
important thing is that I can use [Pandoc](https://pandoc.org) to render my
markdown to a lovely PDF, slide deck, or web-page using the same source file.
[Pandoc's markdown syntax](https://pandoc.org/MANUAL.html#pandocs-markdown)
makes writing detailed, technical markdown documents quite easy.

The desire to use Pandoc-style markdown to write my Anki decks is the titular
reason for this bit of software. The second reason was that I couldn't find any
other software which did exactly what I wanted. The compilation functionality I
wanted for the MVP was the following:

- The **directory structure** in which my markdown files are kept **reflects the
  Anki deck/collection structure**. I.e., The root directory is the base name of
  the deck ("Deck::{subdeck-name}::...etc").
- The **syntax** that I use to define cards should be flexible enough to let me
  create single-line, and multi-line "fronts" of cards **without having to
  resort to putting raw HTML in my document**.
- The **syntax** should also be non-invasive when compiling to PDFs of beamer
  slides. This is because I may want to print the cards out and review them in
  reality in the case where I have an exam.
- Use media from **wherever I want in my file system**.
- There should be some basic avoidance for duplication when importing to Anki.

# Feature overview

- Uses bog-standard Pandoc syntax
- Syntax doesn't interfere with other Pandoc output formats
- Arbitrarily nested collection directory structure
- Use images from anywhere in your file system. They **do not need to be
  colocated with your markdown decks**.
- You can use any file extension for your markdown decks. They don't need to
  have a `.md`
- You can add tags to your cards

# Card syntax

If you have a short question for the front of your card, you can use an H1 to
represent the front of your question card. Then the content which follows **up
until the next card (H1)** represents the back of your card:

```markdown
# What is the capital of France?

Paris

# What if I want to use maths in my flashcards

Just fucking do this:

\begin{equation*}
  x = \dfrac{-b \pm \sqrt{4ac}}{2a}
\end{equation*}

or this

$$
\begin{aligned}
&\int_{-\infty}^{\infty} e^{-x^2/2} \left( \frac{\partial H}{\partial t} +
\nabla \cdot (\mathbf{v} H) - K \Delta H \right) dx \\
&\qquad = \int_{-\infty}^{\infty} e^{-x^2/2} \left( -\frac{\partial V}{\partial
x} + \mathbf{v} \cdot \nabla H + H \nabla \cdot \mathbf{v} - K \Delta H \right)
dx \\
&\qquad = \int_{-\infty}^{\infty} e^{-x^2/2} \left( -\frac{\partial V}{\partial
x} + \frac{\partial}{\partial x} \left( H v_x \right) + \frac{\partial}{\partial
y} \left( H v_y \right) + \frac{\partial}{\partial z} \left( H v_z \right)
\right. \\
&\qquad = 0
\end{aligned}
$$
```

If you want a multi-line heading, the you should start the slide with a
horizontal rule and then split the slide with the [Pandoc slide pause
syntax](https://pandoc.org/MANUAL.html#inserting-pauses) as follows:

```markdown
---

This is now the front of the card.

. . .

This is now the back of the card.
```

Or an alternative syntax for multi-line front cards is to use an H1, The title
of which gets put into the front of the card too.

```markdown
# This is a title for the front of the card

The fact that there is a dotted split line below this means that this is an
extended card. Hence, this is still the front

. . .

This is now the back
```

## Adding tags to cards

You can also assign tags to cards by using the `AnkiTags` HTML tag. The tags for
the card is the list of space-separated tags which follow the tag. For example:

```markdown
. . .

This is the back of my card

<AnkiTags this-is my tag list/>
```

Note that the tags must be at the **end of the card** as the last paragraph.

## Images

You can use images as you would in a regular markdown document. The get mapped
correctly in the .apkg file.

```markdown
![This is the alt text](path/to/image.png)
```

---

For some examples of how to use anything, you can check the `test-dir` file in
the repo.

# Naming your deck/subdecks

The default name for your collection/deck is the name of the input file/folder.
If you want to specify the name of a markdown deck **file**, you can add the
following metadata to the top of your file:

```markdown
---
name: My Deck Name
---
```

If my file name was for example `my-deck.md`, then the name of the deck would be
"{prefix}::My Deck Name".

**You can name a folder of decks using a dotfile**. If you want all decks in a
folder to be prefixed by a certain name, you can add a `.My Deck Name` file to
the (the name of the file can be anything). Hence, the name of your collection
will be "{prefix}::My Deck Name::{subdeck-name}::...etc".

> Note, ***you can have arbitrarily nested directories***

# To be added

- [ ] specify output directory with the `-o`/`--output <path>` option
