# What is a useful way of thinking about aggregate analysis

If you are faced with a function which has an expensive bit, imagine running
that function $n$ times starting from the beginning state. How many times do you
have to run expensive operations.

Note, there may be multiple expensive operations in the example of the binary
counter. In my mind, each index in the length of our counter (each bit) is one
of our expensive operations. The first bit is the most expensive at $n$
occurrences, then they decrease in frequency in powers of two.

# What is my intuitive way of thinking about $\Delta\phi$

You can figure out potential intuitively by asking the question "how work did I
take away from my expensive operation if I was to run it straight after I did
said operation".

This is useful in say a $PUSH$, $POP$, and $MULTIPOP$ scenario as it is easy to
conceptualise that $PUSH$ causes our expensive function one extra unit of work,
$POP$ alleviates one unit of work, and $MULTIPOP$ releases all the work.

# What is the major constraint for amortised complexity

For any function $f_{i}$

$$
  \sum_{i = 1}^{n}a_{i}\leq \sum_{i = 1}^{n} c_{i}
$$ {#eq-am-comp-main-constraint}

where $a_{i}$ is the amortised cost of the function and $c_{i}$ is the real cost
of the function

# Why does must we have $\phi D_{i} \leq \phi D_{0}$

Because of @eq-am-comp-main-constraint, we have the following:

$$
  \begin{aligned}
  &&\sum a_{i}&= \sum c_{i} +
    \underbrace{\sum [\phi D_{i} - \phi D_{i - 1}]}_{\text{Telescoping sum}}\\
  &\Rightarrow &\sum a_{i}& = \sum c_{i} + \phi D_{n} - \phi D_{0}
  \end{aligned}
$$

Hence our $D_{0}$ term cannot be greater than our $D_{n}$ term otherwise the
equality in @eq-am-comp-main-constraint would not hold.
