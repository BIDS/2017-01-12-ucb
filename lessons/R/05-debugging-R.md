> Learning Objectives
> -------------------
>
> -   Exposure to different debugging strategies and options in R
> -   Explain strategies to minimize time debugging
> -   Debug a function

When I am embarking upon a particular analysis, I first outline the
different functions I will need, their input and outputs. I then write
the functions in the order in which they are needed, testing/debugging
the functions along the way.

### Basic debugging strategies

(modified by a tutorial from Chris Paciorek)

Read and think about the error message. Sometimes it's inscrutable, but
often it just needs a bit of deciphering. Looking up a given error
message in the [R mailing list
archive](http://tolstoy.newcastle.edu.au/R) or on Stack Overflow or
simply doing a web search with the exact message in double quotes can be
a good strategy.

Fix errors from the top down --- fix the first error that is reported,
because later errors are often caused by the initial error. It's common
to have a string of many errors, which looks daunting, caused by a
single initial error.

Is the bug reproducible -- does it always happen in the same way at at
the same point? It can help to restart R and see if the bug persists --
this can sometimes help in figuring out if there is a scoping issue
(your function is not looking for objects where you think it is) and we
are using a global variable that we did not mean to.

Another basic strategy is to build up code in pieces (or tear it back in
pieces to a simpler version). This allows you to isolate where the error
is occurring.

If you've written your code modularly (functions are not nested within
each other) with lots of functions, you can test individual functions.
Often the error will be in what gets passed into and out of each
function.

At the beginning of time (the 1970s?), the standard debugging strategy
was to insert print statements in one's code to see the value of a
variable and thereby decipher what could be going wrong. We have better
tools nowadays.

R is a scripting language, so you can usually run your code line by line
to figure out what is happening. This can be a decent approach,
particularly for simple code. However, when you are trying to find
errors that occur within a series of many nested function calls or when
the errors involve variable scoping (how R looks for variables that are
not local to a function), or in other complicated situations, using
formal debugging tools can be much more effective. Finally, if the error
occurs inside of functions provided by R, rather than ones you write, it
can be hard to run the code in those functions line by line.

Some common causes of bugs
--------------------------

Some of these are R-specific, while others are common to a variety of
languages.

-   Parenthesis mis-matches
-   `[[...]]` vs. `[...]`
-   `==` vs. `=`
-   Silent type conversion when you don't want it, or lack of coercion
    where you're expecting it!!!!!!
-   Using the wrong function or variable name
-   Giving unnamed arguments to a function in the wrong order
-   Forgetting to define a variable in the environment of a function and
    having R, get that variable as a global variable from one of the
    enclosing environments. At best the types are not compatible and you
    get an error; at worst, you use a garbage value and the bug is hard
    to trace. In some cases your code may work fine when you develop the
    code (if the variable exists in the enclosing environment), but then
    may not work when you restart R if the variable no longer exists or
    is different.

More insidious bugs - Comparing real numbers exactly using `==` is
dangerous because numbers on a computer are only represented to limited
numerical precision. For example,

    ```r
    1/3 == 4*(4/12-3/12)
    ```

    ```
    ## [1] FALSE
    ```

-   You want to compare an entire vector but your code just compares the
    first value (e.g., in an if statement) -- consider using *identical*
    or *all.equal*
-   In an if-else statement, the `else` cannot be on its own line
    (unless all the code is enclosed in `{}`) because R will see the
    `if` part of the statement, which is a valid R statement, will
    execute that, and then will encounter the `else` and return
    an error.

General tips to avoid bugs
--------------------------

-   Use core R functionality and algorithms already coded. Figure out if
    a functionality already exists in (or can be adapted from) an R
    package (or potentially in a C/Fortran library/package): code that
    is part of standard mathematical/numerical packages will probably be
    more efficient and bug-free than anything you would write.
-   Code in a modular fashion, making good use of functions, so that you
    don't need to debug the same code multiple times. Smaller functions
    are easier to debug, easier to understand, and can be combined in a
    modular fashion (like the UNIX utilities).
-   Write code for clarity and accuracy first; then worry
    about efficiency. Write an initial version of the code in the
    simplest way, without trying to be efficient (e.g., you might use
    for loops even if you're coding in R); then make a second version
    that employs efficiency tricks and check that both produce the
    same output.
-   Plan out your code in advance, including all
    special cases/possibilities.
-   Write tests for your code early in the process.
-   Build up code in pieces, testing along the way. Make big changes in
    small steps, sequentially checking to see if the code has broken on
    test case(s).
-   Don't hard code numbers
-   use variables (e.g., number of iterations, parameter values in
    simulations), even if you don't expect to change the value, as this
    makes the code more readable and reduces bugs when you use the same
    number multiple times; e.g. `speedOfLight <- 3e8` or `nIts <- 1000`.

#### R options for debigging

Interactive debugging via the browser
-------------------------------------

The core strategy for interactive debugging is to use the *browser*
function, which pauses the current execution, and provides an
interpreter, allowing you to view the current state of R. You can invoke
*browser* in four ways

-   by inserting a call to `browser()` in your code if you suspect where
    things are going wrong

-   by invoking the browser after every step of a function using *debug*

-   by using `options(error = recover)` to invoke the browser when an
    error occurs

-   by temporarily modifying a function to allow browsing using *trace*

Once in the browser, you can execute any R commands you want. In
particular, using *ls* to look at the objects residing in the current
function environment, looking at the values of objects, and examining
the classes of objects is often helpful.

Using *debug* to step through code
----------------------------------

To step through a function, use `debug(nameOfFunction)`. Then run your
code. When the function is executed, R will pause execution just before
the first line of the function. You are now using the browser and can
examine the state of R and execute R statements.

Once in the browser context, you can use 'n' or return to step to the
next line, 'c' to execute the entire current function or current loop,
and \`' to stop debugging. We'll see this in the screencast demo.

To unflag the function so that calling it doesn't invoke debug, use
`undebug(nameOfFunction)`. In addition to working with functions you
write you can use *debug* with standard R functions and functions from
packages. For example you could do `debug(glm)`.

Tracing errors in the call stack
--------------------------------

*traceback* and *recover* allow you to see the call stack at the time of
the error - i.e., they will show you all the functions that have been
called, in the order called. This helps pinpoint where in a series of
function calls the error may be occurring.

If you've run the code and gotten an error, you can invoke *traceback*
after things have gone awry. R will show you the call stack, which can
help pinpoint where an error is occurring.

More helpful is to be able to browse within the call stack. To do this
invoke `options(error = recover)` (potentially in your *.Rprofile* if
you do a lot of programming). Then when an error occurs, *recover* gets
called, usually from the function in which the error occurred. The call
to *recover* allows you to navigate the stack of active function calls
at the time of the error and browse within the desired call. You just
enter the number of the call you'd like to enter (or 0 to exit). You can
then look around in the frame of a given function, entering an empty
line when you want to return to the list of calls again.

You can also combine this with `options(warn = 2)`, which turns warnings
into errors to get to the point where a warning was issued.

Using *trace* to temporarily insert code
----------------------------------------

*trace* lets you temporarily insert code into a function (including
standard R functions and functions in packages!) that can then be easily
removed. You can use trace in a variety of ways.

The most flexible way to use *trace* is to use the argument
`edit = TRUE` and then insert whatever code you want wherever you want
in the function given as the first argument to *trace*. If I want to
ensure I use a particular editor, such as emacs, I can use the argument
`edit = “emacs”`. A standard approach would be to add a line with
`browser()` at some point in the function to be able to step through the
code from that point.

You can also use *trace* without directly editing the function. Here are
a couple examples:

-   `trace(lm, recover)` \# invoke *recover* when the function (*lm* in
    this case) starts
-   `trace(lm, exit = browser)` \# invoke *browser* when the function
    ends

You call *untrace*, e.g., `untrace(lm)`, to remove the temporarily
inserted code; otherwise it's removed when the session ends.

To figure out why warnings are being issued, you can do
`trace(warning, recover)` which will insert a call to *recover* whenever
*warning* is called.

Of course you can manually change the code in a function without using
*trace*, but it's very easy to forget to change things back (and a pain
to remember exactly what you changed) and hard to do this with functions
in packages, so *trace* is a nice way to do things.

Challenge - Debugging
---------------------

\`\`\`r hb &lt;- read.csv('data/beeOperations.csv') hb &lt;-
hb\[order(hb$year),\]

plotVar &lt;- function(hb, var, ylabs, types=c("hobby", "semicom",
"com"), div=10^3){ \#\# function to plot honey bee data by region \#\#
takes hb, a data.frame with the varaibles to be plotted \#\# var, the
name of the column of interest \#\# types, the different types of honey
bee keepers \#\# div, a scalar to divide the values on the yaxis by to
avoid \#\# ugly numbers
layout(matrix(1:(length(unique(hb*r**e**g**i**o**n*)) + 1),*n**c**o**l* = 2))*p**a**r*(*o**m**a* = *c*(3, 3, 2, 1),*m**a**r* = *c*(2, 2, 3, 1),*m**g**p* = *c*(2, 1, 0),*c**e**x*.*a**x**i**s* = 1.5)*n**t**y**p**e**s* &lt; −*l**e**n**g**t**h*(*t**y**p**e**s*)*f**o**r*(*i**i**n**u**n**i**q**u**e*(*h**b*region)){
this.region &lt;- hb\[hb$region == i,\]
plot(x=this.region*y**e**a**r*\[*t**h**i**s*.*r**e**g**i**o**n*type ==
types\[1\]\], y=this.region\[,var\]\[this.region$type == types\[1\]\],
type="l", ylim=c(0, max(hb\[,var\], na.rm=TRUE)), xlab="Year",
ylab=ylabs, yaxt="n", main=i) ltys &lt;- c("dashed", "dotted") for(j in
2:length(types)){
points(x=this.region*y**e**a**r*\[*t**h**i**s*.*r**e**g**i**o**n*type ==
types\[j\]\], y=this.region\[,var\]\[this.region$type == types\[j\]\],
type="l", lty=ltys\[j\]) } if(i %in% unique(hb$region)\[1:4\]){ axis(2,
pretty(c(0, max(hb\[,var\], na.rm=TRUE))), labels=pretty(c(0,
max(hb\[,var\], na.rm=TRUE)))/div) } } }

plotVar(hb, "hives", "Hives") plotVar(hb, "apiaries", "Apiaries")
plotVar(hb, "beekeepers", "Beekeepers") plotVar(hb, "hives\_beekeeper",
"Hives per beekeeper") plotVar(hb, "apiary\_beekeeper", "Apiaries per
beekeeper", div=1) \`\`\`
