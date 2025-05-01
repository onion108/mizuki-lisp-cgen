# How to use the generator

## What is code
Codes are list of strings representing code lines, like `'("first-line" "second-line")`.

To write code represented in such structure to a file, `write-code` will do the job. Basically you just call `(write-code code file-name)` and the code will be written into file name, as long as the code is represented in said structure. In the rest of the documentation, the structure will be called **code segment**, to distinct from code lines. A **code line** and its plural only refers to string(s) that represent(s) (a) code line(s).

> [!WARNING]
> Currently we don't have strict type check yet, so not following the rule might result in unpredictable behavior or fortunately a crash. I'm working on this, don't be rush!

`codes` is an utility function to concatenate code lines and code segments. It receives arbitrary amount of code lines or code segments, and returns a big code segment as a result.

## Generating definition

Currently the only type of definition we could generate is function. To define a function, we use `cdef-function`. Here is a brief example of how to use it to define the main function:
```lisp
(cdef-function "int" "main" '("int argc", "char **argv") '("// Body goes here"))
```

## Generating statements
To generate statements, you can use `cstmt-*` functions. The currently supported statements are `cstmt-if`, `cstmt-switch`, `cstmt-for`, `cstmt-while`, `cstmt-do-while`. Most of them are pretty straightforward, but `cstmt-if` and `cstmt-switch` introduces new structures, that will be documented here. (Oh damn dynamically typed languages)

So basically, we introduces a structure called conditional code segments. It is a list of lists, where each list in it is a code segment prepend with a condition, or `nil` representing the default case (which might become `else` in `if`-`else if`-`else` chain or `default: ` in `switch`). We provide another utility function called `cdpr` to create a code segment prepended with condition. It is used just like `codes`, but with an extra condition before code lines or segments.

Considering how often `if (...) { ... } else { ... }` is used, we provides an alias `cstmt-ifelse` to conveniently construct such structure, without writing conditional code segments over and over again.

