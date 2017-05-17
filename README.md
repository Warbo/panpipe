# PanPipe #

This software is released into the Public Domain
  -- Chris Warburton <chriswarbo@gmail.com>, 2014-09-28

## Usage ##

Tested on GNU/Linux, might work on other POSIX systems.

You'll need some way to run Haskell. Check your package manager or go to
https://www.haskell.org/platform/ to get a compiler or a `runhaskell`
interpreter.

You'll also need Pandoc available as a library, which you can get from your
package manager or with `cabal install pandoc`, and will probably want the
`pandoc` command available too.

To use PanPipe, invoke it as a Pandoc "filter", like this:

`pandoc --filter ./panpipe input_file > output_file`

You can also run `panpipe` as a standalone command, but note that its stdio
should be in PanDoc's JSON format, rather than e.g. raw Markdown. You can
convert *to* PanDoc JSON using a command like `pandoc -t json`, and convert
*from* PanDoc JSON using `pandoc -f json`. The `--filter` argument automates
this plumbing.

## Intro ##

PanPipe is a simple Haskell script using PanDoc. It allows code blocks in
PanDoc-compatible documents, eg. Markdown, to be sent to external programs for
processing.

Any code blocks or lines with a "pipe" attribute will have the contents of that
attribute executed as a shell command. The body of the block/line will be piped
to that command's stdin, and the stdout will replace the body of that
block/line. A non-zero exit code will cause PanPipe to exit with that code;
stderr will be sent to PanPipe's stderr.

For example, we can execute shell scripts by piping them to "sh":

````
```{pipe="sh"}
echo "Hello world"
```
````

This will cause "sh" to be called, with 'echo "Hello world"' as its stdin.
It will execute the echo command, to produce 'Hello world' as its stdout. This
will become the new contents of the code block, so in the resulting document
this code block will be replaced by:

````
```
Hello world
```
````

## Usage Notes ##

### Attributes ###

The "pipe" attribute is removed, but other attributes, classes and IDs remain:

````
```{#foo .bar baz="quux" pipe="sh"}
echo 'Hello'
```
````

Will become:

````
```{#foo .bar baz="quux"}
Hello
```
````

### Execution Order ###

PanPipe uses two passes: in the first, all code *blocks* are executed, in the
order they appear in the document. Hence later blocks can rely on the effects of
earlier ones. For example:

````
```{pipe="sh"}
echo "123" > /tmp/blah
echo "hello"
```

```{pipe="sh"}
cat /tmp/blah
```
````

Will become:

````
```
hello
```

```
123
```
````

The second pass executes *inline* code, in the order they appear in the
document.

### Environment ###

Commands will inherit the environment from the shell which calls panpipe, except
they will all be executed in a temporary directory. This makes it easier to
share data between code, without leaving cruft behind:

````
```{pipe="sh"}
echo "hello world" > file1
echo "done"
```

```{pipe="sh"}
cat file1
```
````

Will become:

````
```
done
```

```
hello world
```
````

The temporary directory will contain a symlink called "root" which points to
wherever Pandoc was called from. This allows resources to be shared across
invocations (although it's not recommended to *modify* anything in root).

### Imperative Blocks ###

If you want to execute a block for some effect, but ignore its output, you can
hide the result using a class or attribute:

````
```{.hidden pipe="python -"}
import random
with open('entropy', 'w') as f:
    f.write(str(random.randint(0, 100)))
```
````

When rendered to HTML will produce:

```html
<pre class="hidden"><code></code></pre>
```

### Program Listings ###

A common use-case is to include a program listing in a document *and* show the
results of executing it. You can do this by passing the source code to the Unix
"tee" command, then using a subsequent shell script to run it:

````
```{.python pipe="tee script1.py"}
print "Foo bar baz"
```

```{pipe="sh"}
python script1.py
```
````

Will become:

````
```{.python}
print "Foo bar baz"
```

```
Foo bar baz
```
````

### Changing Block Order ###

Blocks will always be executed in document-order, so you must arrange dependent
blocks appropriately. However, we can display blocks in any order by saving them
to files and dumping them later.

For example, to show the output of a program *before* its source code listing,
we can define the program first, using "tee" to save it to a file and a HTML
class to hide the listing in the resulting document:

````
```{.hidden pipe="tee script2.py"}
print "Hello world"
```
````

Next we can include a block which executes the file we created:

````
```{pipe="sh"}
python script2.py
```
````

Finally we can include a listing by having a block dump the contents of the file
(using ".python" for syntax highlighting):

````
```{.python pipe="sh"}
cat script2.py
```
````

### Inline Snippets ###

PanPipe also works on inline code snippets; for example, my root filesystem is
currently at `` `df -h | grep "/$" | grep -o "[0-9]*%"`{pipe="sh"} `` capacity.

### PanHandle ###

PanPipe keeps the results of script execution inside code blocks/lines, where
they can't interfere with the formatting. If you want to splice some of these
results back into the document, you can use the PanHandle script which was
written to complement PanPipe.

To prevent ambiguity, PanHandle requires data to be in PanDoc's JSON format. We
can convert things to that format using the `-t json` option to the `pandoc`
command. For example, to generate a Markdown list and insert it into the
document, we can do this:

````
```{.unwrap pipe="python - | pandoc -t json"}
for n in range(5):
    print " - Element " + str(n)
```
````

If we run this document through PanPipe, the Python code will output the
following to its stdout:

```
 - Element 0
 - Element 1
 - Element 2
 - Element 3
 - Element 4
```

This will be transformed by `pandoc -t json` into the following:

```
[{"unMeta":{}},[{"t":"BulletList","c":[[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"0"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"1"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"2"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"3"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"4"}]}]]}]]
```

Hence PanPipe will end up giving out a document equivalent to the following:

````
``` {.unwrap}
[{"unMeta":{}},[{"t":"BulletList","c":[[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"0"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"1"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"2"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"3"}]}],[{"t":"Plain","c":[{"t":"Str","c":"Element"},{"t":"Space","c":[]},{"t":"Str","c":"4"}]}]]}]]
```
````

Running *that* code through PanHandle will splice the contents into the document
to give (a JSON equivalent of):

```
 - Element 0
 - Element 1
 - Element 2
 - Element 3
```

Note that this is no longer in a codeblock, so it will render like this:

 - Element 0
 - Element 1
 - Element 2
 - Element 3

Hence we can use PanPipe to obtain or generate data, and PanHandle to splice it
into the document in a sensible way. For example, it's easy to include one
Markdown document inside another:

````
```{.unwrap pipe="sh"}
cat /some/file.md | pandoc -t json
```

```{.unwrap pipe="sh"}
wget -O- http://some.site/some/markdown | pandoc -t json
```
````
