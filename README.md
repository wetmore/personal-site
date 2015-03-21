# Source for mattwetmore.me

The website is powered by [Hakyll](http://jaspervdj.be/hakyll/), with a few small extensions. The module `CustomCompilers` includes a Pandoc compiler with math extensions enabled, as well as a compiler that allows custom writers in Pandoc. I use a custom writer largely based on [this example](https://github.com/jgm/pandoc/blob/master/data/sample.lua) to add sidenotes to the HTML output.

Hakyll stores the compiled site in a folder called `_site`, which I don't track in this repository. The shell script `publish.sh` (which accepts a commit message as an argument)  commits the contents of `_site` to https://github.com/wetmore/wetmore.github.com and pushes them to Github, which hosts the site.
