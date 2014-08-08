# Campaign Vulture

A clojure-based tool that converts Campaign Finance Report PDFs to CSV

## Usage

Currently, parsing is done in two steps: one to create the config files needed
to extract the values on each page and a parsing step. There are two modes to
accomplish this: `parse` and `config`. Both options are passed with the `-m` or
`--mode` command-line arguments.

### Building configuration files

There are two config files this tool needs: `config.cfg` and
`delta.cfg`.  Using our test PDF (and leiningen) we can create the
example config files like this:

    lein run -- -m config -c config/config.cfg -d config/delta.cfg

This will write our two example config files to the config/
directory. The `config.cfg` file tells our tool which example variables
to look for, from which PDF, and from which page. It also has
information about how many values-per-header the parser should look for.

The `delta.cfg` file stores the header string along with delta values
that tell the parser where it will find our desired values. Currently,
the config files need to be generated and then modified by hand. In the
future, this will become more flexible and user-friendly.

### Parsing PDFs

Once our config files have been generated, we can use them to parse our
PDF:

    lein run -- -m parse -c config.cfg -d delta.cfg data/2012CFRpts.pdf out.csv 2 10

Or if we want the parser to try and guess a range of contributor pages,
we can supply the `-r true` argument (this currently looks for a fuzzy match
of the string "other than pledges or loans", which will be configurable in the
future):

    lein run -- -m parse -c config.cfg -d delta.cfg -r true data/2012CFRpts.pdf out.csv

Running this will write a CSV file to out.csv which contains the
information from a contributor cell in `data/test.pdf`. I don't clean up
the output or convert it to a delimited format. As you can see, it's
fairly accurate as-is, with the exception of some line breaks and
formatting oddities.

## License

There are incompatabilities between GPL and Snowtide's PDFTextStream
license (proprietary). This makes it impossible to distribute it
as-is. Eventually I will port this to a free PDF parsing library, but
until then you're on your own for installing PDFTextStream.

Copyright © 2013 Brandon Robertz GPLv3+ (w/ considering PDFTextStream a
system lib) RIP EWOK BATES
