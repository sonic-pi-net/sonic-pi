# Documentation/Translation tools

## Quick guide
### Generate docs for Qt GUI
When building Sonic Pi, run the scripts in the following order:

```bash
  # Translate
  i18n-tool.rb -t
  # Generate HTML
  create-html.rb
  # Format and generate TOC for Qt GUI
  generate-qt-docs.rb
```

### Generate HTML book
```bash
  # Translate
  i18n-tool.rb -t
  # Generate HTML
  create-html.rb
  # Format it as a one page HTML document (located in /app/gui/qt/book/)
  generate-book.rb
```

### Generate HTML site (experimental)
```bash
  # Translate
  i18n-tool.rb -t
  # Generate HTML
  create-html.rb
  # Generate website
  generate-site.rb

  # Install the required gems and serve the website locally
  cd ../../../../../etc/html/
  ./bundle-gems.sh
  ./serve.sh
```


### Update docs and translation files with changes
To update the docs to reflect changes to the base reference and tutorial, do:

```bash
  # Extract reference docs to JSON
  extract-reference.rb
  # Update .po files with changes to reference and en tutorial
  i18n-tool.rb -u
```

## Usage

### extract-reference.rb
Usage: `extract-reference.rb`

Extracts the reference documentation and generates JSON files in /etc/doc/reference/


### i18n-tool.rb
Usage `i18n-tool.rb <options>`

Options:
* `-t` - Generate translated docs from .po files
* `-x` - Extract to an English .pot file
* `-u` - Update translation (.po) files

Handles documentation translations


### create-html.rb
Usage: `create-html.rb [-l LANG]`

Options:
* `-l LANG` - Specify the language to generate (if empty or not specified, defaults to all languages)

Generates individual HTML files for each doc page in /etc/doc/generated_html/


### generate-qt-docs.rb
Usage `generate-qt-docs.rb [-o HEADER_FILE]`

Options:
* `-o HEADER_FILE` - specify the output header file for the Qt TOC (default: /app/gui/qt/utils/ruby_help.h)

Copy help pages, generate info pages, and generate help TOC for Qt GUI

### generate-book.rb
Usage: `generate-book.rb [-l LANG]`

Options:
* `-l LANG` - Specify the language to generate (if empty or not specified, defaults to all languages)

Generates single page HTML files for each section in /app/gui/qt/book/

### generate-site.rb
Usage: `generate-book.rb [-l LANG]`

Options:
* `-l LANG` - Specify the language to generate (if empty or not specified, defaults to all languages)

Generates the source files for a Jekyll site
