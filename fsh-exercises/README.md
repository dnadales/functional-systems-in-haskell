# FSH exercises

## Generating html from my notes

Some notes are written in literate Haskell. For generating html (or any) other
format, use `pandoc`:

```sh
pandoc src/Notes.lhs -f markdown+lhs -t html -s -o Notes.html

```
