#!/bin/bash

pandoc -o Interim.pdf  Interim.md --standalone 
if pandoc -o 12367281.pdf Report.md  --standalone --template=MaynoothUniversity.tex --bibliography references.bib; then
  open 12367281.pdf
fi
