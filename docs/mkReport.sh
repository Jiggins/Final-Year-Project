#!/bin/bash

pandoc -o Interim.pdf  Interim.md --standalone 
pandoc -o 12367281.pdf Report.md  --standalone --template=MaynoothUniversity.tex
open 12367281.pdf
