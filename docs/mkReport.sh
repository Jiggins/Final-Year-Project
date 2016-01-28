#!/bin/bash

pandoc -o Interim.pdf  Interim.md
pandoc -o 12367281.pdf Report.md
open 12367281.pdf
