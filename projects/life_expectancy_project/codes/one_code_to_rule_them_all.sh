#!/bin/bash
mkdir data
mkdir figures

python3 codes/scrape_wiki_table.py
Rscript codes/make_graphs.R 

cd ./text/
xelatex Report.tex
open Report.pdf   
