#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

VM_EXECUTABLE=./pharo

function generate_html() {
    pier_source="$1"
	filename="${pier_source%.*}"
    $VM_EXECUTABLE Pharo.image export --to=html --outputFile=$filename.html $pier_source
}

function generate_md() {
    pier_source="$1"
	filename="${pier_source%.*}"
    $VM_EXECUTABLE Pharo.image export --to=markdown --outputFile=$filename.md $pier_source
}

function generate_latex() {
    pier_source="$1"
	filename="${pier_source%.*}"
    $VM_EXECUTABLE Pharo.image export --to=latex --outputFile=$filename.tex $pier_source
}

function mypdflatex() {
    pier_file="$1"

    echo "Compiling PDF..."
    pdflatex -halt-on-error -file-line-error -interaction batchmode "$pier_file" 2>&1 1>/dev/null
    ret=$?
    if [[ $ret -ne 0 ]]; then
        cat $pier_file.log
        echo "Can't generate the PDF!"
        exit 1
    fi
}

function produce_pdf() {
    dir="$1"
    pier_file="$2"

    cd "$dir"         # e.g., cd Zinc/
    mypdflatex "$pier_file" && mypdflatex "$pier_file"
    cd ..
}

function compile() {
    dir="$1"
    pier_file="$2"
    pier_source="$PWD/${dir}/${pier_file}"
    generate_html "$pier_source"
	generate_md "$pier_source"

    generate_latex "$pier_source"
    produce_pdf "${dir}" "${pier_file}"
}

function compile_chapters() {
    $VM_EXECUTABLE Pharo.image export --to=markdown --outputFile=book.md --configurationFile="$1"
    $VM_EXECUTABLE Pharo.image export --to=latex --outputFile=book.tex --configurationFile="$1"
    $VM_EXECUTABLE Pharo.image export --to=html --outputFile=book.html --configurationFile="$1"
}

if [[ $# -eq 1 ]]; then
	if [[ "$1" == "chapters" ]]; then
		compile_chapters "chapters.conf"
	else
    	dir=$(dirname "$1") # e.g., Zinc
    	pier_file=$(basename "$1") # e.g., Zinc.pier
    	compile "${dir}" "${pier_file}"
	fi
else
	compile_chapters "book.conf"
fi
