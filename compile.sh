#!/bin/bash

# Settings
now=`date +%b%d`
output_dir="Paper"
appendix_dir="Paper_appendix"
bib_file="$HOME/Library/Application Support/BibDesk/library.bib"
data_file="mgh3.RData"

# Create output directory
mkdir $output_dir/{Code,Templates}

echo "Copy bib library..." && cp "$bib_file" $output_dir/
echo "Copy data..." && cp $data_file $output_dir/.RData  && cp timing.csv $output_dir/
echo "Copy Rmd files..." && cp *.Rmd $output_dir/
echo "Copy config..." && cp Config/_output.yml $output_dir/
echo "Copy templates..." && cp Templates/* $output_dir/Templates
echo "Compile report..." && cd $output_dir && crmd report.Rmd > report.Rout 2> report.err
echo "Clean up..." && mv .RData *.Rmd Code/ 
echo "Done!"
exit

# brew install ispell
# excludable
# egotitstic 
# pro-social 
# health care
# resampleing
# precedure
# solicitations
# simular
# Workflow
# fo

# Appendix
echo "Compiling the appendix..."
mkdir -p $appendix_dir/{Code,Templates,Images}
cp $config_dir/_output.yml $appendix_dir
cp Templates/* $appendix_dir/Templates
cp -r Data/Images $appendix_dir
cp .RData *.Rmd $appendix_dir
cd $appendix_dir && crmd report_appendix.Rmd && mv *.Rmd Code/
exit


### Temporary

function compile {
  output_dir="$1"
  mkdir -p $output_dir/{Code,Data/Raw,Templates}
  cp Data/Raw/* $output_dir/Data/Raw && cp .RData *.Rmd $output_dir
  cp Templates/* $output_dir/Templates
  cd $output_dir && crmd report.Rmd && mv *.Rmd Code/
  cd ..
}

if [ "$1" == "--docx" ]; then
  echo "Compiling docx..."
  cp $config_dir/_output_docx.yml $paper_docx_dir
  compile $paper_docx_dir
  
elif [ "$1" == "--pdf" ]; then
  echo "Compiling pdf..."
  cp $config_dir/_output.yml $paper_dir
  compile $paper_dir

elif [ "$1" == "--appendix" ]; then
  echo "Preparing files for appendix..."
  mkdir -p $paper_appendix/{Code,Templates,Images}
  cp $config_dir/_output.yml $paper_appendix
  cp Templates/* $paper_appendix/Templates
  cp -r Data/Images $paper_appendix
  cp .RData *.Rmd $paper_appendix
  echo "Compiling appendix..."
  cd $paper_appendix && crmd report_appendix.Rmd && mv *.Rmd Code/
 
elif [ "$1" == "--all" ]; then
  echo "Compiling all formats..."
  cp $config_dir/_output.yml $paper_dir
  cp $config_dir/_output_docx.yml $paper_docx_dir
  cp $config_dir/_output_html.yml $paper_html_dir
  compile $paper_dir && pwd && compile $paper_docx_dir && compile $paper_html_dir
  
else 
  echo "Compiling html..."
  cp $config_dir/_output_html.yml $paper_html_dir
  compile $paper_html_dir
fi


## Sharable folder
mkdir -p $share_dir
cp $paper_dir/report.pdf $share_dir/mgh_report_$now.pdf
cp $paper_docx_dir/report.docx $share_dir/mgh_report_$now.docx
cp $paper_appendix/report_appendix.pdf $share_dir/mgh_appendix_$now.pdf

