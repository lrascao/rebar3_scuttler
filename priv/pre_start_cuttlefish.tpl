#!/bin/bash

echo "generating .config files from schema dir"
echo "schema dir: {{schema_dir}}"
echo "output dir: {{output_dir}}"
echo "output filename: {{output_filename}}"

./bin/cuttlefish --log_level debug \
                 --etc_dir etc \
                 --dest_dir {{output_dir}} \
                 --dest_file {{output_filename}} \
                 --schema_dir {{schema_dir}} \
                 --conf_file {{conf_file}} \
                 --allow_extra \
                 --silent

