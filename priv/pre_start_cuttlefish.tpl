#!/bin/bash

# echo "generating files from schema"
# echo "schema dir: {{schema_dir}}"
# echo "output dir: {{output_dir}}"
# echo "output filename: {{output_filename}}"

# we want relx to start our script for us, he's the one who
# knows where escript is located
relx_escript ./bin/cuttlefish \
                 --etc_dir etc \
                 --dest_dir {{output_dir}} \
                 --dest_file {{output_filename}} \
                 --schema_dir {{schema_dir}} \
                 --conf_file {{conf_file}} \
                 --allow_extra \
                 --silent

