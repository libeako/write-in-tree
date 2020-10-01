#/bin/bash

package_dir=$1
demo_dir=$1/demo
output_dir=$package_dir/product/demo
copy="rsync --times --force --whole-file"

mkdir -p $output_dir
$demo_dir/program translate -i $demo_dir/main -o $output_dir
$copy $demo_dir/style.css $output_dir/c/
$copy --recursive --links --delete  $demo_dir/a $output_dir/c/
