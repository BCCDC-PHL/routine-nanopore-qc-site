#!/bin/bash

rm -r target/deploy/routine-nanopore-qc 2> /dev/null
mkdir -p target/deploy/routine-nanopore-qc
mkdir -p target/deploy/routine-nanopore-qc/js
cp -r resources/public/css target/deploy/routine-nanopore-qc
cp -r resources/public/images target/deploy/routine-nanopore-qc
cp target/public/cljs-out/prod/main_bundle.js target/deploy/routine-nanopore-qc/js/main.js
cp resources/public/index_prod.html target/deploy/routine-nanopore-qc/index.html
pushd target/deploy > /dev/null
tar -czf routine-nanopore-qc.tar.gz routine-nanopore-qc
rm -r routine-nanopore-qc
popd > /dev/null
