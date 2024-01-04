#!/bin/sh

cd ..

git pull

npm install

npm run-script build
