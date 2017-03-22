#!/bin/bash

# Prep directories
rm -rf dist
mkdir dist

# Frontend build
cd frontend
export PORT=80
npm run clean && npm run compile
cp -r dist ../dist/frontend

cd ../

# Backend build
cd backend
stack install 
cp .stack-work/install/x86_64-linux/lts-8.3/8.0.2/bin/arbital-exe ../dist/backend-exe