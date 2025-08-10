#!/bin/bash

set -e

echo "Building Decentralized Belt System..."

# Build the executables locally first
echo "Step 1: Building executables with cabal..."
cabal install server --enable-executable-static --overwrite-policy=always --installdir=compiled  --install-method=copy --ghc-options="-optl=-static -optl=-pthread -optc=-static"

# Check if build was successful
if [ $? -eq 0 ]; then
    echo "✅ Build successful!"
    
    # Build the runtime Docker image
    echo "Step 2: Building Docker runtime image..."
    docker build -f Dockerfile.runtime-only -t decentralized-belt-system:runtime .
    
    if [ $? -eq 0 ]; then
        echo "✅ Docker image built successfully!"
        echo "Image: decentralized-belt-system:runtime"
        echo ""
        echo "To run the container:"
        echo "docker run -it --rm decentralized-belt-system:runtime"
        echo ""
        echo "To run with custom config:"
        echo "docker run -it --rm -v \$(pwd)/config:/app/config decentralized-belt-system:runtime"
    else
        echo "❌ Docker build failed!"
        exit 1
    fi
else
    echo "❌ Cabal build failed!"
    exit 1
fi
