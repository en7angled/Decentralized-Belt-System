#!/bin/bash

set -e

echo "Building Decentralized Belt System..."

# Build the executables locally first
echo "Step 1: Building executables with cabal..."
cabal install interaction-api --enable-executable-static --overwrite-policy=always --installdir=compiled  --install-method=copy 
cabal install query-api --enable-executable-static --overwrite-policy=always --installdir=compiled  --install-method=copy 

# Check if build was successful
if [ $? -eq 0 ]; then
    echo "✅ Build successful!"
    
    # Build the Docker images
    echo "Step 2: Building Docker images..."
    docker build -f Dockerfile.interaction-api -t bjj-interaction-api:latest .
    docker build -f Dockerfile.query-api -t bjj-query-api:latest .
    
    if [ $? -eq 0 ]; then
        echo "✅ Docker images built successfully!"
        echo "Images:"
        echo "  - bjj-interaction-api:latest"
        echo "  - bjj-query-api:latest"
        echo ""
        echo "To run with docker-compose:"
        echo "docker-compose up -d"
        echo ""
        echo "To run individual containers:"
        echo "docker run -it --rm -p 8082:8082 bjj-interaction-api:latest"
        echo "docker run -it --rm -p 8083:8083 bjj-query-api:latest"
    else
        echo "❌ Docker build failed!"
        exit 1
    fi
else
    echo "❌ Cabal build failed!"
    exit 1
fi
