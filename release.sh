#!/bin/bash
set -e

# Release automation script for sffdr R package
# Automates version bumping, documentation, building, checking, and release creation
#
# Usage:
#   ./release.sh              # Run full workflow interactively
#   ./release.sh --build      # Skip to building R package
#   ./release.sh --check      # Skip to checking R package
#   ./release.sh --conda      # Skip to building conda package
#   ./release.sh --push       # Skip to push/release step

# Parse command-line arguments
SKIP_TO=""
if [[ $# -gt 0 ]]; then
    case $1 in
        --build)
            SKIP_TO="build"
            ;;
        --check)
            SKIP_TO="check"
            ;;
        --conda)
            SKIP_TO="conda"
            ;;
        --push)
            SKIP_TO="push"
            ;;
        --help|-h)
            echo "Usage: $0 [OPTION]"
            echo "Options:"
            echo "  --build     Skip to building R package"
            echo "  --check     Skip to checking R package"
            echo "  --conda     Skip to building conda package"
            echo "  --push      Skip to push/release step"
            echo "  --help      Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
fi

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: Not in a git repository"
    exit 1
fi

# Check for uncommitted changes (skip if jumping to later steps)
if [[ -z "$SKIP_TO" ]] || [[ "$SKIP_TO" == "build" ]]; then
    if ! git diff-index --quiet HEAD --; then
        echo "Warning: You have uncommitted changes."
        read -p "Continue anyway? [y/N]: " continue_choice
        if [[ ! $continue_choice =~ ^[Yy]$ ]]; then
            echo "Aborting."
            exit 1
        fi
    fi
fi

# Activate conda environment
echo "Activating sffdr-dev conda environment..."
source $(conda info --base)/etc/profile.d/conda.sh
conda activate sffdr-dev

# Version bump and documentation (skip if requested)
if [[ -z "$SKIP_TO" ]]; then
    # Prompt for version bump
    echo "Do you want to bump the version?"
    echo "1) patch (x.y.Z)"
    echo "2) minor (x.Y.0)"
    echo "3) major (X.0.0)"
    echo "4) Skip version bump"
    read -p "Enter choice [1-4]: " version_choice
    
    case $version_choice in
        1)
            version_type="patch"
            echo "Bumping $version_type version..."
            R -e "usethis::use_version('$version_type')"
            ;;
        2)
            version_type="minor"
            echo "Bumping $version_type version..."
            R -e "usethis::use_version('$version_type')"
            ;;
        3)
            version_type="major"
            echo "Bumping $version_type version..."
            R -e "usethis::use_version('$version_type')"
            ;;
        4)
            echo "Skipping version bump..."
            ;;
        *)
            echo "Invalid choice"
            exit 1
            ;;
    esac
    
    echo "Updating documentation..."
    R -e "devtools::document()"
    
    echo "Updating dependencies in DESCRIPTION..."
    R -e "attachment::att_amend_desc()"
fi

# Build R package (skip if jumping to check/conda/push)
if [[ -z "$SKIP_TO" ]] || [[ "$SKIP_TO" == "build" ]]; then
    echo "Building package..."
    R CMD build .
fi

# Determine latest tarball
latest=$(ls -t sffdr_*.tar.gz | head -n 1)
echo "Latest build: $latest"

# Check R package (skip if jumping to conda/push)
if [[ -z "$SKIP_TO" ]] || [[ "$SKIP_TO" == "build" ]] || [[ "$SKIP_TO" == "check" ]]; then
    echo "Running R CMD check..."
    R CMD check $latest --no-manual --no-build-vignettes
fi

# Extract version and SHA (needed for conda/push steps)
if [[ -z "$SKIP_TO" ]] || [[ "$SKIP_TO" == "build" ]] || [[ "$SKIP_TO" == "check" ]]; then
    # Extract version from filename (platform-independent)
    version=$(echo "$latest" | sed 's/sffdr_\(.*\)\.tar\.gz/\1/')
    
    # Compute SHA256 (works on both Linux and macOS)
    if command -v sha256sum > /dev/null 2>&1; then
        sha256=$(sha256sum "$latest" | awk '{print $1}')
    else
        sha256=$(shasum -a 256 "$latest" | awk '{print $1}')
    fi
    
    echo "Version: $version"
    echo "SHA256: $sha256"
    
    echo "Updating recipe.yml..."
    # Platform-independent sed (use perl instead)
    perl -i -pe "s/^(\\s*)version: [0-9]+\\.[0-9]+\\.[0-9]+/\${1}version: $version/" recipe.yml
    perl -i -pe "s/sha256: .*/sha256: $sha256/" recipe.yml
    
    echo "Staging changes for commit..."
    git add man DESCRIPTION NAMESPACE recipe.yml
    
    echo "Amending commit..."
    git commit --amend --no-edit
    
    echo "Creating git tag..."
    git tag "v$version"
else
    # Extract version from DESCRIPTION for conda/push steps
    version=$(grep "^Version:" DESCRIPTION | sed 's/Version: //')
    echo "Using version from DESCRIPTION: $version"
    latest=$(ls -t sffdr_${version}.tar.gz 2>/dev/null || ls -t sffdr_*.tar.gz | head -n 1)
    echo "Using tarball: $latest"
fi

# Push and release (skip if jumping to conda only)
if [[ -z "$SKIP_TO" ]] || [[ "$SKIP_TO" == "build" ]] || [[ "$SKIP_TO" == "check" ]] || [[ "$SKIP_TO" == "push" ]]; then
    read -p "Push to remote and create release? [y/N]: " push_choice
    if [[ $push_choice =~ ^[Yy]$ ]]; then
        current_branch=$(git branch --show-current)
        echo "Pushing branch $current_branch..."
        git push -f origin "$current_branch"
        
        echo "Pushing tag v$version..."
        git push -f origin "v$version"
        
        echo "Creating GitHub release..."
        gh release create "v$version" $latest --notes-from-tag
    else
        echo "Skipping push and release creation."
        echo "You can manually push later with:"
        echo "  git push origin $(git branch --show-current)"
        echo "  git push -f origin v$version"
        echo "  gh release create v$version $latest --notes-from-tag"
    fi
fi

# Build conda package (always run unless skipped earlier)
read -p "Build conda package with rattler-build? [y/N]: " build_choice
if [[ $build_choice =~ ^[Yy]$ ]]; then
    echo "Select target platform:"
    echo "1) linux-64"
    echo "2) osx-arm64"
    read -p "Enter choice [1-2]: " platform_choice
    
    case $platform_choice in
        1)
            target_platform="linux-64"
            ;;
        2)
            target_platform="osx-arm64"
            ;;
        *)
            echo "Invalid choice"
            exit 1
            ;;
    esac
    
    echo "Building with rattler-build for $target_platform..."
    rattler-build build --recipe recipe.yml --output-dir ../r-sffdr --target-platform $target_platform
    
    read -p "Upload to Anaconda? [y/N]: " upload_choice
    if [[ $upload_choice =~ ^[Yy]$ ]]; then
        echo "Uploading to Anaconda..."
        rattler-build upload anaconda $(ls ../r-sffdr/$target_platform/r-sffdr-$version-*.conda) --owner twillis209
        echo "Package uploaded successfully!"
    fi
fi

echo "Release process complete!"
