#!/bin/bash
set -e

# Release automation script for sffdr R package
# Automates version bumping, documentation, building, checking, and release creation

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: Not in a git repository"
    exit 1
fi

# Check for uncommitted changes
if ! git diff-index --quiet HEAD --; then
    echo "Warning: You have uncommitted changes."
    read -p "Continue anyway? [y/N]: " continue_choice
    if [[ ! $continue_choice =~ ^[Yy]$ ]]; then
        echo "Aborting."
        exit 1
    fi
fi

# Activate conda environment
echo "Activating sffdr-dev conda environment..."
source $(conda info --base)/etc/profile.d/conda.sh
conda activate sffdr-dev

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

echo "Building package..."
R CMD build .

latest=$(ls -t sffdr_*.tar.gz | head -n 1)
echo "Latest build: $latest"

echo "Running R CMD check..."
R CMD check $latest --no-manual --no-build-vignettes

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

read -p "Push to remote and create release? [y/N]: " push_choice
if [[ $push_choice =~ ^[Yy]$ ]]; then
    current_branch=$(git branch --show-current)
    echo "Pushing branch $current_branch..."
    git push -f origin "$current_branch"
    
    echo "Pushing tag v$version..."
    git push -f origin "v$version"
    
    echo "Creating GitHub release..."
    gh release create "v$version" $latest --notes-from-tag
    
    read -p "Build and upload to Anaconda? [y/N]: " build_choice
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
else
    echo "Skipping push and release creation."
    echo "You can manually push later with:"
    echo "  git push origin $(git branch --show-current)"
    echo "  git push -f origin v$version"
    echo "  gh release create v$version $latest --notes-from-tag"
fi

echo "Release process complete!"
