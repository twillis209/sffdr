
#!/bin/bash
set -e

# Release automation script for sffdr R package
# Automates version bumping, documentation, building, checking, and release creation
#
# Usage:
#   ./release.sh                          # Run full workflow interactively
#   ./release.sh --build                  # Skip to building R package
#   ./release.sh --check                  # Skip to checking R package
#   ./release.sh --conda                  # Skip to building conda package
#   ./release.sh --push                   # Skip to push/release step
#
# Non-interactive options:
#   --version-bump=TYPE                   # patch, minor, major, or none
#   --no-check                            # Skip R CMD check
#   --no-push                             # Skip git push/release
#   --build-conda                         # Build conda package
#   --target-platform=PLATFORM            # linux-64 or osx-arm64
#   --upload-conda                        # Upload to Anaconda
#   --local-channel=PATH                  # Copy to local conda channel and index
#   --non-interactive                     # Run without prompts (requires other flags)

# Parse command-line arguments
SKIP_TO=""
VERSION_BUMP=""
DO_CHECK="yes"
DO_PUSH=""
BUILD_CONDA=""
TARGET_PLATFORM=""
UPLOAD_CONDA="no"
LOCAL_CHANNEL=""
NON_INTERACTIVE="no"

while [[ $# -gt 0 ]]; do
    case $1 in
        --build)
            SKIP_TO="build"
            shift
            ;;
        --check)
            SKIP_TO="check"
            shift
            ;;
        --conda)
            SKIP_TO="conda"
            shift
            ;;
        --push)
            SKIP_TO="push"
            shift
            ;;
        --version-bump=*)
            VERSION_BUMP="${1#*=}"
            shift
            ;;
        --no-check)
            DO_CHECK="no"
            shift
            ;;
        --no-push)
            DO_PUSH="no"
            shift
            ;;
        --build-conda)
            BUILD_CONDA="yes"
            shift
            ;;
        --target-platform=*)
            TARGET_PLATFORM="${1#*=}"
            shift
            ;;
        --upload-conda)
            UPLOAD_CONDA="yes"
            shift
            ;;
        --local-channel=*)
            LOCAL_CHANNEL="${1#*=}"
            shift
            ;;
        --non-interactive)
            NON_INTERACTIVE="yes"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Skip-to options:"
            echo "  --build              Skip to building R package"
            echo "  --check              Skip to checking R package"
            echo "  --conda              Skip to building conda package"
            echo "  --push               Skip to push/release step"
            echo ""
            echo "Non-interactive options:"
            echo "  --version-bump=TYPE  Version bump type: patch, minor, major, or none"
            echo "  --no-check           Skip R CMD check step"
            echo "  --no-push            Skip git push and GitHub release"
            echo "  --build-conda        Build conda package"
            echo "  --target-platform=P  Target platform: linux-64 or osx-arm64"
            echo "  --upload-conda       Upload conda package to Anaconda"
            echo "  --local-channel=PATH Copy conda package to local channel and run conda index"
            echo "  --non-interactive    Run without prompts (requires specifying all options)"
            echo ""
            echo "Example non-interactive usage:"
            echo "  $0 --non-interactive --version-bump=patch --build-conda --target-platform=linux-64 --local-channel=/path/to/channel"
            echo ""
            echo "  --help               Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: Not in a git repository"
    exit 1
fi

# Check for uncommitted changes (skip if jumping to later steps)
if [[ -z "$SKIP_TO" ]] || [[ "$SKIP_TO" == "build" ]]; then
    if ! git diff-index --quiet HEAD --; then
        echo "Warning: You have uncommitted changes."
        if [[ "$NON_INTERACTIVE" == "yes" ]]; then
            echo "Continuing anyway (non-interactive mode)..."
        else
            read -p "Continue anyway? [y/N]: " continue_choice
            if [[ ! $continue_choice =~ ^[Yy]$ ]]; then
                echo "Aborting."
                exit 1
            fi
        fi
    fi
fi

# Activate conda environment
echo "Activating sffdr-dev conda environment..."
source $(conda info --base)/etc/profile.d/conda.sh
conda activate sffdr-dev

# Version bump and documentation (skip if requested)
if [[ -z "$SKIP_TO" ]]; then
    if [[ -n "$VERSION_BUMP" ]]; then
        # Non-interactive mode with specified version bump
        case $VERSION_BUMP in
            patch|minor|major)
                echo "Bumping $VERSION_BUMP version..."
                R -e "usethis::use_version('$VERSION_BUMP')"
                ;;
            none)
                echo "Skipping version bump..."
                ;;
            *)
                echo "Invalid version bump type: $VERSION_BUMP"
                exit 1
                ;;
        esac
    else
        # Interactive mode
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

# Check R package (skip if jumping to conda/push or if --no-check specified)
if [[ "$DO_CHECK" == "yes" ]] && [[ -z "$SKIP_TO" || "$SKIP_TO" == "build" || "$SKIP_TO" == "check" ]]; then
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

# Push and release (skip if jumping to conda only or if --no-push specified)
if [[ "$DO_PUSH" != "no" ]] && [[ -z "$SKIP_TO" || "$SKIP_TO" == "build" || "$SKIP_TO" == "check" || "$SKIP_TO" == "push" ]]; then
    if [[ "$NON_INTERACTIVE" == "yes" && -z "$DO_PUSH" ]]; then
        echo "Skipping push and release (non-interactive mode, not explicitly requested)."
    elif [[ "$DO_PUSH" == "yes" ]] || [[ "$NON_INTERACTIVE" != "yes" ]]; then
        if [[ "$NON_INTERACTIVE" == "yes" ]]; then
            push_choice="y"
        else
            read -p "Push to remote and create release? [y/N]: " push_choice
        fi
        
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
fi

# Build conda package
if [[ "$BUILD_CONDA" == "yes" ]] || [[ "$NON_INTERACTIVE" != "yes" && -z "$SKIP_TO" || "$SKIP_TO" == "conda" ]]; then
    if [[ "$NON_INTERACTIVE" == "yes" ]]; then
        build_choice="y"
    else
        read -p "Build conda package with rattler-build? [y/N]: " build_choice
    fi
    
    if [[ $build_choice =~ ^[Yy]$ ]]; then
        if [[ -n "$TARGET_PLATFORM" ]]; then
            target_platform="$TARGET_PLATFORM"
        else
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
        fi
        
        echo "Building with rattler-build for $target_platform (allowing noarch packages)..."
        rattler-build build --recipe recipe.yml --output-dir ../r-sffdr --target-platform $target_platform --with-solve-target-platform
        
        # Find the built package
        conda_package=$(ls ../r-sffdr/$target_platform/r-sffdr-$version-*.conda 2>/dev/null | head -n 1)
        
        if [[ -z "$conda_package" ]]; then
            echo "Warning: Could not find built conda package"
        else
            echo "Built package: $conda_package"
            
            # Copy to local channel if specified
            if [[ -n "$LOCAL_CHANNEL" ]]; then
                echo "Copying package to local conda channel: $LOCAL_CHANNEL"
                mkdir -p "$LOCAL_CHANNEL/$target_platform"
                cp "$conda_package" "$LOCAL_CHANNEL/$target_platform/"
                
                echo "Running conda index on $LOCAL_CHANNEL..."
                conda index "$LOCAL_CHANNEL"
                echo "✓ Local conda channel updated"
            elif [[ "$NON_INTERACTIVE" != "yes" ]]; then
                read -p "Copy to local conda channel? [y/N]: " local_choice
                if [[ $local_choice =~ ^[Yy]$ ]]; then
                    read -p "Enter local channel path: " local_channel_path
                    if [[ -n "$local_channel_path" ]]; then
                        echo "Copying package to $local_channel_path..."
                        mkdir -p "$local_channel_path/$target_platform"
                        cp "$conda_package" "$local_channel_path/$target_platform/"
                        
                        echo "Running conda index on $local_channel_path..."
                        conda index "$local_channel_path"
                        echo "✓ Local conda channel updated"
                    fi
                fi
            fi
        fi
        
        # Upload to Anaconda
        if [[ "$UPLOAD_CONDA" == "yes" ]]; then
            upload_choice="y"
        elif [[ "$NON_INTERACTIVE" != "yes" ]]; then
            read -p "Upload to Anaconda? [y/N]: " upload_choice
        else
            upload_choice="n"
        fi
        
        if [[ $upload_choice =~ ^[Yy]$ ]]; then
            if [[ -n "$conda_package" ]]; then
                echo "Uploading to Anaconda..."
                rattler-build upload anaconda "$conda_package" --owner twillis209
                echo "Package uploaded successfully!"
            else
                echo "Error: Cannot upload - package not found"
            fi
        fi
    fi
    fi
fi

echo "Release process complete!"
