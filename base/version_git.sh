#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

# This file collects git info and create a julia file with the GIT_VERSION_INFO struct

printf "# This file was autogenerated in base/version_git.sh\n"
printf "immutable GitVersionInfo\n"
printf "    commit::AbstractString\n"
printf "    commit_short::AbstractString\n"
printf "    branch::AbstractString\n"
printf "    build_number::Int\n"
printf "    date_string::AbstractString\n"
printf "    tagged_commit::Bool\n"
printf "    fork_master_distance::Int\n"
printf "    fork_master_timestamp::Float64\n"
printf "end\n"
printf "\n"

cd $1

# If the script didn't ask not to use git info
if [  "$#" = "2"  -a "$2" = "NO_GIT" ]; then
    # this comment is used in base/Makefile to distinguish boilerplate
    printf "# Default output if git is not available.\n"
    printf "const GIT_VERSION_INFO = GitVersionInfo(\"\" ,\"\" ,\"\" ,0 ,\"\" ,true ,0 ,0.)\n"
    exit 0
fi
# Collect temporary variables
origin=$(git config -l 2>/dev/null | grep 'remote\.\w*\.url.*JuliaLang/julia' | sed -n 's/remote\.\([a-zA-Z]*\)\..*/\1\//p')
if [ -z "$origin" ]; then
    origin="origin/"
fi
git_time=$(git log -1 --pretty=format:%ct)

#collect the contents
commit=$(git rev-parse HEAD)
commit_short=$(git rev-parse --short HEAD)
if [ -n "$(git status --porcelain)" ]; then
    # append dirty mark '*' if the repository has uncommited changes
    commit_short="$commit_short"*
fi
branch=$(git branch | sed -n '/\* /s///p')

topdir=$(git rev-parse --show-toplevel)
verchanged=$(git blame -L ,1 -sl -- "$topdir/VERSION" | cut -f 1 -d " ")
if [ $verchanged = 0000000000000000000000000000000000000000 ]; then
    # uncommited change to VERSION
    build_number=0
else
    build_number=$(git rev-list --count HEAD "^$verchanged")
fi

date_string=$git_time
case $(uname) in
  Darwin | FreeBSD)
    date_string="$(/bin/date -jr $git_time -u '+%Y-%m-%d %H:%M %Z')"
    ;;
  MINGW*)
    git_time=$(git log -1 --pretty=format:%ci)
    date_string="$(/bin/date --date="$git_time" -u '+%Y-%m-%d %H:%M %Z')"
    ;;
  *)
    date_string="$(/bin/date --date="@$git_time" -u '+%Y-%m-%d %H:%M %Z')"
    ;;
esac
if [ $(git describe --tags --exact-match 2> /dev/null) ]; then
    tagged_commit="true"
else
    tagged_commit="false"
fi
fork_master_distance=$(git rev-list HEAD ^"$(printf "$origin\n")master" | wc -l | sed -e 's/[^[:digit:]]//g')
fork_master_timestamp=$(git show -s $(git merge-base HEAD $(printf "$origin\n")master) --format=format:"%ct")

# Check for errrors and emit default value for missing numbers.
if [ -z "$build_number" ]; then
    build_number="-1"
fi
if [ -z "$fork_master_distance" ]; then
    fork_master_distance="-1"
fi
if [ -z "$fork_master_timestamp" ]; then
    fork_master_timestamp="0"
fi

printf "const GIT_VERSION_INFO = GitVersionInfo(\n"
printf "    \"$commit\",\n"
printf "    \"$commit_short\",\n"
printf "    \"$branch\",\n"
printf "    $build_number,\n"
printf "    \"$date_string\",\n"
printf "    $tagged_commit,\n"
printf "    $fork_master_distance,\n"
printf "    $fork_master_timestamp.\n"
printf ")\n"
