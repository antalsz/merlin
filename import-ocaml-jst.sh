#!/bin/bash

ocaml_jst=https://github.com/ocaml-flambda/ocaml-jst
default_branch=main

function usage () {
  cat <<USAGE
Usage: $0
       $0 COMMITISH
       $0 REPO COMMITISH
       $0 REPO COMMITISH SUBDIRECTORY

Fetch the new compiler sources and patch Merlin to keep Merlin's local copies of
things in sync.  By default, will pull the "main" branch from
<$repository>, but the branch can be overridden
by any commitish (branch, tag, commit, etc.) and the repository can be
overridden by any URL.  If SUBDIRECTORY is specified, then the relevant files are searched
for in the given subdirectory of the repository.
USAGE
}

repository="$ocaml_jst"
commitish="$default_branch"
subdirectory="."
case $# in
  0)
    # Accept defaults
    ;;
  1)
    case "$1" in
      -h|--help|-\?)
        usage
        exit 0
        ;;
      *)
        commitish="$1"
        ;;
    esac
    ;;
  2|3)
    repository="$1"
    commitish="$2"
    subdirectory="${3:-$subdirectory}"
    ;;
  *)
    usage >&2
    exit 1
    ;;
esac

if ! git diff --quiet; then
    echo "Working directory must be clean before using this script,"
    echo "but currently has the following changes:"
    git diff --stat
    exit 1
fi

# First, fetch the new ocaml-jst sources and copy into upstream/ocaml_jst
git fetch "$repository" "$commitish"
rev=$(git rev-parse FETCH_HEAD)
cd upstream/ocaml_jst
echo $rev > base-rev.txt
for file in $(git ls-tree --name-only -r HEAD | grep -v base-rev.txt); do
  git show "FETCH_HEAD:$subdirectory/$file" > "$file";
done
git add -u .
cd ../..
git commit -m "Import ocaml-jst $(git describe --always $rev)"

# Then patch src/ocaml using the changes you just imported
for file in $(git diff --name-only HEAD^ HEAD); do
   base=${file#upstream/ocaml_jst/}
   case $base in
       parsing/lexer.mll) tgt=preprocess/lexer_raw.mll;;
       parsing/parser.mly) tgt=preprocess/parser_raw.mly;;
       utils/clflags.ml*) echo "Ignoring changes to $base"; continue;;
       *) tgt=$base;;
   esac
   tgt=src/ocaml/$tgt

   # Not all files are necessary
   if [ ! -e $tgt ]; then continue; fi

   err=$(patch --merge $tgt <(git diff HEAD^ HEAD -- $file))
   # ignore patch output if it worked
   if [ $? = 0 ]; then
      git add -u $tgt
   else
      echo "$err"
   fi
   rm -f $tgt.orig
done

