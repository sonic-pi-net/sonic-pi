#!/bin/bash
doc_sha=$(git ls-tree -d HEAD build | awk '{print $3}')
git cat-file -p $doc_sha
new_commit=$(echo "Auto-update docs." | git commit-tree $doc_sha -p gh-pages)
git update-ref refs/heads/gh-pages $new_commit
