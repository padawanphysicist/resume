#!/bin/bash

# Recreate folder
rm -rf tags/
mkdir -p tags

declare -A tag_counter

# This first run is just to populate associative array of tags. I just could not
# find yet a better way of doing this
for f in `find posts/ -iname '*.org'`; do
    tags=$(cat "$f" | gawk '/^\#\+KEYWORDS:/{print $2}' | tr "," " ")
    for tag in ${tags}; do
        tag_counter[${tag}]=0
    done
done

# Loop over all tags and create their respective files
for tag in "${!tag_counter[@]}"; do
    f=tags/${tag}.org
    touch ${f}
    echo "#+TITLE: Posts tagged ${tag}" >> ${f}
    echo "" >> ${f}
done

# Now we do the actual counting
for f in `find posts/ -iname '*.org'`; do
    title=$(cat "$f" | gawk '/^\#\+TITLE:/{print $2}')
    tags=$(cat "$f" | gawk '/^\#\+KEYWORDS:/{print $2}' | tr "," " ")
    for tag in ${tags}; do
        printf "+ [[../%s][%s]]\n" "${f}" "${title}" >> tags/${tag}.org
    done
done

# Now we do the actual counting
#for p in `find posts/ -iname '*.org'`; do
#    tags=$(cat "$p" | gawk '/^\#\+KEYWORDS:/{print $2}' | tr "," " ")
#    for tag in ${tags}; do
#        tag_counter[${tag}]=$(expr ${tag_counter[${tag}]} + 1)
#    done
#done
