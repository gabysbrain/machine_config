#!/bin/bash

MYDIR=`dirname $0`

IGNORE="install.sh"

# add symlinks to add the dotfiles in my home directory
for file in `ls -d ${MYDIR}/* | grep -v ${IGNORE}`; do

  # hard link shell config files and soft link everything else
  if [ ${file} == "${MYDIR}/tcshrc" ]; then
    echo ln ${file} ${HOME}/.`basename ${file}`
  else
    echo ln -s ${file} ${HOME}/.`basename ${file}`
  fi
done

