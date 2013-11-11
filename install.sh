#!/bin/bash

# get the absolute path to this script
pushd `dirname $0` > /dev/null
MYDIR=`pwd`
popd > /dev/null

IGNORE="install.sh LaunchAgents"

# add symlinks to add the dotfiles in my home directory
for file in `ls -d ${MYDIR}/* | grep -v ${IGNORE}`; do

  # hard link shell config files and soft link everything else
  if [ ${file} == "${MYDIR}/zshrc" ]; then
    ln ${file} ${HOME}/.`basename ${file}`
  else
    ln -F -s ${file} ${HOME}/.`basename ${file}`
  fi
done

# copy launchagents we need
for file in `ls -d ${MYDIR}/LaunchAgents/*`; do
  cp $file ~/Library/LaunchAgents/
done
