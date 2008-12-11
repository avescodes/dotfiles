#!/bin/sh

cd ~/.emacs.d/installable || exit
for FILE in .??*; do
  echo "Installing $FILE..."
  mv ~/$FILE ~/$FILE.old >/dev/null 2>&1
  cp ~/.emacs.d/installable/$FILE ~/$FILE
done

echo "Done!"