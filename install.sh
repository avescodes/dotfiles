#!/bin/sh

for FILE in .zshrc .emacs .screenrc; do
  echo "Installing $FILE..."
  mv ~/$FILE ~/$FILE.old >/dev/null 2>&1
  cp ~/.emacs.d/$FILE ~/$FILE
done

echo "Done!"