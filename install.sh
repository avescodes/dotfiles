#!/bin/sh

cd ~/.emacs.d/installable || exit
for FILE in *; do
  echo "Installing .$FILE..."
  mv ~/.$FILE ~/.$FILE.old >/dev/null 2>&1
  cp ~/.emacs.d/installable/$FILE ~/.$FILE
done

cd $HOME/.clojure || mkdir $HOME/.clojure 
echo "Installing jline.jar to ~/.clojure/"
cp $CLJ_DIR/jline.jar $HOME/.clojure/jline.jar
echo "Done!"