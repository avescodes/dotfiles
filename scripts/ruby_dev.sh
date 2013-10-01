#!/bin/bash
set -xe


################################################
#Setup rbenv, compile ruby, and install bundler.

sudo apt-get update
sudo apt-get install -yy --no-install-recommends build-essential libssl-dev

git clone https://github.com/sstephenson/rbenv ~/.rbenv
git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
source ~/.bashrc

rbenv install 1.9.3-p392
source ~/.bashrc

gem install --no-rdoc --no-ri bundler
rbenv rehash
