begin
  require 'rubygems'
rescue LoadError
end
require 'fileutils'

PATH = File.dirname __FILE__

task :default => [:install]

desc "Install dotfiles to user home directory (~/)"
task :install do
  puts "Installing dotfiles..."
  dotfiles = Dir.entries("#{PATH}/dotfiles")-['.','..']

  puts "Backing up current files to ~/<FILE>.old."
  dotfiles.each do |dotfile|
    sys_loc = File.join(File.expand_path("~"), dotfile)
    FileUtils.copy sys_loc, "#{sys_loc}.old" rescue nil
    install "dotfiles/#{dotfile}", sys_loc
  end
end
