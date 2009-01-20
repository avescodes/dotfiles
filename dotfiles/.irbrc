#!/usr/bin/ruby

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"

require 'rubygems'
require 'fileutils'
require 'irb/ext/save-history'
require 'wirble'

Wirble.init
Wirble.colorize

colors = Wirble::Colorize.colors.merge(
                                       {
                                         :comma => :light_grey,
                                         :refers => :light_grey,
                                         :string => :light_green,
                                         :number => :red,
                                         :symbol => :blue,
                                         :symbol_prefix => :light_blue,

                                         :open_hash          => :purple,
                                         :close_hash         => :purple,
                                         :open_array         => :purple,
                                         :close_array        => :purple,
                                       })
Wirble::Colorize.colors = colors

alias e exit
alias q exit

def ls(arg='*')
  Dir[arg]
end

def cd(arg='.')
  FileUtils.cd(arg, :verbose => true)
end

class Object
  # print documentation
  #
  # ri 'Array#pop'
  # Array.ri
  # Array.ri :pop
  # arr.ri :pop
  def ri(method = nil)
    unless method && method =~ /^[A-Z]/ # if class isn't specified
      klass = self.kind_of?(Class) ? name : self.class.name
      method = [klass, method].compact.join('#')
    end
    puts `ri '#{method}'`

  end

  def m
    (methods - Object.instance_methods).sort
  end

end

if (RUBY_VERSION == "1.8.7")
  require 'utility_belt' # Not 1.9.1 compatible
end
