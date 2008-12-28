#!/usr/bin/ruby

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"
require 'rubygems'

if (RUBY_VERSION == "1.8.7")
  #load libraries
  require 'irb/ext/save-history'

  require 'wirble'
  require 'utility_belt'

  #start wirble (with color)
  Wirble.init
  Wirble.colorize

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
    def local_methods
      (methods - Object.instance_methods).sort
    end
  end
end
