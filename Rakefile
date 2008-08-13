task :default => [ :installfiles ]

BASEDIR = File.dirname(__FILE__)

FILES = [ '.zshrc', '.emacs', '.screenrc' ]

task :installfiles do
  FILES.each do |file|
    puts "Installing #{file} ..."
    `mv ~/#{file} ~/#{file}.old >/dev/null 2>&1`
    `cp #{BASEDIR}/#{file} ~/#{file}`
  end
  puts "Done!"
end
