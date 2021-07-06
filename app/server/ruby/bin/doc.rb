
$task = nil

OptionParser.new do |opts|
  opts.banner = "Usage: doc.rb [options]
  Translates the Sonic Pi tutorial."
  opts.on('-x', '--extract', 'extract the reference documentation into JSON files') do
    $task = :extract
  end
  opts.on('-t', '--translate', "") do
    $task = :translate
  end
  opts.on('-u', '--update', 'update translation files with current English tutorial (similar to msgmerge)') do
    $task = :update
  end

  #opts.on('--add-lang', 'add a .po file for the specified language if it doesn\'t exist') do
  #  $task = :add_lang
  #end
end.parse!

case $task
when :extract

end
