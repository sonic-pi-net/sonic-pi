require 'fileutils'

OS = case RUBY_PLATFORM
when /.*arm.*-linux.*/
  :linux_arm
when /.*linux.*/
  :linux
when /.*darwin.*/
  :macos
when /.*mingw.*|.*mswin.*|.*bccwin.*|.*wince.*|.*emx.*/
  :windows
else
  RUBY_PLATFORM
end

OS = ask_if_raspbian if (OS == :linux_arm)

RUBY_API = RbConfig::CONFIG['ruby_version']

SONIC_PI_ROOT = File.expand_path("#{__dir__}/..")
SPI_QT_GUI_PATH = File.expand_path("#{__dir__}/../app/gui/qt")
SPI_SERVER_PATH = File.expand_path("#{__dir__}/../app/server")

# Bundler
def server_bundle_exec(cmd)
  exec_sh_commands([
    %Q(cd #{SPI_SERVER_PATH}/ruby),
    %Q(bundle exec #{cmd})
  ])
end

# Make
def make_clean(dir)
  info("### Running make clean in #{dir}")
  if File.exist?("#{dir}/Makefile") then
    exec_sh("cd #{dir} && make clean")
    FileUtils.rm_rf "#{dir}/Makefile"
  end
end

# OS detection
# TODO: See if this is actually needed - are there any differences which need to be accounted for in building for Raspbian on a RPi than Debian on a PC?
def ask_if_raspbian
  i = :invalid
  # Ask the user whether they are using Raspbian on a Raspberry Pi
  while (i == :invalid) do
    puts("Are you using Raspbian on a Raspberry Pi or other ARM device? (yes/no)")
    i = case STDIN.gets.chomp
    when "y" || "yes"
      :raspberry
    when "n" || "no"
      :linux
    else
      :invalid
    end
  end
  return i
end

# Logging & user input
def info(text)
  text_cyan = '\033[1;36m'
  text_nc = '\033[0m'
  #exec_sh(%Q(echo -e \\"#{text_cyan}#{text}#{text_nc}\\"))
  puts(`tput bold` + `tput setaf 45` + text + `tput sgr0`) # for debugging
end

def ask_yes_no(question, default)
  i = :invalid
  answers = ""
  d = :invalid
  #puts(default)

  if (default == "y" || default == "yes")
    #puts("yes!")
    answers = "(Y/n)"
    d = true
  elsif (default == "n" || default == "no")
    #puts("no!")
    answers = "(y/N)"
    d = false
  else
    answers = "(y/n)"
  end

  while (i == :invalid) do
    puts("#{question} #{answers}")
    i = case STDIN.gets.chomp
    when "y", "yes"
      true
    when "n", "no"
      false
    else
      d
    end
  end
  return i
end

# System command execution (sh, bash, cmd)
def exec_sh_commands(commands)
  exec_sh(commands.join(" && "))
end

def exec_bash_commands(commands)
  exec_bash(commands.join(" && "))
end

def exec_win_commands(commands)
  exec_win_cmd(commands.join(" && "))
end

def exec_sh(command)
  puts(`tput setaf 11` + "Executing sh command: " + command + `tput sgr0`) # for debugging
  result = `sh -c "#{command}"`
  puts(result.to_s)
  return result
end

def exec_bash(command)
  puts(`tput setaf 11` + "Executing bash command: " + command + `tput sgr0`) # for debugging
  result = `bash -licv "#{command}"`
  puts(result.to_s)
  return result
end

# UNTESTED!
# TODO: See if Ruby uses sh, bash, or cmd.exe for the system shell on Windows
def exec_win_cmd(command)
  puts("Executing cmd command: " + command) # for debugging
  result = `cmd.exe /c #{command}`
  puts(result.to_s)
  return result
end

# File system manipulation
def replace_dir(dir1, dir2)
  #puts(File.expand_path(dir1))
  #puts(File.expand_path(dir2))
  FileUtils.rm_rf(dir2)
  create_dir(dir2)
  FileUtils.copy_entry(dir1, dir2, remove_destination=true)
end

def create_dir(path)
  FileUtils.mkdir_p(path) if (File.directory?(path) == false)
end

def make_tree(path)
  if (File.directory?(path) == false)
    make_tree(File.dirname(path))
    FileUtils.mkdir(path)
  end
end

def install_files(files, src_folder, dest_folder, verbose=false)
  files.each { |f|
		if (File.file?(f))
			src = f.dup()
			dest = f.dup()
			#puts(f)

			# Replace the Sonic Pi build dir with the install prefix
			dest[0..src_folder.length()] = dest_folder + "/"

			#puts(f)
			#puts(dest)

      # Create the directory for the dest file if it doesn't exist
			if (!File.directory?(File.dirname(dest)))
				FileUtils.mkdir_p(File.dirname(dest))
			end

      if (verbose)
			     puts("Copying #{f} to #{dest}...")
      end

			begin
				FileUtils.cp(f, File.dirname(dest), :preserve => true)
			rescue Exception => e
				# Attempt manual copy
				puts("FileUtils.cp(#{f}, #{File.dirname(dest)}, :preserve => true) failed: #{e.message} Attempting manual copy...")
				src_file = File.open(f, "r")
				contents = src_file.read()
				src_file.close()

				dest_file = File.open(dest, "w")
				dest_file.write(contents)
				dest_file.close()
			end
		end
	}
end
