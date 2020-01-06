require_relative "./utils"

# Package management
def ask_pkg_manager
  m = :invalid
  # Ask which package manager to use
  while (m == :invalid) do
    puts("Which package manager should we use to install dependency packages?")
    puts("1: apt (for Debian, Ubuntu and other Debian based distros)")
    puts("2: pacman (for Arch Linux and some other distros)")

    m = case STDIN.gets.chomp
    when "1"
      :apt
    when "2"
    	:pacman
    else
      :invalid
    end
  end
  return m
end

def ask_checkinstall
  # Ask if the user wants to use checkinstall
  c = :invalid
  while (c == :invalid) do
    puts("Do you want to use checkinstall to install programs made from source? (Available on Debian, Ubuntu and some other distros)")
    puts("This makes it easier to uninstall these programs.")
    puts("(yes/no)")

    c = case STDIN.gets.chomp
    when "y" || "yes"
      :yes
    when "n" || "no"
      :no
    else
      :invalid
    end
  end
  return c
end

def ask_distro
  d = :none
  while (d == :none) do
    puts("What package type do you want to install, and how do you want to install it?")
    puts("1 - for Debian, Ubuntu and other Debian based distros - .deb packages installed via. dpkg")
    puts("2 - for RedHat and other distros - .rpm package")

    d = case STDIN.gets.chomp
    when "1"
      :debian
    when "2"
      :rpm
    else
      :none
    end
  end
  return d
end

def install_packages(packages, _pkg_manager)
  puts packages
  package_names = []
  length = packages.length - 1
  for i in 0..length
    package_names.push(packages[i].split(" ")[0])
  end

  case _pkg_manager
  when :apt
    cmd = exec_sh(%Q(sudo apt-get install -y #{package_names.join(" ")}))
  when :pacman
  	cmd = exec_sh(%Q(pacman -S #{package_names.join(" ")}))
  else
  end
end

def check_ver(pkg_spec_ver, _pkg_manager)
  # Get the version available and the version specified in the dependencies
  ver = get_ver(pkg_spec_ver.split(" ")[0], _pkg_manager)
  op = pkg_spec_ver.split(" ")[1]
  spec_ver = pkg_spec_ver.split(" ")[2]

  if (spec_ver != nil)
    return compare_versions(ver, op, spec_ver)
  else
    # No version is specified in the dependencies
    return true
  end
end

def compare_versions(ver, op, spec_ver)
  version1 = ver.split(".")
  version2 = spec_ver.split(".")

  # Compare the versions
  for i in 0..2
    version1.push("0") if (version1[i] == nil)
    version2.push("0") if (version2[i] == nil)
    case op
    when ">="
      if ((version1[i].to_i >= version2[i].to_i) == false)
        # The version is less than the version specified in the dependencies!
        return false
      end

    when "=" || "=="
      if ((version1[i].to_i == version2[i].to_i) == false)
        # The version is isn't the version specified in the dependencies!
        return false
      end

    when "<="
      if ((version1[i].to_i <= version2[i].to_i) == false)
        # The version is more than the version specified in the dependencies!
        return false
      end

    when ">"
      if ((version1[i].to_i > version2[i].to_i) == false)
        # The version is less than the version specified in the dependencies!
        return false
      end

		when "<"
      if ((version1[i].to_i < version2[i].to_i) == false)
        # The version is more than the version specified in the dependencies!
        return false
      end

    else
      return false
    end
  end
  return true
end

def get_ver(package, _pkg_manager)
  case _pkg_manager
  when :apt
    ver = exec_sh(%Q(apt-cache show #{package}| grep '^Version:'))
    ver = ver.split("\n")[0] # Get the first line of the output
    ver = ver.to_s

    ver.slice!("Version: ") # Remove the 'Version: ' bit
    ver = ver.split("+")[0]
    ver = ver.split("-")[0]
    ver = ver.split("~")[0]
    ver = ver.split(":")[1]

    puts(ver)
    return ver
  when :pacman
  	var = exec_sh(%Q(pacman -Si #{package}| grep '^Version         :'))
  	ver = ver.split("\n")[0] # Get the first line of the output
    ver = ver.to_s

    ver.slice!("Version         : ") # Remove the 'Version         : ' bit
    ver = ver.split("+")[0]
    ver = ver.split("-")[0]
    ver = ver.split("~")[0]
    ver = ver.split(":")[1]

    puts(ver)
    return ver
  else
  end
end
