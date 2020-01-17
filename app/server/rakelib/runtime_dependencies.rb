require_relative '../../../rakelib/utils'
require_relative '../../../rakelib/package_management'

# Application/library versions built by this script.
MIN_SUPERCOLLIDER_VERSION = "3.9"
SUPERCOLLIDER_VERSION = "3.9.1"

MIN_SC_PLUGINS_VERSION = "3.9.0"
SC_PLUGINS_VERSION = "3.9.0" # 3.9.1 is currently in pre-release and I've had issues installing it, but 3.9.0 seems to work fine.

AUBIO_VERSION = "c6ae035" # v0.4.6
OSMID_VERSION = "391f35f789f18126003d2edf32902eb714726802"

namespace "server" do
	desc "Bundle the other Sonic Pi runtime dependencies with Sonic Pi"
	task :bundle_runtime_deps, [:make_jobs] do |t, args|
		install_prefix=""
		case OS
		when :linux, :raspberry
		 install_prefix = File.join(Dir.pwd, "native", "linux")
		when :windows
		 install_prefix = File.join(Dir.pwd, "native", "windows")
		when :macos
		 install_prefix = File.join(Dir.pwd, "native", "macos")
		end
		Rake::Task[:install_runtime_deps].invoke(args.make_jobs, install_prefix)
	end

	desc "Build the other Sonic Pi runtime dependencies"
	task :build_runtime_deps, [:make_jobs, :install_prefix, :checkinstall] => [
	"build_supercollider",
	"build_aubio",
	] do |t, args|
		args.with_defaults(:make_jobs => 1)
		args.with_defaults(:install_prefix => "")
		args.with_defaults(:checkinstall => "no")
	end

	desc "Install the other Sonic Pi runtime dependencies to the specified location"
	task :install_runtime_deps, [:make_jobs, :install_prefix, :checkinstall] => [
	:build_supercollider,
	:install_supercollider,
	:build_aubio,
	:install_aubio
	] do |t, args|
		args.with_defaults(:make_jobs => 1)
	end

	namespace "runtime_deps" do

		task :create_build_dir do
			info("Creating build dir for building Sonic Pi server dependencies")
		 	FileUtils.mkdir_p(File.join(SPI_SERVER_PATH, "build_dependencies"))
		end


		# External runtime dependencies: supercollider & sc3_plugins, aubio
		task :supercollider, [:make_jobs] do |t, args|
			args.with_defaults(:make_jobs => 1)

		  #OS = ask_if_raspbian if (OS == :linux_arm)
		  info("Checking the version of supercollider installed...")
		  case OS
		  when :linux, :raspberry
		    if (check_ver("supercollider >= #{MIN_SUPERCOLLIDER_VERSION}", SPI_BUILD_CONFIG.pkg_manager) == false)
		      build_supercollider
		    elsif (check_ver("sc3-plugins >= #{MIN_SC_PLUGINS_VERSION}", SPI_BUILD_CONFIG.pkg_manager) == false)
		      build_supercollider
		    else
		      check = exec_sh(%Q(dpkg -S `which scsynth`))
		      if (check.include?("supercollider") == false)
		        # Supercollider isn't installed
		        info("The version of supercollider avaiable is good, installing supercollider...")
		        install_packages(["supercollider"], SPI_BUILD_CONFIG.pkg_manager)
		      else
		        # Supercollider is installed
		        info("The version of supercollider installed is good! :)")
		      end
		    end
		  when :windows
      build_supercollider
		  when :macos
      build_supercollider
		  end
		end

		desc "Build Supercollider from source"
		task :build_supercollider, [:make_jobs, :install_prefix] do |t, args|
			args.with_defaults(:make_jobs => 1)
			args.with_defaults(:install_prefix => "")

		  OS = ask_if_raspbian if (OS == :linux_arm)

		  case OS
		  when :raspberry
		    install_packages(Dependencies::Raspberry.supercollider, SPI_BUILD_CONFIG.pkg_manager)
		  when :linux
		    install_packages(Dependencies::Linux.supercollider, SPI_BUILD_CONFIG.pkg_manager)
		  when :macos
		    #TODO - Get packages from homebrew or macports?
		  when :windows
		    #TODO
		  end

	    info("Building supercollider from source...")

	    # Build SuperCollider
	    exec_sh_commands([
	      %Q(cd #{SPI_SERVER_PATH}/build_dependencies),
	      %Q(git clone --recursive https://github.com/supercollider/supercollider.git || true),
	      %Q(cd supercollider),
	      %Q(git checkout Version-#{SUPERCOLLIDER_VERSION}),
	      %Q(git submodule init && git submodule update),
	      %Q(git submodule update --init)
	    ])

	    FileUtils.mkdir_p(File.join("build_dependencies", "supercollider", "build"))
	    case OS
	    when :linux, :raspberry
	      exec_sh_commands([
	        %Q(cd #{SPI_SERVER_PATH}/build_dependencies/supercollider/build),
	        %Q(cmake -DSC_EL=no -DCMAKE_INSTALL_PREFIX=#{args.install_prefix} ..),
	        %Q(make -j#{args.make_jobs}),
	        %Q(cd ../..) # build folder
	      ])
	    when :windows
	      # UNTESTED
	      exec_win_commands([
          %Q(cd #{SPI_SERVER_PATH}/build_dependencies/supercollider/build),
          %Q(cmake -G "Visual Studio 12 2013" ..),
          %Q(cmake --build . --config Release)
        ])
	    when :macos
        # UNTESTED
        exec_sh_commands([
	        %Q(cd #{SPI_SERVER_PATH}/build_dependencies/supercollider/build),
	        %Q(cmake -DSC_EL=no -DCMAKE_INSTALL_PREFIX=#{args.install_prefix} ..),
	        %Q(make -j#{args.make_jobs}),
	        %Q(cd ../..) # build folder
	      ])
	    end
		end

		desc "Build sc3_plugins from source"
		task :build_sc3_plugins, [:make_jobs, :install_prefix] do |t, args|
    args.with_defaults(:make_jobs => 1)
    args.with_defaults(:install_prefix => "")

		  OS = ask_if_raspbian if (OS == :linux_arm)

		  case OS
		  when :linux, :raspberry
		    install_packages(Dependencies::Linux.supercollider, SPI_BUILD_CONFIG.pkg_manager)
		    info("Building sc3-plugins from source...")
		    # Build sc3-plugins
		    exec_sh_commands([
		      %Q(cd #{SPI_SERVER_PATH}/build_dependencies),
		      %Q(git clone --recursive https://github.com/supercollider/sc3-plugins.git || true),
		      %Q(cd sc3-plugins),
		      %Q(git checkout Version-#{SC_PLUGINS_VERSION}),
		      %Q(git submodule init && git submodule update),
		      %Q(git submodule update --init),
		      %Q(cp -r external_libraries/nova-simd/* source/VBAPUGens),
		      %Q(mkdir -p build),
		      %Q(cd build),
		      #cmake -DSC_PATH=../../supercollider -DCMAKE_INSTALL_PREFIX=/usr/local ..
		      #cmake -DCMAKE_INSTALL_PREFIX=/usr/local --build . --config Release
		      %Q(cmake -DSC_PATH=../../supercollider -DCMAKE_INSTALL_PREFIX=#{args.install_prefix} ..),
		      %Q(cmake -DCMAKE_INSTALL_PREFIX=#{args.install_prefix} --build . --config Release),
		      %Q(make -j#{args.make_jobs}),
		      %Q(cd ../..) # build folder
		    ])
		  when :macos
				# Portable build - Package with Sonic Pi
		  when :windows
      exec_win_commands([

      ])
		  end
		end

		desc "Install Supercollider"
		task :install_supercollider, [:checkinstall] => [:build_supercollider] do |t, args|
    # Install SuperCollider
    info("Installing SuperCollider...")
		  case OS
		  when :linux, :raspberry
		    case args.checkinstall
		    when "debian"
		      exec_sh_commands([
		        %Q(cd #{SPI_SERVER_PATH}/build_dependencies/supercollider/build),
		        %Q(sudo checkinstall -D --pkgname=supercollider --pkgversion=1:#{SUPERCOLLIDER_VERSION} --pkglicense=GPL-2.0 --pkggroup=sound --nodoc --default --install=no),
		        %Q(sudo dpkg -i supercollider_#{SUPERCOLLIDER_VERSION}-1_amd64.deb),
		        %Q(cd ../..) # build folder
		      ])
		    when "rpm"
		      exec_sh_commands([
		        %Q(cd #{SPI_SERVER_PATH}/build_dependencies/supercollider/build),
		        %Q(sudo checkinstall -R --pkgname=supercollider --pkgversion=1:#{SUPERCOLLIDER_VERSION} --pkglicense=GPL-2.0 --pkggroup=sound --nodoc --default --install=no),
		        %Q(sudo rpm -i supercollider_#{SUPERCOLLIDER_VERSION}-1_amd64.rpm),
		        %Q(cd ../..) # build folder
		      ])
		    else
		    	# Install manually
		    	exec_sh_commands([
		      	%Q(cd #{SPI_SERVER_PATH}/build_dependencies/supercollider/build),
		      	%Q(make install),
		      	%Q(cd ../..) # build folder
		    	])
		    end
		  when :windows
      # Portable build - Package with Sonic Pi
      exec_win_commands([
       %Q(xcopy /E "#{SPI_SERVER_PATH}\\build_dependencies\\supercollider\\build\\server\\scsynth\\Release\\*" "native\\windows")
      ])
		  when :macos
			end
		end

		desc "Install Supercollider Plugins"
		task :install_sc3_plugins, [:checkinstall] => [:build_sc3_plugins] do |t, args|
			    # Install SuperCollider
		    info("Installing sc3-plugins...")
		  case OS
		  when :linux, :raspberry
		    case args.checkinstall
		    when "debian"
		      exec_sh_commands([
		        %Q(cd #{SPI_SERVER_PATH}/build_dependencies/sc3-plugins/build),
		        %Q(sudo checkinstall -D --pkgname=sc3-plugins --pkgversion=1:#{SC_PLUGINS_VERSION} --pkglicense=GPL-2.0 --pkggroup=sound --nodoc --default --install=no),
		        %Q(sudo dpkg -i sc3-plugins_#{SC_PLUGINS_VERSION}-1_amd64.deb),
		        %Q(cd ../..) # build folder
		      ])
		    when "rpm"
		      exec_sh_commands([
		        %Q(cd #{SPI_SERVER_PATH}/build_dependencies/supercollider/build),
		        %Q(sudo checkinstall -R --pkgname=sc3-plugins --pkgversion=1:#{SC_PLUGINS_VERSION} --pkglicense=GPL-2.0 --pkggroup=sound --nodoc --default --install=no),
		        %Q(sudo rpm -i sc3-plugins_#{SC_PLUGINS_VERSION}-1_amd64.rpm),
		        %Q(cd ../..) # build folder
		      ])
		    else
		    	# Install manually
		    	exec_sh_commands([
		      	%Q(cd #{SPI_SERVER_PATH}/build_dependencies/sc3-plugins/build),
		      	%Q(make install),
		      	%Q(cd ../..) # build folder
		    	])
		    end
		  when :windows

		  when :macos
			end
		end

		desc "Build Aubio from source"
		task :build_aubio, [:make_jobs, :install_prefix] do |t, args|
    args.with_defaults(:make_jobs => 1)
		  args.with_defaults(:install_prefix => "")

		  OS = ask_if_raspbian if (OS == :linux_arm)

    info("Building libaubio from source...")

    exec_sh_commands([
      %Q(cd #{SPI_SERVER_PATH}/build_dependencies),
      %Q(git clone https://git.aubio.org/git/aubio/ || true),
      %Q(cd aubio),
      %Q(git checkout #{AUBIO_VERSION}),
      %Q(make getwaf)
    ])

		  case OS
		  when :linux, :raspberry
		    #install_packages(Dependencies::Linux.aubio, SPI_BUILD_CONFIG.pkg_manager)
		    exec_sh_commands([
		      %Q(./waf configure --prefix=#{args.install_prefix}),
		      %Q(./waf build)
		    ])
		  when :windows
      # UNTESTED
      exec_win_commands([
		      %Q(waf configure --prefix=#{args.install_prefix}),
		      %Q(waf build)
		    ])
		  when :macos
		    # UNTESTED
		    exec_sh_commands([
		      %Q(./waf configure --prefix=#{args.install_prefix}),
		      %Q(./waf build)
		    ])
		  end
		end

		desc "Install Aubio"
		task :install_aubio => [:build_aubio] do |t, args|
		  OS = ask_if_raspbian if (OS == :linux_arm)

		  case OS
		  when :linux, :raspberry
		    info("Installing libaubio...")
		    exec_sh_commands([
        %Q(cd #{SPI_SERVER_PATH}/build_dependencies/aubio),
		      %Q(sudo ./waf install),
		      %Q(cd ..) # Build folder
		    ])
		  when :windows
      #TODO
		  when :macos
      #TODO
		  end
		end

		# Internal runtime dependencies: osmid
		# Should be built with Sonic Pi
		desc "Build osmid from source"
		task :build_osmid, [:make_jobs] do |t, args|
			args.with_defaults(:make_jobs => 1)

		  OS = ask_if_raspbian if (OS == :linux_arm)

    info("Building osmid from source, and bundling it...")
    exec_sh_commands([
      %Q(cd #{SPI_SERVER_PATH}/build_dependencies),
      %Q(git clone https://github.com/llloret/osmid.git || true),
      %Q(cd osmid),
      %Q(git checkout ${OSMID_VERSION}),
      %Q(mkdir -p build)
    ])

		  case OS
		  when :linux, :raspberry
		    #install_packages(Dependencies::Linux.osmid, SPI_BUILD_CONFIG.pkg_manager)
		    exec_sh_commands([
		      %Q(cd #{SPI_SERVER_PATH}/build_dependencies/osmid/build),
		      %Q(cmake ..),
		      %Q(make -j#{args.make_jobs}),
		      %Q(mkdir -p #{__dir__}/../native/linux/osmid),
		      %Q(install m2o o2m -t #{__dir__}/../native/linux/osmid),
		      #%Q(cd ../..) # Build folder
		    ])
		  when :windows
				# UNTESTED
      	exec_sh_commands([
		      %Q(cd #{SPI_SERVER_PATH}/build_dependencies/osmid/build),
		      %Q(cmake ..),
		      %Q(make -j#{args.make_jobs}),
		      %Q(mkdir -p #{__dir__}/../native/windows/osmid),
		      %Q(install m2o o2m -t #{__dir__}/../native/windows/osmid),
		      #%Q(cd ../..) # Build folder
		    ])
		  when :macos
      	# UNTESTED
      	exec_sh_commands([
		      %Q(cd #{SPI_SERVER_PATH}/build_dependencies/osmid/build),
		      %Q(cmake ..),
		      %Q(make -j#{args.make_jobs}),
		      %Q(mkdir -p #{__dir__}/../server/native/osx/osmid),
		      %Q(install m2o o2m -t #{__dir__}/../server/native/osx/osmid),
		      #%Q(cd ../..) # Build folder
		    ])
		  end
		end

	end
end
