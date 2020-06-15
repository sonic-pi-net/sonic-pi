#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

begin
  require 'rugged'

  module SonicPi
    class GitSave

      def initialize(path)
        path = path.encode('utf-8')
        @path = path
        begin
          @repo = Rugged::Repository.new(path + '.git')
        rescue
          begin
            @repo = Rugged::Repository.init_at path, false
          rescue
            # Repo is malformed - nuke it for now!
            FileUtils.rm_rf path + '.git'
            @repo = Rugged::Repository.init_at path, false
          end
        end
      end

      def save!(filename, content, msgpre="")
        oid = @repo.write(content, :blob)
        index = @repo.index
        index.reload
        index.add(:path => filename, :oid => oid, :mode => 0100644)

        options = {}
        options[:tree] = index.write_tree(@repo)

        options[:author] = { :email => "autosave@sonic-pi.net", :name => 'Sonic Pi Autosave', :time => Time.now }
        options[:committer] = { :email => "autosave@sonic-pi.net", :name => 'Sonic Pi Autosave', :time => Time.now }
        options[:message] ||= "#{msgpre} :~: Autosave Workspace #{filename}"
        options[:parents] = @repo.empty? ? [] : [ @repo.head.target ].compact
        options[:update_ref] = 'HEAD'

        Rugged::Commit.create(@repo, options)
      end


    end
  end

rescue LoadError
  # Rugged isn't available - don't attempt to save buffers into a local git repo
  module SonicPi
    class GitSave
      def initialize(*args)
      end

      def save!(*args)
      end
    end
  end
end
