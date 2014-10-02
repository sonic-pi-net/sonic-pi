#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

require 'rugged'

module SonicPi
  class GitSave

    def initialize(path)
      @path = path
      begin
        @repo = Rugged::Repository.new(path + '/.git')
      rescue Rugged::OSError => e
        @repo = Rugged::Repository.init_at path, false
      end
    end

    def save!(filename, content)
      puts "saving: #{filename}"
      oid = @repo.write(content, :blob)
      index = @repo.index
      index.reload
      index.add(:path => filename, :oid => oid, :mode => 0100644)

      options = {}
      options[:tree] = index.write_tree(@repo)

      options[:author] = { :email => "autosave@sonic-pi.net", :name => 'Sonic Pi Autosave', :time => Time.now }
      options[:committer] = { :email => "autosave@sonic-pi.net", :name => 'Sonic Pi Autosave', :time => Time.now }
      options[:message] ||= "Autosave Workspace #{filename}"
      options[:parents] = @repo.empty? ? [] : [ @repo.head.target ].compact
      options[:update_ref] = 'HEAD'

      Rugged::Commit.create(@repo, options)
    end


  end
end
