#!/usr/bin/env ruby -w

def unversioned_files
  `svn status`.
    select { |line| line =~ /^\?/ }.
    collect { |line| line[7..-1] }
end

unversioned_files.each do |file|
  `svn add #{file}`
end