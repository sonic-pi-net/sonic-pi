require 'open-uri'
require 'json'
require 'set'

CWD = File.expand_path(File.dirname(__FILE__))

IGNORED_METHODS = %w(
  git_blob_free
  git_blob_lookup
  git_blob_lookup_prefix
  git_blob_id
  git_blob_owner
  git_commit_create_v
  git_commit_free
  git_commit_id
  git_commit_lookup_prefix
  git_commit_parent_oid
  git_commit_time_offset
  git_commit_tree_oid
  git_config_file__ondisk
  git_config_find_global
  git_config_find_system
  git_diff_list_free
  git_diff_patch_free
  git_index_entry_stage
  git_indexer_stream_free
  git_note_free
  git_object__size
  git_odb_add_alternate
  git_odb_add_backend
  git_odb_backend_loose
  git_odb_backend_pack
  git_odb_new
  git_odb_open_rstream
  git_odb_read_header
  git_odb_read_prefix
  git_odb_write
  git_oid_allocfmt
  git_oid_cpy
  git_oid_ncmp
  git_oid_pathfmt
  git_oid_streq
  git_oid_tostr
  git_packbuilder_free
  git_reference_owner
  git_reference_listall
  git_reflog_delete
  git_reflog_rename
  git_repository_odb
  git_repository_set_odb
  git_signature_dup
  git_signature_now
  git_tag_free
  git_tag_id
  git_tag_lookup
  git_tag_lookup_prefix
  git_tag_target_oid
  git_tree_free
  git_tree_id
  git_tree_lookup
  git_tree_lookup_prefix
  git_strarray_copy
  git_trace_set
  imaxdiv
)

method_list = nil

# The list of methods in libgit2 that we want coverage for
open('http://libgit2.github.com/libgit2/HEAD.json') do |f|
  json_data = JSON.parse(f.read())
  method_list = json_data['groups']
end

# Don't look for the methods in IGNORED_METHODS.
look_for = []
method_list.each do |_, methods|
  methods.reject! { |m| IGNORED_METHODS.include? m }
  look_for += methods
end

# Look at the .c and .h files in the rugged directory
source_files = Dir.glob("#{CWD}/../../ext/rugged/*.{c,h}")

# If any of the files contain the string representation
# of a libgit2 method, add it to our set of found methods
found = Set.new
source_files.each do |file|
  File.open(file) do |f|
    contents = f.read()
    look_for.each do |method|
      if contents.index(method) != nil
        found.add(method)
      end
    end
  end
end

# Keep a count of missing and total
total_missing = 0
total_methods = 0

# Print the results for each group
method_list.each do |group, group_methods|

  # Skip the group if all methods are ignored
  next if group_methods.size == 0

  # What are for we missing for this group?
  group_miss = group_methods.reject {|m| found.include? m}
  print "\n#{group} [#{group_methods.size - group_miss.size}/#{group_methods.size}]: "

  # Add the numbers to our grand total running count
  total_missing += group_miss.size
  total_methods += group_methods.size

  # Unit test style printout. A dot is a match, an 'M' is a miss.
  group_methods.each do |m|
    print found.include?(m) ? "." : "M"
  end

  print "\n"

  # Print out what is missing
  if not group_miss.empty?
    puts "  > missing: " + "#{group_miss.join(", ")}"
  end
end

# The grand tally
percent = (100.0 * (total_methods - total_missing) / total_methods).round
puts "\n" + "=" * 60
puts "\nTOTAL: [#{total_methods - total_missing}/#{total_methods}] wrapped. (#{percent}% coverage)"
