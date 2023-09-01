def sort_listing(list)
  list.sort_by do |o|
    [o.scope.to_s,
     object == o.namespace ? 0 : 1, # sort owned methods first
     # o.namespace.to_s.downcase, # sort by included module
     o.name.to_s.downcase]
  end
end

# TODO (pitr-ch 01-Jan-2019): enable inherited methods including, and do review of the documentation

# def method_listing(include_specials = true)
#   return @smeths ||= method_listing.reject {|o| special_method?(o) } unless include_specials
#   return @meths if defined?(@meths) && @meths
#   @meths = object.meths(:inherited => true, :included => !options.embed_mixins.empty?)
#   unless options.embed_mixins.empty?
#     @meths = @meths.reject {|m| options.embed_mixins_match?(m.namespace) == false }
#   end
#   @meths = sort_listing(prune_method_listing(@meths))
#   @meths
# end
