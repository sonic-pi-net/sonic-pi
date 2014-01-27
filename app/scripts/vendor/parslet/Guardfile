guard 'rspec', :version => 2 do
  watch(%r(^spec/(.*)_spec.rb))
  watch(%r(^lib/(.*)\.rb))                { |m| "spec/#{m[1]}_spec.rb" }
  watch('spec/spec_helper.rb')            { "spec" }

	watch(%r'^lib/parslet/bytecode/(.*)\.rb') { 'spec/acceptance/vm_spec.rb' }
	watch(%r'^lib/parslet/pattern/(.*)\.rb') { 'spec/parslet/pattern_spec.rb' }
end
