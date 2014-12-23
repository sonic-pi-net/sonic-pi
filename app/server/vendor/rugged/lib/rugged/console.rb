# Loaded by script/console. Land helpers here.

def repo
  Rugged::Repository.new(File.expand_path('../../../', __FILE__))
end

Pry.config.prompt = lambda do |context, nesting, pry|
  "[rugged] #{context}> "
end
