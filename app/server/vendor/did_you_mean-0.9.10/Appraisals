appraise "activerecord_32" do
  gem "activerecord", "~> 3.2.0"
end

appraise "activerecord_40" do
  gem "activerecord", "~> 4.0.0"
end

appraise "activerecord_41" do
  gem "activerecord", "~> 4.1.0"
end

appraise "activerecord_42" do
  gem "activerecord", "~> 4.2.0"
end

appraise "activerecord_edge" do
  git 'git://github.com/rails/rails.git' do
    gem 'activerecord', require: 'activerecord'
  end

  gem 'arel', :github => 'rails/arel'
end
