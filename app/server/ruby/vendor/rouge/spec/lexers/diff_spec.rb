# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Diff do
  let(:subject) { Rouge::Lexers::Diff.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.diff'
      assert_guess :filename => 'foo.patch'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-diff'
      assert_guess :mimetype => 'text/x-patch'
    end

    it 'guesses by source' do
      assert_guess :source => <<-source
diff --git a/lib/rouge.rb b/lib/rouge.rb
index d228e4b..560b687 100644
--- a/lib/rouge.rb
+++ b/lib/rouge.rb
@@ -13,6 +13,7 @@ module Rouge
 end
 
 load_dir = Pathname.new(__FILE__).dirname
+load load_dir.join('rouge/text_analyzer.rb')
 load load_dir.join('rouge/token.rb')
 load load_dir.join('rouge/lexer.rb')
 load load_dir.join('rouge/lexers/text.rb')
      source

      assert_guess :source => <<-source
--- a/lib/rouge.rb
+++ b/lib/rouge.rb
@@ -13,6 +13,7 @@ module Rouge
 end
 
 load_dir = Pathname.new(__FILE__).dirname
+load load_dir.join('rouge/text_analyzer.rb')
 load load_dir.join('rouge/token.rb')
 load load_dir.join('rouge/lexer.rb')
 load load_dir.join('rouge/lexers/text.rb')
      source
    end
  end
end
