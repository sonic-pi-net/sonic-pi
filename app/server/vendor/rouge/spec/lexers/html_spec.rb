# -*- coding: utf-8 -*- #

describe Rouge::Lexers::HTML do
  let(:subject) { Rouge::Lexers::HTML.new }
  include Support::Lexing

  it 'lexes embedded script tags' do
    assert_no_errors '<script>x && x < y;</script>'
  end

  describe 'lexing' do
    include Support::Lexing

    describe 'element names' do
      it 'allow dashes to support custom elements' do
        assert_tokens_equal '<custom-element></custom-element>',
                            ['Name.Tag', '<custom-element></custom-element>']
      end
    end
  end

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.html'
      assert_guess :filename => 'foo.htm'
      assert_guess :filename => 'foo.xhtml'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/html'
      assert_guess :mimetype => 'application/xhtml+xml'
    end

    it 'guesses by source' do
      assert_guess :source => '<!DOCTYPE html>'
      assert_guess :source => <<-source
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE html
            PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
            "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
        <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
        </html>
      source

      assert_guess :source => <<-source
        <!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
        <html lang="ar" dir="rtl" xmlns="http://www.w3.org/1999/xhtml">
        </html>
      source

      assert_guess :source => '<html></html>'
    end
  end
end
