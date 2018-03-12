# -*- coding: utf-8 -*-

=begin
  gettext/text_domain_group - GetText::TextDomainGroup class

  Copyright (C) 2009  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby or LGPL.

=end

module GetText

  class TextDomainGroup
    attr_reader :text_domains

    def initialize
      @text_domains = []
    end

    def add(text_domain)
      @text_domains.unshift(text_domain) unless @text_domains.include? text_domain
    end
  end
end
