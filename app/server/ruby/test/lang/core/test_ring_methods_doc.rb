#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

module SonicPi
  # #1581: ring dot-methods (.take, .reflect, .invert_around ...) were only
  # documented in the tutorial, never in the searchable Lang tab. This adds a
  # single consolidated "Ring & List Methods" Lang entry.
  class RingMethodsDocTester < Minitest::Test

    def test_entry_is_registered_in_the_lang_docs
      assert SonicPi::Lang::Core.docs.key?(:ring_and_list_methods),
             "expected a :ring_and_list_methods Lang doc entry"
    end

    def test_whole_doc_set_including_new_entry_renders_to_html
      # docs_html_map iterates every registered entry and will raise on a
      # malformed one (e.g. missing :args), so a clean return validates ours too.
      map = SonicPi::Lang::Core.docs_html_map
      html = map["ring_and_list_methods"]
      refute_nil html, "new entry produced no HTML"
      assert html.include?("Ring & list methods")   # the page title
    end

    def test_page_lists_the_key_methods
      html = SonicPi::Lang::Core.docs_html_map["ring_and_list_methods"]
      %w[.take .drop .reflect .mirror .stretch .shuffle .invert_around].each do |m|
        assert html.include?(m), "page should mention #{m}"
      end
    end
  end
end
