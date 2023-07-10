module YARD

  VERSION[0..2] == '0.9' or raise 'incompatible YARD'

  module Templates::Helpers

    # make sure the signatures are complete not simplified with
    # '...' and '?' instead of nil
    module HtmlHelper
      def signature_types(meth, link = true)
        meth = convert_method_to_overload(meth)
        if meth.respond_to?(:object) && !meth.has_tag?(:return)
          meth = meth.object
        end

        type = options.default_return || ""
        if meth.tag(:return) && meth.tag(:return).types
          types = meth.tags(:return).map {|t| t.types ? t.types : [] }.flatten.uniq
          first = link ? h(types.first) : format_types([types.first], false)
          # if types.size == 2 && types.last == 'nil'
          #   type = first + '<sup>?</sup>'
          if types.size == 2 && types.last =~ /^(Array)?<#{Regexp.quote types.first}>$/
            type = first + '<sup>+</sup>'
            # elsif types.size > 2
            #   type = [first, '...'].join(', ')
          elsif types == ['void'] && options.hide_void_return
            type = ""
          else
            type = link ? h(types.join(", ")) : format_types(types, false)
          end
        elsif !type.empty?
          type = link ? h(type) : format_types([type], false)
        end
        type = "#{type} " unless type.empty?
        type
      end

      # enables :strikethrough extension
      def html_markup_markdown(text)
        # TODO: other libraries might be more complex
        provider = markup_class(:markdown)
        if provider.to_s == 'RDiscount'
          provider.new(text, :autolink).to_html
        elsif provider.to_s == 'RedcarpetCompat'
          provider.new(text, :no_intraemphasis, :gh_blockcode,
                       :fenced_code, :autolink, :tables,
                       :lax_spacing, :strikethrough).to_html
        else
          provider.new(text).to_html
        end
      end
    end
  end
end
