doctype html
html
  head
    title Slim Examples

  / This is a comment for the body
  body
    h1 Markup examples

    / Inline nested ruby expressions
    p Nested interp #{list.map { |x| x + 1 }.inject(&:+)} parses ok
    
    /! HTML comment
    #content
      p This example shows you how a basic Slim file looks like.
      span class=ruby_method data-something='hmmm' = some_other_ruby_method

      / Splats
      .csclass data-attr=RUBY_CONSTANT width = "23px" Woot!
      .card *method_which_return_hash! = place.name
      .herp *@hash_instance_variable = place.name
      #derp*{'data-url'=>place_path(place), 'data-id'=>place.id} = place.name

      / Dynamic tags
      *ruby_tag Some text for it
      *some_tag_method( CONSTANT, "string", 1234.3 ) Woot!

      p
        ' Some text with <bold>bold text</bold>

      <p attr="something">
        ' Some other text with #{interp}


      javascript:
        var foo = 23;
        function bar( lel ) {
          return lel * 4;
        }

      / And this is a \
        wrapping comment
      / And a
        p non-wrapping one

      /[if IE]
        p Get a better browser

      a href='http://google.com/' To Google!

      css:
        p {
          font-family: #{font_family( "Tahoma" ) && Time.now};
        }

      == yield

      - unless items.empty?
        table
          - for item in items do
            tr
              td.name = item.name
              td.price = item.price
      - else
        p<
         | No items found.  Please add some inventory.
           Thank you! a href="lol"
           a lol
         a lol


      coffee:
        square = (x) -> x * x

      markdown:
        - Hello from #{SomeClass.interpolation_works!}

    div id="footer"
      = 3.times { render 'footer' }
      | Copyright © #{year} #{author}
