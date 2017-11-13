{{foo}}

{{! "bar" should be regular HTML }}
{{foo}} \{{bar}} {{baz}}

{{foo}} \{{bar}} \{{baz}}

{{foo}} \{{{bar}}} {{baz}}

{{foo/bar}}

{{foo.bar}}
{{foo.bar.baz}}
{{foo.[bar]}}
{{foo.[bar]}}{{foo.[baz]}}
{{.}}
{{../foo/bar}}
{{../foo.bar}}
{{this/foo}}
{{  foo  }}
{{  foo  
bar }}
foo {{ bar }} baz
{{> foo}}
{{> foo bar }}
{{>foo}}
{{>foo  }}
foo {{! this is a comment }} bar {{ baz }}
foo {{!-- this is a {{comment}} --}} bar {{ baz }}

foo {{!-- this is a
{{comment}}
--}} bar {{ baz }}

{{#foo}}content{{/foo}}

{{^}}
{{else}}
{{ else }}

{{^foo}}
{{^ foo  }}
{{ foo bar baz }}
{{ foo bar "baz" }}
{{ foo bar 'baz' }}
{{ foo bar "baz bat" }}
{{ foo "bar\"baz" }}

{{ foo 'bar\'baz' }}

{{ foo 1 }}
{{ foo true }}
{{ foo false }}

{{ foo bar=baz }}
{{ foo bar baz=bat }}
{{ foo bar baz=1 }}
{{ foo bar baz=true }}
{{ foo bar baz=false }}
{{ foo bar
   baz=bat }}

{{ foo bar baz="bat" }}
{{ foo bar baz="bat" bam=wot }}
{{foo omg bar=baz bat="bam"}}

{{ @foo }}
{{ foo @bar }}
{{ foo bar=@baz }}

{{#foo bar}}
{{/foo}}
