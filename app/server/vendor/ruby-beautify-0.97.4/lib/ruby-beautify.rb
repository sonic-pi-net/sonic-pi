require 'open3'
require 'ripper'
require 'ruby-beautify/version'

module RubyBeautify
	extend self

	OPEN_BLOCK_START = ["module", "class", "begin", "def", 'if', 'while', 'unless', 'case']
	BOTH_BLOCK = ["else", "elsif", 'rescue', 'when']
	OPEN_BLOCK_DO  = ['do', '{']
	CLOSE_BLOCK = ['end', '}']

	OPEN_BRACKETS  = [:on_lparen, :on_lbracket, :on_lbrace, :on_embexpr_beg, :on_tlambeg]
	CLOSE_BRACKETS = [:on_rparen, :on_rbracket, :on_rbrace, :on_embexpr_end]
	NEW_LINES = [:on_nl, :on_ignored_nl, :on_comment, :on_embdoc_end, :on_heredoc_end]


	def pretty_string(content, indent_token: "\t", indent_count: 1, indent_empty: false, syntax_check: true)
		output_string = ""
		raise "Bad Syntax" if syntax_check && !syntax_ok?(content)
		lex = ::Ripper.lex(content)

		content_index = 0
		indent_level = 0
		heredoc_level = 0
		line_lex = []

		# walk through tokens, line by line
		lex.each_with_index do |token, i|
			last_token = (i == lex.size-1)
			line_lex << token

			heredoc_level += 1 if token[1] == :on_heredoc_beg
			heredoc_level -= 1 if token[1] == :on_heredoc_end

			if (heredoc_level == 0 && NEW_LINES.include?(token[1])) || last_token # if token is a new line or last token in list
				# did we just close something?  if so, lets bring it down a level.
				if closing_block?(line_lex) || closing_assignment?(line_lex)
					indent_level -= 1 if indent_level > 0
				end

				# print our line, in place.
				line_string = line_lex.map {|l| l.last}.join
				content_index += line_string.length
				line_string.strip!
				output_string += (indent_token * indent_count * indent_level) + line_string if indent_empty || !line_string.empty?
				output_string += "\n" unless last_token

				# oh, we opened something did we?  lets indent for the next run.
				if opening_block?(line_lex) || opening_assignment?(line_lex)
					indent_level += 1
				end

				line_lex.clear
			end
		end

		# in case of syntax error, add remaining input that Ripper didn't want to lex
		output_string += content[content_index, content.length]

		return output_string
	end

	# check the syntax of a string, will pipe it through the ruby bin to see if
	# it has a valid syntax.
	def syntax_ok?(string)
		out, err, status = Open3.capture3("ruby -c -", stdin_data:string )
		return false unless err.empty?
		return true
	end

	# same trick as opening_block
	def opening_assignment?(line_lex)
		opens = opening_assignment_count line_lex
		closes = closing_assignment_count line_lex
		return false if opens == closes
		return true if opens > closes
	end

	# ...
	def closing_assignment?(line_lex)
		opens = opening_assignment_count line_lex
		closes = closing_assignment_count line_lex
		return false if opens == closes
		return true if closes > opens
	end

	# test for assignment from a block
	def contains_block_assignment?(line_lex)
		compacted_line = line_lex.reject{|x| x[1] == :on_sp} #remove spaces
		idx = compacted_line.rindex{|x| ['=', '||='].include? x[2]} #find last equal
		if idx
			return OPEN_BLOCK_START.include?(compacted_line[idx+1][2]) #check for if/begin block
		end
		return false
	end

	# is the first word a key word?
	def starts_block?(line_lex)
		return true if contains_block_assignment? line_lex
		line_lex.each do |x|
			# search for a first non-space token
			if not x[1] == :on_sp
				return x[1] == :on_kw && OPEN_BLOCK_START.include?(x[2])
			end
		end
	end

	# is the first word one of our 'both' keywords?
	def both_block?(line_lex)
		line_lex.each do |x|
			# search for a first non-space token
			if not x[1] == :on_sp
				return x[1] == :on_kw && BOTH_BLOCK.include?(x[2])
			end
		end
	end

	# kinda complex, we count open/close to determine if we ultimately have a
	# hanging line.   Always true if it's a both_block.
	def opening_block?(line_lex)
		opens = (starts_block?(line_lex) || both_block?(line_lex)) ? 1 : 0
		opens += opening_block_count line_lex
		closes = closing_block_count line_lex
		return false if opens == closes
		return true if opens > closes
	end

	# kinda complex, we count open/close to determine if we ultimately have close a
	# hanging line.   Always true if it's a both_block.
	def closing_block?(line_lex)
		return true if both_block? line_lex
		opens = starts_block?(line_lex) ? 1 : 0
		opens += opening_block_count line_lex
		closes = closing_block_count line_lex
		return false if opens == closes
		return true if opens < closes
	end

	private
	# how many times do we open in this line?
	def opening_block_count(line_lex)
		line_lex.select {|l| l[1] == :on_kw && OPEN_BLOCK_DO.include?(l[2])}.count
	end

	# how many times do we close?
	def closing_block_count(line_lex)
		line_lex.select {|l| l[1] == :on_kw && CLOSE_BLOCK.include?(l[2])}.count
	end

	# count the amount of opening assignments.
	def opening_assignment_count(line_lex)
		line_lex.select {|l| OPEN_BRACKETS.include? l[1]}.count
	end

	# count the amount of closing assignments.
	def closing_assignment_count(line_lex)
		line_lex.select {|l| CLOSE_BRACKETS.include? l[1]}.count
	end

	# try to find a config and return a modified argv, walks all the way to root
	# looking for '.ruby-beautify' and then returns an argv with our new options.
	def config_argv
		target_dirs = Dir.pwd.split("/")

		# sloppy walk method, not well tested but shouldn't get in the way.
		while (target_dirs.any?)
			target = "#{target_dirs.join("/")}/.ruby-beautify"
			break if File.exist?(target)
			target_dirs.pop
			target = nil
		end

		return ARGV unless target
		lines = open(target).readlines
		return ARGV + lines.map {|l| l.chomp}
	end
end
