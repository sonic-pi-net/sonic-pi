require 'open3'
require 'ripper'
require 'ruby-beautify/version'

module RubyBeautify
	extend self

	OPEN_BLOCK_START = ["module", "class", "begin", "def", 'if', 'while', 'unless', 'case']
	BOTH_BLOCK = ["else", "elsif", 'rescue', 'when']
	OPEN_BLOCK_DO  = ['do', '{']
	CLOSE_BLOCK = ['end', '}']

	OPEN_BRACKETS  = [:on_lparen, :on_lbracket, :on_lbrace, :on_embexpr_beg]
	CLOSE_BRACKETS = [:on_rparen, :on_rbracket, :on_rbrace, :on_embexpr_end]
	NEW_LINES = [:on_nl, :on_ignored_nl, :on_comment, :on_embdoc_end]


	def pretty_string(content, indent_token: "\t", indent_count: 1)
		output_string = ""
		raise "Bad Syntax" unless syntax_ok? content
		lex = ::Ripper.lex(content)

		indent_level = 0
		line_lex = []

		# walk through line tokens
		lex.each do |token|
			line_lex << token
			if NEW_LINES.include? token[1] # if type of this token is a new line

				# did we just close something?  if so, lets bring it down a level.
				if closing_block?(line_lex) || closing_assignment?(line_lex)
					indent_level -= 1 if indent_level > 0
				end

				# print our line, in place.
				line_string = line_lex.map {|l| l.last}.join
				output_string += indented_line(indent_level, indent_token, indent_count, line_string)

				# oh, we opened something did we?  lets indent for the next run.
				if opening_block?(line_lex) || opening_assignment?(line_lex)
					indent_level += 1
				end

				line_lex.clear
			end
		end

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

	# prepare an indented line. Requires the level, token, count and string.
	def indented_line(level, token = "\t", count = 1, string)
		output_string = ""
		if string =~ /^\n$/
			output_string += "\n"
		else
			indent = (token * count) * level
			output_string += "#{indent}#{string.lstrip}"
		end
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
