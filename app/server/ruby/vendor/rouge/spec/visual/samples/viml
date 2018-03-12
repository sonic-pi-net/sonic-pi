" Vim completion script
" Language:	PHP
" Maintainer:	Mikolaj Machowski ( mikmach AT wp DOT pl )
" Last Change:	2006 May 9
"
"   TODO:
"   - Class aware completion:
"      a) caching?
"   - Switching to HTML (XML?) completion (SQL) inside of phpStrings
"   - allow also for XML completion <- better do html_flavor for HTML
"     completion
"   - outside of <?php?> getting parent tag may cause problems. Heh, even in
"     perfect conditions GetLastOpenTag doesn't cooperate... Inside of
"     phpStrings this can be even a bonus but outside of <?php?> it is not the
"     best situation

function! phpcomplete#CompletePHP(findstart, base)
	if a:findstart
		unlet! b:php_menu
		" Check if we are inside of PHP markup
		let pos = getpos('.')
		let phpbegin = searchpairpos('<?', '', '?>', 'bWn',
				\ 'synIDattr(synID(line("."), col("."), 0), "name") =~? "string\|comment"')
		let phpend   = searchpairpos('<?', '', '?>', 'Wn',
				\ 'synIDattr(synID(line("."), col("."), 0), "name") =~? "string\|comment"')

		if phpbegin == [0,0] && phpend == [0,0]
			" We are outside of any PHP markup. Complete HTML
			let htmlbegin = htmlcomplete#CompleteTags(1, '')
			let cursor_col = pos[2]
			let base = getline('.')[htmlbegin : cursor_col]
			let b:php_menu = htmlcomplete#CompleteTags(0, base)
			return htmlbegin
		else
			" locate the start of the word
			let line = getline('.')
			let start = col('.') - 1
			let curline = line('.')
			let compl_begin = col('.') - 2
			while start >= 0 && line[start - 1] =~ '[a-zA-Z_0-9\x7f-\xff$]'
				let start -= 1
			endwhile
			let b:compl_context = getline('.')[0:compl_begin]
			return start

			" We can be also inside of phpString with HTML tags. Deal with
			" it later (time, not lines).
		endif

	endif
	" If exists b:php_menu it means completion was already constructed we
	" don't need to do anything more
	if exists("b:php_menu")
		return b:php_menu
	endif
	" Initialize base return lists
	let res = []
	let res2 = []
	" a:base is very short - we need context
	if exists("b:compl_context")
		let context = b:compl_context
		unlet! b:compl_context
	endif

	if !exists('g:php_builtin_functions')
		call phpcomplete#LoadData()
	endif

	let scontext = substitute(context, '\$\?[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*$', '', '')

	if scontext =~ '\(=\s*new\|extends\)\s\+$'
		" Complete class name
		" Internal solution for finding classes in current file.
		let file = getline(1, '$')
		call filter(file,
				\ 'v:val =~ "class\\s\\+[a-zA-Z_\\x7f-\\xff][a-zA-Z_0-9\\x7f-\\xff]*\\s*("')
		let fnames = join(map(tagfiles(), 'escape(v:val, " \\#%")'))
		let jfile = join(file, ' ')
		let int_values = split(jfile, 'class\s\+')
		let int_classes = {}
		for i in int_values
			let c_name = matchstr(i, '^[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*')
			if c_name != ''
				let int_classes[c_name] = ''
			endif
		endfor

		" Prepare list of classes from tags file
		let ext_classes = {}
		let fnames = join(map(tagfiles(), 'escape(v:val, " \\#%")'))
		if fnames != ''
			exe 'silent! vimgrep /^'.a:base.'.*\tc\(\t\|$\)/j '.fnames
			let qflist = getqflist()
			if len(qflist) > 0
				for field in qflist
					" [:space:] thing: we don't have to be so strict when
					" dealing with tags files - entries there were already
					" checked by ctags.
					let item = matchstr(field['text'], '^[^[:space:]]\+')
					let ext_classes[item] = ''
				endfor
			endif
		endif

		" Prepare list of built in classes from g:php_builtin_functions
		if !exists("g:php_omni_bi_classes")
			let g:php_omni_bi_classes = {}
			for i in keys(g:php_builtin_object_functions)
				let g:php_omni_bi_classes[substitute(i, '::.*$', '', '')] = ''
			endfor
		endif

		let classes = sort(keys(int_classes))
		let classes += sort(keys(ext_classes))
		let classes += sort(keys(g:php_omni_bi_classes))

		for m in classes
			if m =~ '^'.a:base
				call add(res, m)
			endif
		endfor

		let final_menu = []
		for i in res
			let final_menu += [{'word':i, 'kind':'c'}]
		endfor

		return final_menu

	elseif scontext =~ '\(->\|::\)$'
		" Complete user functions and variables
		" Internal solution for current file.
		" That seems as unnecessary repeating of functions but there are
		" few not so subtle differences as not appending of $ and addition
		" of 'kind' tag (not necessary in regular completion)

		if scontext =~ '->$' && scontext !~ '\$this->$'

			" Get name of the class
			let classname = phpcomplete#GetClassName(scontext)

			" Get location of class definition, we have to iterate through all
			" tags files separately because we need relative path from current
			" file to the exact file (tags file can be in different dir)
			if classname != ''
				let classlocation = phpcomplete#GetClassLocation(classname)
			else
				let classlocation = ''
			endif

			if classlocation == 'VIMPHP_BUILTINOBJECT'

				for object in keys(g:php_builtin_object_functions)
					if object =~ '^'.classname
						let res += [{'word':substitute(object, '.*::', '', ''),
							   	\    'info': g:php_builtin_object_functions[object]}]
					endif
				endfor

				return res

			endif

			if filereadable(classlocation)
				let classfile = readfile(classlocation)
				let classcontent = ''
				let classcontent .= "\n".phpcomplete#GetClassContents(classfile, classname)
				let sccontent = split(classcontent, "\n")

				" YES, YES, YES! - we have whole content including extends!
				" Now we need to get two elements: public functions and public
				" vars
				" NO, NO, NO! - third separate filtering looking for content
				" :(, but all of them have differences. To squeeze them into
				" one implementation would require many additional arguments
				" and ifs. No good solution
				" Functions declared with public keyword or without any
				" keyword are public
				let functions = filter(deepcopy(sccontent),
						\ 'v:val =~ "^\\s*\\(static\\s\\+\\|public\\s\\+\\)*function"')
				let jfuncs = join(functions, ' ')
				let sfuncs = split(jfuncs, 'function\s\+')
				let c_functions = {}
				for i in sfuncs
					let f_name = matchstr(i,
							\ '^&\?\zs[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\ze')
					let f_args = matchstr(i,
							\ '^&\?[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\s*(\zs.\{-}\ze)\_s*{')
					if f_name != ''
						let c_functions[f_name.'('] = f_args
					endif
				endfor
				" Variables declared with var or with public keyword are
				" public
				let variables = filter(deepcopy(sccontent),
						\ 'v:val =~ "^\\s*\\(public\\|var\\)\\s\\+\\$"')
				let jvars = join(variables, ' ')
				let svars = split(jvars, '\$')
				let c_variables = {}
				for i in svars
					let c_var = matchstr(i,
							\ '^\zs[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\ze')
					if c_var != ''
						let c_variables[c_var] = ''
					endif
				endfor

				let all_values = {}
				call extend(all_values, c_functions)
				call extend(all_values, c_variables)

				for m in sort(keys(all_values))
					if m =~ '^'.a:base && m !~ '::'
						call add(res, m)
					elseif m =~ '::'.a:base
						call add(res2, m)
					endif
				endfor

				let start_list = res + res2

				let final_list = []
				for i in start_list
					if has_key(c_variables, i)
						let class = ' '
						if all_values[i] != ''
							let class = i.' class '
						endif
						let final_list +=
								\ [{'word':i,
								\   'info':class.all_values[i],
								\   'kind':'v'}]
					else
						let final_list +=
								\ [{'word':substitute(i, '.*::', '', ''),
								\   'info':i.all_values[i].')',
								\   'kind':'f'}]
					endif
				endfor

				return final_list

			endif

		endif

		if a:base =~ '^\$'
			let adddollar = '$'
		else
			let adddollar = ''
		endif
		let file = getline(1, '$')
		let jfile = join(file, ' ')
		let sfile = split(jfile, '\$')
		let int_vars = {}
		for i in sfile
			if i =~ '^\$[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\s*=\s*new'
				let val = matchstr(i, '^[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*').'->'
			else
				let val = matchstr(i, '^[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*')
			endif
			if val !~ ''
				let int_vars[adddollar.val] = ''
			endif
		endfor

		" ctags has good support for PHP, use tags file for external
		" variables
		let fnames = join(map(tagfiles(), 'escape(v:val, " \\#%")'))
		let ext_vars = {}
		if fnames != ''
			let sbase = substitute(a:base, '^\$', '', '')
			exe 'silent! vimgrep /^'.sbase.'.*\tv\(\t\|$\)/j '.fnames
			let qflist = getqflist()
			if len(qflist) > 0
				for field in qflist
					let item = matchstr(field['text'], '^[^[:space:]]\+')
					" Add -> if it is possible object declaration
					let classname = ''
					if field['text'] =~ item.'\s*=\s*new\s\+'
						let item = item.'->'
						let classname = matchstr(field['text'],
								\ '=\s*new\s\+\zs[a-zA-Z_0-9\x7f-\xff]\+\ze')
					endif
					let ext_vars[adddollar.item] = classname
				endfor
			endif
		endif

		" Now we have all variables in int_vars dictionary
		call extend(int_vars, ext_vars)

		" Internal solution for finding functions in current file.
		let file = getline(1, '$')
		call filter(file,
				\ 'v:val =~ "function\\s\\+&\\?[a-zA-Z_\\x7f-\\xff][a-zA-Z_0-9\\x7f-\\xff]*\\s*("')
		let fnames = join(map(tagfiles(), 'escape(v:val, " \\#%")'))
		let jfile = join(file, ' ')
		let int_values = split(jfile, 'function\s\+')
		let int_functions = {}
		for i in int_values
			let f_name = matchstr(i,
					\ '^&\?\zs[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\ze')
			let f_args = matchstr(i,
					\ '^&\?[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\s*(\zs.\{-}\ze)\_s*{')
			let int_functions[f_name.'('] = f_args.')'
		endfor

		" Prepare list of functions from tags file
		let ext_functions = {}
		if fnames != ''
			exe 'silent! vimgrep /^'.a:base.'.*\tf\(\t\|$\)/j '.fnames
			let qflist = getqflist()
			if len(qflist) > 0
				for field in qflist
					" File name
					let item = matchstr(field['text'], '^[^[:space:]]\+')
					let fname = matchstr(field['text'], '\t\zs\f\+\ze')
					let prototype = matchstr(field['text'],
							\ 'function\s\+&\?[^[:space:]]\+\s*(\s*\zs.\{-}\ze\s*)\s*{\?')
					let ext_functions[item.'('] = prototype.') - '.fname
				endfor
			endif
		endif

		let all_values = {}
		call extend(all_values, int_functions)
		call extend(all_values, ext_functions)
		call extend(all_values, int_vars) " external variables are already in
		call extend(all_values, g:php_builtin_object_functions)

		for m in sort(keys(all_values))
			if m =~ '\(^\|::\)'.a:base
				call add(res, m)
			endif
		endfor

		let start_list = res

		let final_list = []
		for i in start_list
			if has_key(int_vars, i)
				let class = ' '
				if all_values[i] != ''
					let class = i.' class '
				endif
				let final_list += [{'word':i, 'info':class.all_values[i], 'kind':'v'}]
			else
				let final_list +=
						\ [{'word':substitute(i, '.*::', '', ''),
						\   'info':i.all_values[i],
						\   'kind':'f'}]
			endif
		endfor

		return final_list
	endif

	if a:base =~ '^\$'
		" Complete variables
		" Built-in variables {{{
		let g:php_builtin_vars = {'$GLOBALS':'',
								\ '$_SERVER':'',
								\ '$_GET':'',
								\ '$_POST':'',
								\ '$_COOKIE':'',
								\ '$_FILES':'',
								\ '$_ENV':'',
								\ '$_REQUEST':'',
								\ '$_SESSION':'',
								\ '$HTTP_SERVER_VARS':'',
								\ '$HTTP_ENV_VARS':'',
								\ '$HTTP_COOKIE_VARS':'',
								\ '$HTTP_GET_VARS':'',
								\ '$HTTP_POST_VARS':'',
								\ '$HTTP_POST_FILES':'',
								\ '$HTTP_SESSION_VARS':'',
								\ '$php_errormsg':'',
								\ '$this':''
								\ }
		" }}}

		" Internal solution for current file.
		let file = getline(1, '$')
		let jfile = join(file, ' ')
		let int_vals = split(jfile, '\ze\$')
		let int_vars = {}
		for i in int_vals
			if i =~ '^\$[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\s*=\s*new'
				let val = matchstr(i,
						\ '^\$[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*').'->'
			else
				let val = matchstr(i,
						\ '^\$[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*')
			endif
			if val != ''
				let int_vars[val] = ''
			endif
		endfor

		call extend(int_vars,g:php_builtin_vars)

		" ctags has support for PHP, use tags file for external variables
		let fnames = join(map(tagfiles(), 'escape(v:val, " \\#%")'))
		let ext_vars = {}
		if fnames != ''
			let sbase = substitute(a:base, '^\$', '', '')
			exe 'silent! vimgrep /^'.sbase.'.*\tv\(\t\|$\)/j '.fnames
			let qflist = getqflist()
			if len(qflist) > 0
				for field in qflist
					let item = '$'.matchstr(field['text'], '^[^[:space:]]\+')
					let m_menu = ''
					" Add -> if it is possible object declaration
					if field['text'] =~ item.'\s*=\s*new\s\+'
						let item = item.'->'
						let m_menu = matchstr(field['text'],
								\ '=\s*new\s\+\zs[a-zA-Z_0-9\x7f-\xff]\+\ze')
					endif
					let ext_vars[item] = m_menu
				endfor
			endif
		endif

		call extend(int_vars, ext_vars)
		let g:a0 = keys(int_vars)

		for m in sort(keys(int_vars))
			if m =~ '^\'.a:base
				call add(res, m)
			endif
		endfor

		let int_list = res

		let int_dict = []
		for i in int_list
			if int_vars[i] != ''
				let class = ' '
				if int_vars[i] != ''
					let class = i.' class '
				endif
				let int_dict += [{'word':i, 'info':class.int_vars[i], 'kind':'v'}]
			else
				let int_dict += [{'word':i, 'kind':'v'}]
			endif
		endfor

		return int_dict

	else
		" Complete everything else -
		"  + functions,  DONE
		"  + keywords of language DONE
		"  + defines (constant definitions), DONE
		"  + extend keywords for predefined constants, DONE
		"  + classes (after new), DONE
		"  + limit choice after -> and :: to funcs and vars DONE

		" Internal solution for finding functions in current file.
		let file = getline(1, '$')
		call filter(file,
				\ 'v:val =~ "function\\s\\+&\\?[a-zA-Z_\\x7f-\\xff][a-zA-Z_0-9\\x7f-\\xff]*\\s*("')
		let fnames = join(map(tagfiles(), 'escape(v:val, " \\#%")'))
		let jfile = join(file, ' ')
		let int_values = split(jfile, 'function\s\+')
		let int_functions = {}
		for i in int_values
			let f_name = matchstr(i,
					\ '^&\?\zs[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\ze')
			let f_args = matchstr(i,
					\ '^&\?[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\s*(\s*\zs.\{-}\ze\s*)\_s*{')
			let int_functions[f_name.'('] = f_args.')'
		endfor

		" Prepare list of functions from tags file
		let ext_functions = {}
		if fnames != ''
			exe 'silent! vimgrep /^'.a:base.'.*\tf\(\t\|$\)/j '.fnames
			let qflist = getqflist()
			if len(qflist) > 0
				for field in qflist
					" File name
					let item = matchstr(field['text'], '^[^[:space:]]\+')
					let fname = matchstr(field['text'], '\t\zs\f\+\ze')
					let prototype = matchstr(field['text'],
							\ 'function\s\+&\?[^[:space:]]\+\s*(\s*\zs.\{-}\ze\s*)\s*{\?')
					let ext_functions[item.'('] = prototype.') - '.fname
				endfor
			endif
		endif

		" All functions
		call extend(int_functions, ext_functions)
		call extend(int_functions, g:php_builtin_functions)

		" Internal solution for finding constants in current file
		let file = getline(1, '$')
		call filter(file, 'v:val =~ "define\\s*("')
		let jfile = join(file, ' ')
		let int_values = split(jfile, 'define\s*(\s*')
		let int_constants = {}
		for i in int_values
			let c_name = matchstr(i, '\(["'']\)\zs[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\ze\1')
			" let c_value = matchstr(i,
			" \ '\(["'']\)[a-zA-Z_\x7f-\xff][a-zA-Z_0-9\x7f-\xff]*\1\s*,\s*\zs.\{-}\ze\s*)')
			if c_name != ''
				let int_constants[c_name] = '' " c_value
			endif
		endfor

		" Prepare list of constants from tags file
		let fnames = join(map(tagfiles(), 'escape(v:val, " \\#%")'))
		let ext_constants = {}
		if fnames != ''
			exe 'silent! vimgrep /^'.a:base.'.*\td\(\t\|$\)/j '.fnames
			let qflist = getqflist()
			if len(qflist) > 0
				for field in qflist
					let item = matchstr(field['text'], '^[^[:space:]]\+')
					let ext_constants[item] = ''
				endfor
			endif
		endif

		" All constants
		call extend(int_constants, ext_constants)
		" Treat keywords as constants

		let all_values = {}

		" One big dictionary of functions
		call extend(all_values, int_functions)

		" Add constants
		call extend(all_values, int_constants)
		" Add keywords
		call extend(all_values, g:php_keywords)

		for m in sort(keys(all_values))
			if m =~ '^'.a:base
				call add(res, m)
			endif
		endfor

		let int_list = res

		let final_list = []
		for i in int_list
			if has_key(int_functions, i)
				let final_list +=
						\ [{'word':i,
						\   'info':i.int_functions[i],
						\   'kind':'f'}]
			elseif has_key(int_constants, i)
				let final_list += [{'word':i, 'kind':'d'}]
			else
				let final_list += [{'word':i}]
			endif
		endfor

		return final_list

	endif

endfunction
" vim:set foldmethod=marker:
