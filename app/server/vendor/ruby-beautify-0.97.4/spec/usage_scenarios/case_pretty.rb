# Test to validate case statements
def case_test opts=nil
	case test_case
	when 'Passing'
		call(:pass)
	when 'Failing'
		call(:fail)
	when 'Error'
		call(:error)
	end
end
