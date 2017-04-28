# Test for lambda literal
class ArrowOperator
	def one_liner
		x = -> (x) {x * x}
	end
	def one_liner2args
		x = -> (x, y) {x * y}
	end
	def multiline
		x = -> (x) {
			x * x
		}
	end
	def multiline2args
		x = -> (x, y) {
			x * y
		}
	end
	def omit_braces
		x = -> x {
			x * x
		}
	end
	def omit_braces2args
		x = -> x, y {
			x * y
		}
	end
	def argument_nothing
		x = -> {
			x * x
		}
	end
end
