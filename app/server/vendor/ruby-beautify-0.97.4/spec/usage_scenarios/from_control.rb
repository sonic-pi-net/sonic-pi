# Test for variable assignment from an if statement
@products = if params[:category]
Category.find(params[:category]).products
else
Product.all
end

# Test for variable assignment from a block
response = begin
if true?
api_call(test)
else
rejected
end
rescue
'FALSE'
end
