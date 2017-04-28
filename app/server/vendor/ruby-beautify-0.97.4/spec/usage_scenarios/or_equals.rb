# Test or-equals
def expensive_value
@expensive_value ||= begin
if conn.active?
expensive_call
else
:none
end
end
end

# Test multiple or-equals
def expensive_value
@expensive_value ||= @cached_value ||= begin
if conn.active?
expensive_call
else
:none
end
end
end
