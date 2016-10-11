Given /^I want to keep stuff simple$/ do
  expect(1).to eq(1)
end

When /^I write my cukes for the fake project$/ do
  expect(1).to eq(1)
end

Then /^I make all neccessary tests in a single step$/ do
  expect(FakedProject.foo).to eq("bar")

  expect(FrameworkSpecific.cucumber).to eq("Only tested in Cucumber")

  expect(FakedProject.a_class_method).to eq("this is a mixed-in class method")

  expect(FakedProject.new.an_instance_method).to eq("this is a mixed-in instance method")
  expect(FakedProject.new.dynamic).to eq("A dynamically defined instance method")

  something = SomeClass.new("foo")
  expect(something.reverse).to eq("oof")
  expect(something.compare_with("foo")).to be true
end
