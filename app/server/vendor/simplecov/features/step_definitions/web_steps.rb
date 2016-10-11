module WithinHelpers
  def with_scope(locator)
    locator ? within(locator) { yield } : yield
  end
end
World(WithinHelpers)

When /^I open the coverage report$/ do
  visit "/"
end

Given /^(?:|I )am on (.+)$/ do |path|
  visit path
end

When /^(?:|I )go to (.+)$/ do |path|
  visit path
end

When /^(?:|I )press "([^"]*)"(?: within "([^"]*)")?$/ do |button, selector|
  with_scope(selector) do
    click_button(button)
  end
end

When /^(?:|I )follow "([^"]*)"(?: within "([^"]*)")?$/ do |link, selector|
  with_scope(selector) do
    click_link(link)
  end
end

Then /^(?:|I )should see "([^"]*)"(?: within "([^"]*)")?$/ do |text, selector|
  with_scope(selector) do
    expect(page).to have_content(text)
  end
end

Then /^(?:|I )should see \/([^\/]*)\/(?: within "([^"]*)")?$/ do |regexp, selector|
  regexp = Regexp.new(regexp)
  with_scope(selector) do
    expect(page).to have_xpath("//*", :text => regexp)
  end
end

Then /^(?:|I )should not see "([^"]*)"(?: within "([^"]*)")?$/ do |text, selector|
  with_scope(selector) do
    expect(page).to have_no_content(text)
  end
end

Then /^(?:|I )should not see \/([^\/]*)\/(?: within "([^"]*)")?$/ do |regexp, selector|
  regexp = Regexp.new(regexp)
  with_scope(selector) do
    expect(page).to have_no_xpath("//*", :text => regexp)
  end
end

Then /^show me the page$/ do
  save_and_open_page # rubocop:disable Lint/Debugger
end

Then /^print the page$/ do
  puts page.body
end
