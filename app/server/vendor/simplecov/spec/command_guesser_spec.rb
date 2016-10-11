require "helper"

describe SimpleCov::CommandGuesser do
  subject { SimpleCov::CommandGuesser }
  it 'correctly guesses "Unit Tests" for unit tests' do
    subject.original_run_command = "/some/path/test/units/foo_bar_test.rb"
    expect(subject.guess).to eq("Unit Tests")
    subject.original_run_command = "test/units/foo.rb"
    expect(subject.guess).to eq("Unit Tests")
    subject.original_run_command = "test/foo.rb"
    expect(subject.guess).to eq("Unit Tests")
    subject.original_run_command = "test/{models,helpers,unit}/**/*_test.rb"
    expect(subject.guess).to eq("Unit Tests")
  end

  it 'correctly guesses "Functional Tests" for functional tests' do
    subject.original_run_command = "/some/path/test/functional/foo_bar_controller_test.rb"
    expect(subject.guess).to eq("Functional Tests")
    subject.original_run_command = "test/{controllers,mailers,functional}/**/*_test.rb"
    expect(subject.guess).to eq("Functional Tests")
  end

  it 'correctly guesses "Integration Tests" for integration tests' do
    subject.original_run_command = "/some/path/test/integration/foo_bar_controller_test.rb"
    expect(subject.guess).to eq("Integration Tests")
    subject.original_run_command = "test/integration/**/*_test.rb"
    expect(subject.guess).to eq("Integration Tests")
  end

  it 'correctly guesses "Cucumber Features" for cucumber features' do
    subject.original_run_command = "features"
    expect(subject.guess).to eq("Cucumber Features")
    subject.original_run_command = "cucumber"
    expect(subject.guess).to eq("Cucumber Features")
  end

  it 'correctly guesses "RSpec" for RSpec' do
    subject.original_run_command = "/some/path/spec/foo.rb"
    expect(subject.guess).to eq("RSpec")
  end

  it "defaults to RSpec because RSpec constant is defined" do
    subject.original_run_command = "some_arbitrary_command with arguments"
    expect(subject.guess).to eq("RSpec")
  end
end if SimpleCov.usable?
