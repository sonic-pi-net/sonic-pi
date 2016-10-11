require "spec_helper"

describe FakedProject do
  it "should have added a class method to FakedProject" do
    expect(FakedProject.a_class_method).to eq("this is a mixed-in class method")
  end

  it "should have added a mixed-in instance method to FakedProject" do
    expect(subject.an_instance_method).to eq("this is a mixed-in instance method")
  end

  it "should have added a dyntamically-defined instance method to FakedProject" do
    expect(subject.dynamic).to eq("A dynamically defined instance method")
  end
end
