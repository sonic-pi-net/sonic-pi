require "helper"

require "simplecov/formatter/multi_formatter"

describe SimpleCov::Formatter::MultiFormatter do
  describe ".[]" do
    # Regression test for https://github.com/colszowka/simplecov/issues/428
    it "constructs a formatter with multiple children" do
      # Silence deprecation warnings.
      allow(described_class).to receive(:warn)

      children = [
        SimpleCov::Formatter::SimpleFormatter,
        SimpleCov::Formatter::SimpleFormatter,
      ]

      expect(described_class[*children].new.formatters).to eq(children)
    end
  end
end
