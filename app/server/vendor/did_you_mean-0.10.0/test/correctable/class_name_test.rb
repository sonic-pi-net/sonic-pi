require 'test_helper'

module ACRONYM
end

class Project
  def self.bo0k
    Bo0k
  end
end

class Book
  class TableOfContents; end

  def tableof_contents
    TableofContents
  end

  class Page
    def tableof_contents
      TableofContents
    end

    def self.tableof_contents
      TableofContents
    end
  end
end

class ClassNameTest < Minitest::Test
  def test_suggestions
    error = assert_raises(NameError) { ::Bo0k }
    assert_suggestion "Book", error.suggestions
  end

  def test_suggestions_include_case_specific_class_name
    error = assert_raises(NameError) { ::Acronym }
    assert_suggestion "ACRONYM", error.suggestions
  end

  def test_suggestions_include_top_level_class_name
    error = assert_raises(NameError) { Project.bo0k }
    assert_suggestion "Book", error.suggestions
  end

  def test_names_in_suggestions_have_namespaces
    error = assert_raises(NameError) { ::Book::TableofContents }
    assert_suggestion "Book::TableOfContents", error.suggestions
  end

  def test_suggestions_searches_for_names_in_upper_level_scopes
    error = assert_raises(NameError) { Book::Page.tableof_contents }
    assert_suggestion "Book::TableOfContents", error.suggestions
  end

  def test_suggestions_should_work_from_within_instance_method
    error = assert_raises(NameError) { ::Book.new.tableof_contents }
    assert_suggestion "Book::TableOfContents", error.suggestions
  end

  def test_suggestions_should_work_from_within_instance_method_on_nested_class
    error = assert_raises(NameError) { ::Book::Page.new.tableof_contents }
    assert_suggestion "Book::TableOfContents", error.suggestions
  end
end
