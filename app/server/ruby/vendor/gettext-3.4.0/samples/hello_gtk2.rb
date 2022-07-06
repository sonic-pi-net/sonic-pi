#!/usr/bin/ruby
# hello_gtk2.rb - sample for Ruby/GTK2
#
# Copyright (C) 2001-2006 Masao Mutoh
# This file is distributed under the same license as gettext.

require 'rubygems'
require 'gettext'
require 'gtk2'

class LocalizedWindow < Gtk::Window
  include GetText

  base_dir = File.dirname(__FILE__)
  bindtextdomain("hello_gtk2",
                 :path => File.join(base_dir, "locale"),
                 :output_charset => "utf-8")

  def initialize
    super
    signal_connect('delete-event') do
      Gtk.main_quit
    end

    add(Gtk::Label.new(_("hello, gtk world")))
  end
end

LocalizedWindow.new.show_all
Gtk.main
