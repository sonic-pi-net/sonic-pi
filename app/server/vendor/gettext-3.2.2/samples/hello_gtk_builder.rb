#!/usr/bin/env ruby
# hello_gtk_builder.rb - sample for Gtk::Builder in Ruby/GTK3
#
# Copyright (C) 2013 Kouhei Sutou <kou@clear-code.com>
# This file is distributed under the same license as gettext.

require "gtk3"

class HelloGtkBuilder
  def initialize
    domain = "hello_gtk_builder"
    base_dir = File.dirname(__FILE__)
    GLib::GetText.bindtextdomain(domain, File.join(base_dir, "locale"))

    @builder = Gtk::Builder.new
    @builder.translation_domain = domain
    @builder << File.join(base_dir, "hello_gtk_builder.ui")
    @builder.connect_signals do |name|
      method(name)
    end

    @window = @builder["window1"]
    @window.show_all
  end

  def on_quit
    Gtk.main_quit
  end
end

HelloGtkBuilder.new
Gtk.main
