# -*- coding: utf-8 -*-
#
# Copyright (C) 2012-2014  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
#
# License: Ruby's or LGPL
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "gettext/po_entry"

module GetText

  # PO stores PO entries like Hash. Each key of {POEntry} is msgctxt
  # and msgid.
  # PO[msgctxt, msgid] returns the {POEntry} containing msgctxt and
  # msgid.
  # If you specify msgid only, msgctxt is treated as nonexistent.
  #
  # @since 2.3.4
  class PO
    include Enumerable

    class NonExistentEntryError < StandardError
    end

    # @!attribute [rw] order
    #   The order is used to sort PO entries(objects of {POEntry}) in
    #   {#to_s}.
    #   @param [:reference, :msgid] order (:reference) The sort key.
    #
    #     Use `:reference` for sorting by location that message is placed.
    #
    #     Use `:msgid` for sorting by msgid alphabetical order.
    #
    #     `:references` is deprecated since 3.0.4. It will be removed
    #     at 4.0.0. Use `:reference` instead.
    #
    #   @return [Symbol] the name as order by sort.
    attr_accessor :order

    def initialize(order=nil)
      @order = order || :reference
      @entries = {}
    end

    # Returns {POEntry} containing msgctxt and msgid.
    # If you specify one argument, it is treated as msgid.
    # @overload [](msgid)
    #   @!macro [new] po.[].argument
    #     @param [String] msgid msgid contained returning {POEntry}.
    #     @return [POEntry]
    #   @!macro po.[].argument
    # @overload [](msgctxt, msgid)
    #   @param [String] msgctxt msgctxt contained returning {POEntry}.
    #   @!macro po.[].argument
    def [](msgctxt, msgid=nil)
      if msgid.nil?
        msgid = msgctxt
        msgctxt = nil
      end

      @entries[[msgctxt, msgid]]
    end

    # Stores {POEntry} or msgstr binding msgctxt and msgid. If you
    # specify msgstr, this method creates {POEntry} containing it.
    # If you specify the two argument, the first argument is treated
    # as msgid.
    #
    # @overload []=(msgid, po_entry)
    #   @!macro [new] po.store.entry.arguments
    #     @param [String] msgid msgid binded {POEntry}.
    #     @param [POEntry] po_entry stored {POEntry}.
    #   @!macro po.store.entry.arguments
    # @overload []=(msgctxt, msgid, po_entry)
    #   @param [String] msgctxt msgctxt binded {POEntry}.
    #   @!macro po.store.entry.arguments
    # @overload []=(msgid, msgstr)
    #   @!macro [new] po.store.msgstr.arguments
    #     @param [String] msgid msgid binded {POEntry}.
    #     @param [String] msgstr msgstr contained {POEntry} stored PO.
    #       This {POEntry} is generated in this method.
    #   @!macro po.store.msgstr.arguments
    # @overload []=(msgctxt, msgid, msgstr)
    #   @param [String] msgctxt msgctxt binded {POEntry}.
    #   @!macro po.store.msgstr.arguments
    def []=(*arguments)
      case arguments.size
      when 2
        msgctxt = nil
        msgid = arguments[0]
        value = arguments[1]
      when 3
        msgctxt = arguments[0]
        msgid = arguments[1]
        value = arguments[2]
      else
        raise(ArgumentError,
              "[]=: wrong number of arguments(#{arguments.size} for 2..3)")
      end

      id = [msgctxt, msgid]
      if value.instance_of?(POEntry)
        @entries[id] = value
        return(value)
      end

      msgstr = value
      if @entries.has_key?(id)
        entry = @entries[id]
      else
        if msgctxt.nil?
          entry = POEntry.new(:normal)
        else
          entry = POEntry.new(:msgctxt)
        end
        @entries[id] = entry
      end
      entry.msgctxt = msgctxt
      entry.msgid = msgid
      entry.msgstr = msgstr
      entry
    end

    # Returns if PO stores {POEntry} containing msgctxt and msgid.
    # If you specify one argument, it is treated as msgid and msgctxt
    # is nil.
    #
    # @overload has_key?(msgid)
    #   @!macro [new] po.has_key?.arguments
    #     @param [String] msgid msgid contained {POEntry} checked if it be
    #       stored PO.
    #   @!macro po.has_key?.arguments
    # @overload has_key?(msgctxt, msgid)
    #   @param [String] msgctxt msgctxt contained {POEntry} checked if
    #     it be stored PO.
    #   @!macro po.has_key?.arguments
    def has_key?(*arguments)
      case arguments.size
      when 1
        msgctxt = nil
        msgid = arguments[0]
      when 2
        msgctxt = arguments[0]
        msgid = arguments[1]
      else
        message = "has_key?: wrong number of arguments " +
                    "(#{arguments.size} for 1..2)"
        raise(ArgumentError, message)
      end
      id = [msgctxt, msgid]
      @entries.has_key?(id)
    end

    # Calls block once for each {POEntry} as a block parameter.
    # @overload each(&block)
    #   @yield [entry]
    #   @yieldparam [POEntry] entry {POEntry} in PO.
    # @overload each
    #   @return [Enumerator] Returns Enumerator for {POEntry}.
    def each(&block)
      @entries.each_value(&block)
    end

    # @return [Bool] `true` if there is no entry, `false` otherwise.
    def empty?
      @entries.empty?
    end

    # For {PoParer}.
    def set_comment(msgid, comment, msgctxt=nil)
      id = [msgctxt, msgid]
      self[*id] = nil unless @entries.has_key?(id)
      self[*id].comment = comment
    end

    # Formats each {POEntry} to the format of PO files and returns joined
    # them.
    # @see http://www.gnu.org/software/gettext/manual/html_node/PO-Files.html#PO-Files
    #   The description for Format of PO in GNU gettext manual
    # @param (see POEntry#to_s)
    # @return [String] Formatted and joined PO entries. It is used for
    #   creating .po files.
    def to_s(options={})
      po_string = ""

      header_entry = @entries[[nil, ""]]
      unless header_entry.nil?
        po_string << header_entry.to_s(options.merge(:max_line_width => nil))
      end

      content_entries = @entries.reject do |(_, msgid), _|
        msgid == :last or msgid.empty?
      end

      sort(content_entries).each do |msgid, entry|
        po_string << "\n" unless po_string.empty?
        po_string << entry.to_s(options)
      end

      if @entries.has_key?([nil, :last])
        po_string << "\n" unless po_string.empty?
        po_string << @entries[[nil, :last]].to_s(options)
      end

      po_string
    end

    private
    def sort(entries)
      case @order
      when :reference, :references # :references is deprecated.
        sorted_entries = sort_by_reference(entries)
      when :msgid
        sorted_entries = sort_by_msgid(entries)
      else
        sorted_entries = entries.to_a
      end
    end

    def sort_by_reference(entries)
      entries.each do |_, entry|
        entry.references = entry.references.sort do |reference, other|
          compare_reference(reference, other)
        end
      end

      entries.sort do |msgid_entry, other_msgid_entry|
        # msgid_entry = [[msgctxt, msgid], POEntry]
        entry_first_reference = msgid_entry[1].references.first
        other_first_reference = other_msgid_entry[1].references.first
        compare_reference(entry_first_reference, other_first_reference)
      end
    end

    def compare_reference(reference, other)
      entry_source, entry_line_number = split_reference(reference)
      other_source, other_line_number = split_reference(other)

      if entry_source != other_source
        entry_source <=> other_source
      else
        entry_line_number <=> other_line_number
      end
    end

    def split_reference(reference)
      return ["", -1] if reference.nil?
      if /\A(.+):(\d+?)\z/ =~ reference
        [$1, $2.to_i]
      else
        [reference, -1]
      end
    end

    def sort_by_msgid(entries)
      entries.sort_by do |msgid_entry|
        # msgid_entry = [[msgctxt, msgid], POEntry]
        msgid_entry[0]
      end
    end
  end
end
