#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013 - 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# What a synth's opt is allowed to be, as data.
#
# A rule is a Hash — {:kind => :max, :max => 131, :incl => false} — and this file is the only place that says whether
# a value keeps a rule (ok?) and the only place that puts a rule into words (message). SynthInfo's v_* helpers build
# their lambdas and their messages from here, so a rule is stated once.
#
# The words are made from the rule, never read back out of it: anything that has only the sentence (a GUI, a
# different runtime) can be given the rules themselves instead.
#
# Plain Ruby on purpose, with nothing required: Sonic Pi's own runtimes are not all MRI, and one of them runs this
# file as it is.

module SonicPi
  # An opt given a value its synth does not allow. It carries the rule it broke, so that whatever shows the error can
  # say it in its own words and offer a value that would do, rather than reading the sentence back apart.
  class OptError < StandardError
    attr_reader :fault
    def initialize(message, fault = nil)
      super(message)
      @fault = fault
    end
  end

  module Validation
    module_function

    # ── the rules ──────────────────────────────────────────────────────────

    # "positive" and "greater than or equal to 0" are the same test but not the same sentence, and Sonic Pi's
    # errors and documentation have always said them differently: the rule keeps them apart
    def positive           = { :kind => :positive, :min => 0, :incl => true }
    def positive_not_zero  = { :kind => :positive, :min => 0, :incl => false }
    def between_inclusive(min, max) = { :kind => :between, :min => min, :max => max, :incl => true }
    def between_exclusive(min, max) = { :kind => :between, :min => min, :max => max, :incl => false }
    def less_than(max)     = { :kind => :max, :max => max, :incl => false }
    def less_than_oet(max) = { :kind => :max, :max => max, :incl => true }
    def greater_than(min)  = { :kind => :min, :min => min, :incl => false }
    def greater_than_oet(min) = { :kind => :min, :min => min, :incl => true }
    def one_of(options)    = { :kind => :one_of, :options => options }
    def not_zero           = { :kind => :not, :value => 0 }
    # a sustain that is either a length or -1 for "hold until told otherwise"
    def positive_or(other) = { :kind => :positive_or, :other => other, :min => 0 }
    # an opt bounded together with another: their sum keeps to a maximum
    def sum_less_than_oet(other, max) = { :kind => :sum_max, :other => other, :max => max, :incl => true }
    # a buffer, its name, or [name, duration]: a shape rather than a number, so it is named and left to the host
    def buffer_like        = { :kind => :buffer_like }

    # ── does a value keep the rule ─────────────────────────────────────────
    #
    # `others` gives the rest of the opts, for a rule that speaks of another (sum_max). A rule about a number, given
    # something that is not one, is not kept: that is what its message is for.

    def ok?(rule, value, others = {})
      case rule[:kind]
      when :min, :positive then number?(value) && (rule[:incl] ? value >= rule[:min] : value > rule[:min])
      when :max      then number?(value) && (rule[:incl] ? value <= rule[:max] : value < rule[:max])
      when :between  then number?(value) && (rule[:incl] ? value >= rule[:min] && value <= rule[:max]
                                                          : value > rule[:min] && value < rule[:max])
      when :one_of   then rule[:options].include?(value)
      when :not      then value != rule[:value]
      when :sum_max  then number?(value) && number?(others[rule[:other]]) &&
                          (rule[:incl] ? value + others[rule[:other]] <= rule[:max] : value + others[rule[:other]] < rule[:max])
      when :positive_or then number?(value) && (value == rule[:other] || value >= rule[:min])
      when :buffer_like then buffer_like?(value)
      else true
      end
    end

    # ── the same rule, in words ────────────────────────────────────────────
    #
    # Sonic Pi says these in its errors and prints them in its documentation, so they are the wording they have
    # always had.

    def message(rule)
      case rule[:kind]
      when :positive then rule[:incl] ? "must be zero or greater" : "must be greater than zero"
      when :min
        rule[:incl] ? "must be a value greater than or equal to #{rule[:min]}" : "must be a value greater than #{rule[:min]}"
      when :max
        rule[:incl] ? "must be a value less than or equal to #{rule[:max]}" : "must be a value less than #{rule[:max]}"
      when :between
        "must be a value between #{rule[:min]} and #{rule[:max]} #{rule[:incl] ? 'inclusively' : 'exclusively'}"
      when :one_of      then "must be one of the following values: #{rule[:options].inspect}"
      when :not         then rule[:value] == 0 ? "must not be zero" : "must not be #{rule[:value]}"
      when :sum_max     then "added to #{rule[:other].to_sym} must be less than or equal to #{rule[:max]}"
      when :positive_or then "must either be a positive value or #{rule[:other]}"
      when :buffer_like then "must be a buffer description, such as a buffer, :foo, \"foo\", or [:foo, 4]"
      else "must be valid"
      end
    end

    # ── the same rule, as the bounds a control can be built from ───────────
    #
    # What the GUI's selectors read (SynthInfo#merge_validation_bounds folds these together): the ends of the range
    # and whether each is included, the values allowed, the value refused.

    def bounds(rule)
      case rule[:kind]
      when :min, :positive then { :min => rule[:min], :min_incl => rule[:incl] }
      when :max      then { :max => rule[:max], :max_incl => rule[:incl] }
      when :between  then { :min => rule[:min], :max => rule[:max], :min_incl => rule[:incl], :max_incl => rule[:incl] }
      when :one_of   then { :options => rule[:options] }
      when :not      then { :exclude => rule[:value] }
      else nil
      end
    end

    # ── the whole sentence ─────────────────────────────────────────────────
    #
    # What Sonic Pi says when a value does not keep its rule, said in one place so that every runtime says it the
    # same way. Takes a rule, or the sentence a rule already made.

    def error_message(opt, rule_or_message, value)
      said = rule_or_message.is_a?(Hash) ? message(rule_or_message) : rule_or_message
      "Value of opt #{opt.to_sym.inspect} #{said}, got #{value.inspect}."
    end

    # The error itself, for whoever is doing the checking: one class and one sentence, whichever runtime raises it,
    # with the rule and the value alongside for an error card to build on.
    def error(opt, rule_or_message, value, fault = {})
      rule = rule_or_message.is_a?(Hash) ? rule_or_message : nil
      SonicPi::OptError.new(error_message(opt, rule_or_message, value),
                            { :opt => opt.to_sym, :value => value, :rule => rule }.merge(fault))
    end

    def number?(value) = value.is_a?(Numeric)

    # a buffer, its name, or a pair of name and duration ([:foo, 4]) — a list of any kind, so it is asked what it
    # can do rather than what it is: Sonic Pi's rings and vectors are lists as much as an Array is
    def buffer_like?(value)
      return true if defined?(SonicPi::Buffer) && value.is_a?(SonicPi::Buffer)
      return true if value.is_a?(String) || value.is_a?(Symbol)
      return false unless value.respond_to?(:size) && value.respond_to?(:[]) && !value.is_a?(Hash)
      value.size == 2 && (value[0].is_a?(Symbol) || value[0].is_a?(String)) && value[1].is_a?(Numeric)
    end
  end
end
