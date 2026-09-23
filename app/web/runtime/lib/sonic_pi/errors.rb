# SPDX-License-Identifier: AGPL-3.0-or-later
# The exceptions programs can see, under the names Sonic Pi gives them: a
# trace records the class name, so the namespaces are part of the contract.
module SonicPi
  class Stop < StandardError; end

  # what Time State cannot keep (a lambda, an object): native's SonicPi::Core::NotThreadSafeError (time_state.rb)
  module Core
    class NotThreadSafeError < StandardError; end
  end
  class TraceCutShort < StandardError; end

  # native's language errors (lang/core.rb), in its family: a program that rescues SonicPiError or TimingError
  # catches what it would there
  module Lang
    module Core
      class SonicPiError < StandardError; end
      class AssertionError < SonicPiError; end
      class TimingError < SonicPiError; end
      class ZeroTimeLoopError < TimingError; end
      class NotImmutableError < SonicPiError; end
      class TimeTravelError < SonicPiError; end
      class LiveLockError < SonicPiError; end
      class DeprecationError < SonicPiError; end
      class MapArgError < SonicPiError; end
    end
  end

  # SonicPi::OptError, for an opt given a value its synth does not allow, is declared in validation.rb: it is
  # native's file, and the class is part of what the two runtimes share.

  class InvalidNoteError < ArgumentError; end
  class InvalidOctaveError < ArgumentError; end
  class InvalidScaleError < ArgumentError; end
end
