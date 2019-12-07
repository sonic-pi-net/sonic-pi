module Parser
  # Parser metadata
  module Meta

    # All node types that parser can produce. Not all parser versions
    # will be able to produce every possible node.
    NODE_TYPES =
      %w(
        true false nil int float str dstr str
        sym dsym xstr regopt regexp array splat
        array pair kwsplat hash irange erange self
        lvar ivar cvar gvar const defined? lvasgn
        ivasgn cvasgn gvasgn casgn mlhs masgn op_asgn
        op_asgn and_asgn ensure rescue arg_expr
        or_asgn and_asgn or_asgn back_ref nth_ref
        match_with_lvasgn match_current_line
        module class sclass def defs undef alias args
        cbase arg optarg restarg blockarg block_pass args def kwarg kwoptarg
        kwrestarg send super zsuper yield block send
        and not or if when case while until while_post
        until_post for break next redo return resbody
        kwbegin begin retry preexe postexe iflipflop eflipflop
        shadowarg complex rational __FILE__ __LINE__
      ).map(&:to_sym).to_set.freeze

  end # Meta
end # Parser
