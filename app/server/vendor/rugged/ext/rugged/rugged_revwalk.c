/*
 * The MIT License
 *
 * Copyright (c) 2014 GitHub, Inc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
VALUE rb_cRuggedWalker;

static void rb_git_walk__free(git_revwalk *walk)
{
	git_revwalk_free(walk);
}

VALUE rugged_walker_new(VALUE klass, VALUE owner, git_revwalk *walk)
{
	VALUE rb_walk = Data_Wrap_Struct(klass, NULL, &rb_git_walk__free, walk);
	rugged_set_owner(rb_walk, owner);
	return rb_walk;
}

/*
 *  call-seq:
 *    Walker.new(repository) -> walker
 *
 *  Create a new +Walker+ instance able to walk commits found
 *  in +repository+, which is a <tt>Rugged::Repository</tt> instance.
 */
static VALUE rb_git_walker_new(VALUE klass, VALUE rb_repo)
{
	git_repository *repo;
	git_revwalk *walk;
	int error;

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_revwalk_new(&walk, repo);
	rugged_exception_check(error);

	return rugged_walker_new(klass, rb_repo, walk);;
}

/*
 *  call-seq:
 *    walker.each { |commit| block }
 *    walker.each -> Iterator
 *
 *  Perform the walk through the repository, yielding each
 *  one of the commits found as a <tt>Rugged::Commit</tt> instance
 *  to +block+.
 *
 *  If no +block+ is given, an +Iterator+ will be returned.
 *
 *  The walker must have been previously set-up before a walk can be performed
 *  (i.e. at least one commit must have been pushed).
 *
 *    walker.push("92b22bbcb37caf4f6f53d30292169e84f5e4283b")
 *    walker.each { |commit| puts commit.oid }
 *
 *  generates:
 *
 *    92b22bbcb37caf4f6f53d30292169e84f5e4283b
 *    6b750d5800439b502de669465b385e5f469c78b6
 *    ef9207141549f4ffcd3c4597e270d32e10d0a6bc
 *    cb75e05f0f8ac3407fb3bd0ebd5ff07573b16c9f
 *    ...
 */
static VALUE rb_git_walker_each(VALUE self)
{
	git_revwalk *walk;
	git_commit *commit;
	git_repository *repo;
	git_oid commit_oid;
	int error;

	Data_Get_Struct(self, git_revwalk, walk);
	repo = git_revwalk_repository(walk);

	if (!rb_block_given_p())
		return rb_funcall(self, rb_intern("to_enum"), 0);

	while ((error = git_revwalk_next(&commit_oid, walk)) == 0) {
		error = git_commit_lookup(&commit, repo, &commit_oid);
		rugged_exception_check(error);

		rb_yield(rugged_object_new(rugged_owner(self), (git_object *)commit));
	}

	if (error != GIT_ITEROVER)
		rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    walker.push(commit) -> nil
 *
 *  Push one new +commit+ to start the walk from. +commit+ must be a
 *  +String+ with the OID of a commit in the repository, or a <tt>Rugged::Commit</tt>
 *  instance.
 *
 *  More than one commit may be pushed to the walker (to walk several
 *  branches simulataneously).
 *
 *  Duplicate pushed commits will be ignored; at least one commit must have been
 *  pushed as a starting point before the walk can begin.
 *
 *    walker.push("92b22bbcb37caf4f6f53d30292169e84f5e4283b")
 */
static VALUE rb_git_walker_push(VALUE self, VALUE rb_commit)
{
	git_revwalk *walk;
	git_commit *commit;
	int error;

	Data_Get_Struct(self, git_revwalk, walk);

	commit = (git_commit *)rugged_object_get(
		git_revwalk_repository(walk), rb_commit, GIT_OBJ_COMMIT);

	error = git_revwalk_push(walk, git_object_id((git_object *)commit));

	git_commit_free(commit);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    walker.hide(commit) -> nil
 *
 *  Hide the given +commit+ (and all its parents) from the
 *  output in the revision walk.
 */
static VALUE rb_git_walker_hide(VALUE self, VALUE rb_commit)
{
	git_revwalk *walk;
	git_commit *commit;
	int error;

	Data_Get_Struct(self, git_revwalk, walk);

	commit = (git_commit *)rugged_object_get(
		git_revwalk_repository(walk), rb_commit, GIT_OBJ_COMMIT);

	error = git_revwalk_hide(walk, git_object_id((git_object *)commit));

	git_commit_free(commit);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    walker.sorting(sort_mode) -> nil
 *
 *  Change the sorting mode for the revision walk.
 *
 *  This will cause +walker+ to be reset.
 */
static VALUE rb_git_walker_sorting(VALUE self, VALUE ruby_sort_mode)
{
	git_revwalk *walk;
	Data_Get_Struct(self, git_revwalk, walk);
	git_revwalk_sorting(walk, FIX2INT(ruby_sort_mode));
	return Qnil;
}

/*
 *  call-seq:
 *    walker.simplify_first_parent() -> nil
 *
 *  Simplify the walk to the first parent of each commit.
 */
static VALUE rb_git_walker_simplify_first_parent(VALUE self)
{
	git_revwalk *walk;
	Data_Get_Struct(self, git_revwalk, walk);
	git_revwalk_simplify_first_parent(walk);
	return Qnil;
}

/*
 *  call-seq:
 *    walker.reset -> nil
 *
 *  Remove all pushed and hidden commits and reset the +walker+
 *  back into a blank state.
 */
static VALUE rb_git_walker_reset(VALUE self)
{
	git_revwalk *walk;
	Data_Get_Struct(self, git_revwalk, walk);
	git_revwalk_reset(walk);
	return Qnil;
}

void Init_rugged_revwalk(void)
{
	rb_cRuggedWalker = rb_define_class_under(rb_mRugged, "Walker", rb_cObject);
	rb_define_singleton_method(rb_cRuggedWalker, "new", rb_git_walker_new, 1);

	rb_define_method(rb_cRuggedWalker, "push", rb_git_walker_push, 1);
	rb_define_method(rb_cRuggedWalker, "each", rb_git_walker_each, 0);
	rb_define_method(rb_cRuggedWalker, "walk", rb_git_walker_each, 0);
	rb_define_method(rb_cRuggedWalker, "hide", rb_git_walker_hide, 1);
	rb_define_method(rb_cRuggedWalker, "reset", rb_git_walker_reset, 0);
	rb_define_method(rb_cRuggedWalker, "sorting", rb_git_walker_sorting, 1);
	rb_define_method(rb_cRuggedWalker, "simplify_first_parent", rb_git_walker_simplify_first_parent, 0);
}
