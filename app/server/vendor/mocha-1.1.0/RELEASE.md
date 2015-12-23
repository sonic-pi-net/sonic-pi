# Release Notes

## 1.1.0

* Set visibility of any instance stub method.
* Stub methods with a prepended method if there are other prepended methods. Thanks to @mrsimo.
* Improve docs for `Mock#responds_like` & `#responds_like_instance_of`.
* Use GitHub convention for instructions on contributing to Mocha.
* Fix typos in docs. Thanks to @10io

## 1.0.0

### External changes
* Assume 'mocha' has been required when requiring 'mocha/setup'.
* Provide shortcuts for integrating with specific test library i.e. `require 'mocha/test_unit'` or `require 'mocha/mini_test'`
as alternatives to `require 'mocha/setup'`.
* Do not automatically try to integrate with test libraries. Since the automatic test library integration functionality
requires the test library to be loaded and this doesn't usually happen until *after* the bundle is loaded, it makes things
simpler if we use `require 'mocha/setup'` to explicitly setup Mocha when we know the test library has been loaded. Fixes #146 & #155.
* Consider stubs on superclasses if none exist on primary receiver. Largely based on changes suggested by @ccutrer in #145.
Note: this may break existing tests which rely on the old behaviour. Stubbing a superclass method and then invoking that
method on a child class would previously cause an unexpected invocation error. By searching up through the inheritance
hierarchy for each of the delegate mock objects, we can provide more intuitive behaviour. Instead of an unexpected invocation
error, invoking the method on the child class will cause the stubbed method on the superclass to be used.
* Avoid recursion when constructing unexpected invocation message. Fixes #168.
* Add explanation of method dispatch. Heavily based on the relevant jMock v1 documentation. Fixes #172.
* Make class_eval line number more accurate. This sets the line number as the line number of the `def` statement. Closes #169.
* Allow nesting of `responds_with` parameter matcher. Closes #166.
* Define `Mocha` module before it's referenced. The test helper defines a class `TestCase` within the `Mocha` module. When
running the tests inside the bundle, the `Mocha` module happens to be defined at this point. However when running the tests outside the bundle, it is not defined and so an exception is raised: `uninitialized constant Mocha (NameError)`. Fixes #163.
* Document lack of thread-safety. Fixes #154.
* Document how to use the build-matrix script. Fixes #160.
* Stubbing non-public method should use same visibility. This will probably break some existing tests that were somehow relying
on the stubbed method being public while the original method was protected or private. Fixes #150.

### Internal changes
* Use lastest Rubygems in Travis CI builds.
* Run the standard test suite against Ruby 2.1.0 in the build matrix.
* Run integration tests against Ruby 2.0.0 with latest Test::Unit gem in the build matrix.
* Test::Unit is not available in Ruby v1.9.3 standard library, so remove it from the build matrix.
* Force use of Test::Unit runner, etc in relevant integration tests. Prior to this, I don't think we were really testing the
Mocha integration with Test::Unit much, because, although `TestUnitTest` was a subclass of `Test::Unit::TestCase`, the
important test case instances are the temporary ones built by `TestRunner#run_as_test` et al. Prior to this change, these
would only have used Test::Unit where MiniTest was not available *at all* i.e. only in early versions of Ruby and when the
MiniTest gem was not loaded.
* Reset environment variables between build matrix builds.
* Only activate integration with relevant test library for each of the integration tests.
* Include standard build combinations from Travis CI config i.e. builds using standard library versions of test libraries.
* Fix `build-matrix.rb` script. Also use `.travis.yml` to decide what combinations to run. This means we
can now simulate the Travis CI build locally and avoid duplication. Fixes #157.
* Remove Ruby version map from build matrix script. I'm using the `rbenv-aliases` plugin to alias minor versions to the
relevant patch version.

## 0.14.0

* Official support for MiniTest v5. All tests now pass on the continuous integration build.

## 0.14.0.alpha

* Add speculative support for Minitest v5. Due to incompatibilities it has not yet been possible to run the Mocha test suite against Minitest v5. However, @zenspider (author of Minitest) provided the patch and he has tested it against Rails v4. Fixes #156. Thanks to @zenspider.
* Documentation updates.

## 0.13.3
* Allow `Mocha::ParameterMatchers#includes` to accept multiple items. Thanks to @simao.
* Allow stubbing of *private* `Kernel` methods. Fixes #134. Thanks to @camski for reporting.
* Avoid a warning when `test/unit/version` is required by other libraries in the same project. Fixes #140. Thanks to @tmiller.
* Make auto-activation of Test::Unit integration more resilient. This change is specifically to cope with the nasty re-defining of classes that is done by the `minitest-spec-rails` gem. Fixes #143. Thanks to @tubaxenor for reporting.
* Safer restoration of stubbed method visibility. Fixes #141. Thanks to @tmm1.
* Ensure `Mockery` instance gets reset even if exception raised. Fixes #144.
* Adapt Mocha acceptance tests to cope with changes in output from latest (v4.6.2) of MiniTest.
* Updates to README about Rails compatibility.

## 0.13.2
* Stubbing of methods re-declared with different visibilty. Fixes #109.
* Add `Mock#responds_like_instance_of`. Fixes #119.
* Make `Expectation#inspect` less verbose and more useful. Fixes #122.
* Make unit tests more robust to changes in environment. Fixes #121.
* Update README in an attempt to head Rails-related issues off at the pass.
* Add a Gem Badge to provide a link to Mocha on Rubygems.
* Make documentation example consistent with other examples.

## 0.13.1
* Fix #97 - `Mocha::ParameterMatchers#has_entry` does not work with an Array as the entry's value. Thanks to @ngokli.
* Allow deprecation `:debug` mode to be switched on from `MOCHA_OPTIONS` environment variable.

## 0.13.0
* Major overhaul of MiniTest & Test::Unit integration. Mocha now integrates with later versions of the two test libraries using documented hooks rather than monkey-patching. This should mean that Mocha will integrate with new versions of either library without the need to release a new version of Mocha each time, which was clearly bad and unsustainable. Many thanks to @tenderlove, @zenspider & @kou for their help, suggestions & patience.
* Temporarily deprecated `require 'mocha'`. Use `require 'mocha/setup'` instead. The plan is that eventually `require 'mocha'` will *not* automatically integrate with either of the two test libraries as it does at the moment, and you'll need to explicitly & separately trigger the integration. I think this will provide a lot more flexibility and will hopefully do away with the need for the `require: false` option in the `Gemfile` which has always confused people.
* Deprecated `require 'mocha_standalone'` and `require 'mocha/standalone'`. Use `require 'mocha/api` instead.
* Although these are not part of Mocha's public API, I thought I should mention that the MiniTest and Test::Unit assertion counter classes have been combined into a single class `Mocha::Integration::AssertionCounter`.
* Extracted Mocha::Hooks module from Mocha::API and added documentation for test library authors.
* Improvements to documentation. Much of it has been combined into the README file.
* Fix #101 - Mock#respond_to? doesn't work with a string argument - thanks to @urbanautomaton.
* Fix #105 - Travis link in README - thanks to @cknadler.
* Various improvements to automated testing of integration with test libraries.
* Make deprecation warnings more prominent.

## 0.12.7
* Officially support minitest v4.1.0 (still monkey-patching).

## 0.12.6
* Fixes #103.

## 0.12.5
* Officially support minitest v3.5.0 (still monkey-patching).

## 0.12.4
* Officially support minitest v3.4.0 & test-unit v2.5.2 (still monkey-patching).

## 0.12.3
* Revert rename of undocumented internal module since it turns out Rails/ActiveSupport is relying on its existence.

## 0.12.2
* Officially support minitest v3.3.0 (still monkey-patching)

## 0.12.1
* Deprecation warning (instead of fail fast) if neither Test::Unit nor MiniTest is loaded. Fixes #88.
* Remove deprecated access to `Mocha::Standalone`.
* Remove the deprecated file `stubba.rb`.
* Officially support test-unit v2.5.1 (still monkey-patching).
* Improve the API acceptance test.

## 0.12.0
* Fail fast if neither Test::Unit nor MiniTest is loaded. Fixes #40.
* Officially support MiniTest up to v3.2.0 (still monkey-patching).
* Officially support test-unit v2.5.0 (still monkey-patching).
* Do not monkey-patch Test::Unit or MiniTest unless we *know* it's ok.
* Add acceptance tests to demonstrate using a block as a custom parameter matcher.
* Update Travis CI build status image to use the new build under the freerange account.

## 0.11.4
* Homepage has moved to http://gofreerange.com/mocha/docs.

## 0.11.3
* Fix for #78 i.e. alias Object#method as Object#_method, not Object#__method__ which already exists as another Ruby method.

## 0.11.2
* Rails has a Request class which defines its own #method method. This broke the new mechanism for stubbing a method. This release includes a slightly modified version of fix #77 provided by @sikachu. See https://github.com/rails/rails/pull/5907 for further info.

## 0.11.1
* In Ruby 1.8.7 methods accepting a block parameter were incorrectly restored without the block parameter after being stubbed. Fix for #76.

## 0.11.0
* Store original method when stubbing rather than using alias_method. This fixes #41, #47, #74 and all tests now pass on both Ruby 1.8.7 and 1.9.3.
* Attempting to stub a method on a frozen object should fail fast. See #68.
* Prevent stubbing a method on nil by default. See #68.
* Generate documentation using YARD instead of Rdoc - removes dependency on Coderay.
* Publish documentation on Github pages instead of Rubyforge - uses rake task written by @tomafro.
* Remove agiledox which has outlived it's usefulness.
* Removed trailing whitespace throughout codebase.
* Add documentation for Mock#unstub.
* Improve documentation for ObjectMethods.
* Provide a way to run multiple tests within a single acceptance test method.

## 0.10.5
* Fix for issue #66 (hopefully without regressing on issue #63) - Mocha::Mock has Mocha::Mockery as a dependency. Stop trying to pretend otherwise. Thanks to @kennyj for reporting.
* Fix a bunch of warnings in Ruby 1.9. There are still the 6 test failures mentioned in issue #41 which I suspect are due to the introspection gem not being Ruby 1.9-compatible.
* Add links to README for source code & issue tracker.
* Fix for issue #67 - Make the travis-ci badge visible in the README. Thanks to Diego Plentz for pull request.
* Fix for issue #70 - Rename Mock#expectations to Mock#__expectations__ to avoid conflicts. Thanks to Jeremy Stephens for pull request.

## 0.10.4
* Fix for issue #65 - expectations not being verified in subsequent tests.
* Fix for issue #63 - require Mocha::Mockery at Mocha::Mock class load time and not on invocation of Mock#method_missing.
* Fix for issue #45 - raise ArgumentError if Mocha::ParameterMatchers#has_entry is given
Hash with wrong number of entries.
* Make global variable name more obscure to avoid clashes with other libraries.
* Move travis-ci-related gemfiles into their own directory.

## 0.10.3
* Fix for issue #57. Gem::Requirement#=~ was only added in rubygems v1.8.0, but Object#=~ means the result of various monkey-patching checks is always false/nil for earlier versions of rubygems. However, the method it aliases #satisfied_by? has existed since Gem::Dependency was extracted from Gem::Version in rubygems v0.9.4.4, so it's much safer to use that. Thanks to fguillen for reporting and helping with diagnosis.

## 0.10.2
* Merge pull request #53. Unstubbing a method should not remove expectations for other stubbed methods. Fixes #52. Thanks to saikat.

## 0.10.1
* Merge pull request #51. Use Gem::Requirement & Gem::Version for version comparison. Fixes issue #50. Thanks to meineerde.
* Fixed typo in rdoc for Mocha::ObjectMethods.
* Improve README as suggested in issue #46. Explain that Mocha must be loaded after test libraries and how to achieve this using Bundler.
* Merge pull request #43 - nobody expects the spanish inquisition! Thanks to cairo140.
* Fix for issue #39 - improve documentation for Expectation#multiple_yields.
* Fix for issue #38 where a subtle change in test-unit v2.3.0 had been missed - only visible in verbose mode.
* Support for MiniTest up to v2.6.2 has been verified.
* Add explicit development dependency on coderay for generating syntax-highlighted code examples.

## 0.10.0
* Add Expectation#throws to allow a stubbed method to use Kernel#throw.
* Updates for versions of Test::Unit up to and including v2.3.3 (including patch by Jens Fahnenbruck).
* Updates for versions of MiniTest up to and including v2.5.1.
* Since the singleton method added by Mocha masks the underlying instance method, there's no need to move it out the way and then back again. This fixes Github issue #20, because the original method is left unchanged - https://github.com/floehopper/mocha/issues/20 (thanks to Nick Lewis).
* Handle stubbing of a singleton method, leaving the original method unchanged after the test.
* When stubbing an instance method that was originally defined as a singleton method, the original method should still exist after the test.
* Fixed mis-print in Mocha::ObjectMethods#unstub documentation (patch by Gleb Pomykalov).
* Improved test coverage around stubbing of methods defined in different ways - this makes use of the newly extracted introspection gem (although this means some tests are now failing in Ruby v1.9.2).
* Added configuration for Travis continuous integration.
* Make the gemspec the canonical reference and stop generating it from the Rakefile.
* Use the built-in Bundler rake tasks for packaging the gem.
* Use the "release" rake task provided by Bundler instead of using the Rake::XForge::Release functionality.
* Extract Object#__metaclass__ into a new metaclass gem.
* Run rake tasks without `bundle exec`.
* Avoid deprecation warning for rdoc rake task.
* Remove the `use_test_unit_gem` MOCHA_OPTION which hasn't worked since we switched to bundler - we can now run the tests specifying a different Gemfile instead.
* Use multiple Gemfiles seems to run Travis CI builds against multiple version of test-unit & minitest.

## 0.9.12
* Make Mocha's tests pass under Ruby 1.9.2 i.e. using MiniTest. One of the main issues was that we were not parsing stacktraces on MiniTest errors comprehensively enough.
* Avoid 'circular require considered harmful' warning when running Mocha's tests in Ruby 1.9.2
* Make performance tests work on Ruby 1.9.2 i.e. using MiniTest.
* Declare rake as a *development* dependency with newer versions of Rubygems since it's only needed to carry out developer-related tasks.

## 0.9.11
* Added explicit support for minitest v1.5.0 to v2.0.2.
* Make testable by rubygems-test.
* Update links to my blog and make other links consistent.
* Added a URI parameter matcher that ignores the order of query parameters so that tests can be independent of undefined hash ordering (patch by Paul Battley).
* Include unexpected invocation in failure message and change the language slightly to make the failure message less confusing. See http://floehopper.lighthouseapp.com/projects/22289/tickets/52.
* No need to create regular expression every time the BacktraceFilter#filtered method is called. See http://floehopper.lighthouseapp.com/projects/22289-mocha/tickets/66.

## 0.9.10
* Added Mocha::ObjectMethods#unstub method - https://github.com/floehopper/mocha/issues#issue/6
* Inherit Mocha::ExpectationError from Exception instead of StandardError to reduce the chances of a test passing by accident - thanks to James Sanders (jsanders) - https://github.com/floehopper/mocha/issues#issue/15
* Fixed bug - GitHub README page to link correctly to code examples - https://github.com/floehopper/mocha/issues/closed#issue/11
* Fixed bug - PASSTHROUGH_EXCEPTIONS are defined on MiniTest::Unit::TestCase not in Mocha - thanks to Brian Troutwine (blt) - https://github.com/floehopper/mocha/issues/closed#issue/14

## 0.9.9
* Avoid loading bits of the test-unit gem by accident. This is an attempt at a fix for the problem that James Adam reported [1]. By using 'load' instead of 'require' to detect the version of Test::Unit, we can avoid rubygems trying to load bits of the test-unit gem when it's not wanted. [1] http://floehopper.lighthouseapp.com/projects/22289-mocha/tickets/50#ticket-50-13
* Fix exception when running rake without test-unit gem. When test-unit gem >=v2.0.0 was installed but the "use_test_unit_gem" MOCHA_OPTIONS was not specified, a "comparison of Fixnum with Hash failed" exception was being raised when running the performance tests. This was because bits of the test-unit gem were being loaded accidentally and a Hash was being incorrectly supplied to the TestRunner.run method.
* Explicitly require rubygems for running tests via rake using test-unit gem.
* Handle newer versions of test-unit gem (v2.0.2 to v2.0.9)
* Handle newer versions of minitest gem (v1.4.0 to v1.6.0)
* Added warnings about monkey-patching test-unit and minitest to aid debugging. These are enabled by including "debug" in the MOCHA_OPTIONS environment variable. This is now a comma-separated list, so that we can specify multiple options e.g. MOCHA_OPTIONS=debug,use_test_unit_gem
* Eloy Duran (alloy) made the unit tests run on 1.9.2dev r25249.
* Eloy Duran (alloy) also improved some MiniTest TestResult code I'd written and got the acceptance tests running on Ruby 1.9 HEAD. There are still 4 failures because for some reason the backtrace line numbers are off by one. And the minitest_test test case does not run when the whole suite is run with MiniTest. These issues still need investigation.
* Fixed some acceptance tests to run in Ruby 1.9.2 - it's no longer possible to subvert the protection of a method by calling it via Object#send.
* Fixed "test:performance" rake task so it runs in Ruby 1.9.2.
* Fix test incorrectly failing under Rubinius 1.0. This test imposed too many constraints. It appears that Object#inspect legitimately calls Object#object_id in Rubinius. But we're only interested in what 'id' methods Mocha::ObjectMethods#mocha_inspect calls. By stubbing Object#inspect we can relax the constraints imposed by the test.
* Luke Redpath (lukeredpath) added new shorthand "any" and "all" composite parameter matchers using "&" and "|". This provides an alternative syntax for expecting any or all matchers to pass, e.g. foo.expects(:bar).with(equals(1) | equals(2)).
* Improved documentation for Expectation#raises. A number of people have suggested an extension to the API to cope with custom exceptions that have extra constructor parameters. However, since the arguments supplied to Expectation#raises are just passed on to Kernel#raise, it's possible to pass in an instance of an exception. Thus no change to the API is required, but it does seem worthwhile pointing this out in the docs.
* Corrected RDoc example for Expectation#never thanks to Red David (reddavis).
* Improved RDoc including a change suggested by Rohit Arondekar (rohit).
* Updated gemspec as requested by Sam Woodard (shwoodard).

## 0.9.8
* Fixed bug "NameError raised when using Mocha as a Rails plug-in" - http://floehopper.lighthouseapp.com/projects/22289/tickets/53. Since 0.9.6 the Rails plugin has been broken. See bug report for details. You will need to explicitly load Mocha *after* the test framework has been loaded, e.g. by adding "require 'mocha'" at the bottom of test/test_helper.rb.
* Make Mocha::ParameterMatchers#regexp_matches, #includes, #has_value, #has_key more robust. Thanks to Sander Hartlage.
* Allow passing a block to Mocha::Configuration methods to only change configuration for the duration of the block. Thanks to Dan Manges.
* Fixed bug "doc generation fails in 0.9.7 gem" - http://floehopper.lighthouseapp.com/projects/22289/tickets/51.
* Remove rdoc template incorporating google analytics from source control. The file just needs to exist locally and be ignored by source control. This should stop the warning showing up on e.g. RunCodeRun build results.

## 0.9.7
* Although I had provided a deprecation warning for people using Mocha::Standalone, I had assumed people wouldn't be explicitly loading the mocha/standalone.rb file. It turns out this assumption was incorrect at least in the case of Rspec. This is now fixed.

## 0.9.6
* Version 2.0.1 of the test-unit gem introduced a private 'run_test' method on TestCase which clashed with the public TestRunner#run_test method. So this latter method has been renamed to 'run_as_test'.
* Stop requiring rubygems - this should be an environmental choice for the user. http://gist.github.com/54177 - describes why requiring rubygems in your library code is a bad idea.
* It seems like overkill to vendorize coderay and meta_project when they're only needed to generate the examples for documentation and for publishing files on RubyForge. So I'm removing them and installing them locally as gems when I need them.
* Added support for 'test-unit' gem (version >= 2.0). Note that as with other versions of Test::Unit I'm completely replacing the TestCase#run method. Unfortunately in version 2.0.0 this method differs slightly from the same method in version 2.0.1 & 2.0.2, so we have to provide different implementations to ensure that the internal working of Test::Unit are not compromised by Mocha. Note also that unless the 'test-unit' gem is loaded, requiring 'test/unit' leads to a mixture of stdlib and gem classes being loaded causing errors. To avoid a dependency on rubygems, the gem is loaded only if MOCHA_OPTIONS is set to 'use_test_unit_gem' - this option is only intended for use in running Mocha's own tests. It might be worthwhile to create a shim gem like minitest_tu_shim to allow the test-unit gem to completely replace the stdlib, but that's a job for another day. The changes in the Rakefile are to make the default task run with the 'test-unit' gem (version >= 2.0).
* Renamed Mocha::Standalone to Mocha::API to better reflect its purpose. Added a deprecation warning for those who are referencing Mocha::Standalone.
* Fix exception raised by HasEntry#matches? if first param is not a Hash (thanks to Taylor Barstow).
* Ken Collins reported [1] that Mocha is always loading MiniTest if it is available and loading it causes some Rails/ActionPack tests to break. I've removed the loading of MiniTest, but this now means the user has to ensure that if they want to use MiniTest in conjunction with Mocha, he must load MiniTest before loading Mocha. [1] http://rails.lighthouseapp.com/projects/8994-ruby-on-rails/tickets/2060
* Implemented Bacon integration (thanks to Ubiratan Pires Alberton), but this was then removed after deciding only to maintain integration with Test::Unit and MiniTest which are both Ruby standard libraries. See mailing list for details.
* Don't monkey-patch MiniTest if it's already been monkey-patched by Mocha.
* Fixed bug: MiniTest integration was counting ExpectationErrors as errors not failures. http://floehopper.lighthouseapp.com/projects/22289-mocha/tickets/41.
* Fixed bug: Some Bacon tests were failing in Ruby 1.9.1. http://floehopper.lighthouseapp.com/projects/22289-mocha/tickets/43.
* Chad Humphries pointed out that in Ruby 1.9.1, if you are not using Test::Unit or MiniTest, Mocha will attempt to load and monkey-patch Test::Unit. Mocha will now only monkey-patch Test::Unit and/or MiniTest if they have already been loaded. MiniTest tests will now run in both Ruby 1.8.6 (with MiniTest gem) and in Ruby 1.9.1 (with MiniTest std lib). See Ligthouse ticket - http://floehopper.lighthouseapp.com/projects/22289/tickets/49.
* Made Mocha compatible with minitest 1.4.0 and above (thanks to Denis Defreyne).

## 0.9.5
* Fixed Lighthouse bug #32 - stub_everything should mean mock responds to anything.
* Added Expectation#twice to improve readability. Thanks to pull request from Celestino Gomes.
* In Ruby 1.9.1, requiring 'test/unit' loads a thin wrapper around MiniTest and Test::Unit::TestCase ends up inheriting from MiniTest::Unit::TestCase. So we need to avoid including the Mocha modules more than once to avoid nasty consequences. Thanks to Matthias Hennemeyer for help with this.
* Ruby 1.9 includes rake, but not rake/contrib. For the moment I've moved the sshpublisher require into the only rake task that needs it, so that I can at least run the tests in Ruby 1.9. It looks like I will need to build a rake/contrib gem or similar to get this working properly - http://intertwingly.net/blog/2008/01/07/Rake-Contrib-for-1-9

## 0.9.4
* Added mocha.gemspec file generated with Chad Woolley's new rake task, so that a floehopper-mocha gem will get built on GitHub.
* Add rake task to update mocha.gemspec with unique version, which will cause gem to be auto-built on github
* As Tobias Crawley correctly pointed out in feature request #23055 "stubs(with_hash) not working with existing object" [1], following the principle of least surprise, it should be possible to call ObjectMethods#expects & ObjectMethods#stubs with a Hash of method_names vs return_values like you can with Mock#expects & Mock#stubs. I've also updated & improved the docs to reflect the changes. [1] http://rubyforge.org/tracker/index.php?func=detail&aid=23055&group_id=1917&atid=7480
* Removed deprecated gem autorequire.

## 0.9.3
* Added support for MiniTest thanks to Jeff Smick.
* Fixed a possible bug with some of the non-default Configuration options relating to the argument to Object#respond_to?
* As per Jay Fields recommendations [1] and with further impetus from a talk at Ruby Manor, any methods added to core classes are now added by including a module. This means that Mocha is a better citizen of the Ruby world and it's behaviour is more easily extended. [1] http://blog.jayfields.com/2008/07/ruby-underuse-of-modules.html & http://blog.jayfields.com/2008/07/ruby-redefine-method-behavior.html
* Removed deprecated gem autorequire.

## 0.9.2
* Improved documentation to address [#22530] 'Mock methods with multiple return values not possible?'
* respond_with parameter matcher was not available in tests.
* Patch [#22630] Fix for a bug in running Rails tests with Ruby 1.8.7. Array#flatten was being called which in turn was checking whether each element responded to #to_ary. This check was using the two parameter version of #respond_to?, but Mock was only defining a one parameter version.

## 0.9.1

* Fixed bug #21465 - expects & stubs should support method names as strings (as well as symbols) or fail fast. Convert all expectation method names to a symbol in case they were supplied as a string.
* By removing Mock#unexpected_method_called we reduce the number of methods vulnerable to the problem that surfaced in bug #21563.
* Fix bug #21563 - stubbing 'verified?' method is unsafe. Instance method names on the Mock class should be more obscure.
* Performance improvement. StubbaExampleTest goes twice as fast on my local machine.
* Added primitive performance test to default rake task.
* Fix format of case statements which don't work in Ruby 1.9 and make others consistent.
* There is no point in running (potentially expensive) checks if configuration is set to allow such checks to fail. This is a relatively quick fix in response to Chris McGrath's performance problems.
* Fix for bug #21161 - 'uninitialized constant Deprecation in stubba.rb'.
* It's more readable to talk about 'once' and 'twice' rather than '1 time' and '2 times'.
* Fix bug #20883 - never should raise when called to prevent follow up errors. Fail fast when there are no matching invokable expectations and handle the stub_everything case sensibly. This might not be entirely backwards compatible, but I think the benefits outweigh the risks. The most likely change is that a test that was already failing will now fail faster, which doesn't seem so awful.

## 0.9.0

* Configurable warnings or errors
  * when a method on a non-public method is stubbed
  * when a method on a non-existent method is stubbed
  * when a method on a non-mock object is stubbed
  * when a method is stubbed unnecessarily (i.e. the stubbed method is not called during the test)

* Improved error messages
  * User-friendly list of unsatisfied expectations, satisfied expectations and state machines.
  * Improved readability of cardinality description.
  * Display sensible failure message for any_instance expectations e.g. "#<AnyInstance:Foo>.bar - expected calls: 1, actual calls: 0"

* Parameter matchers
  * New to this release
    * optionally (allows matching of optional parameters if available)
    * yaml_equivalent (allows matching of YAML that represents the specified object)
    * responds_with (tests the quack not the duck)
  * Nesting of parameter matchers is now supported.

* Optional block passed into mock initializer is evaluated in the context of the new mock instance and can be used as a shortcut to set up expectations.

* Added JMock-style sequences for constraining the order of expected invocations. See Standalone#sequence and Expectation#in_sequence.

* Added JMock-style states for constraining the order of expected invocations. See Standalone#states, Expectation#then, Expectation#when and StateMachine.

* Compatibility with versions of Ruby
  * Compatibility with Ruby v1.9. All test errors and warnings fixed.
  * Nasty fix so that TestCaseAdaptor works consistently with earlier versions of Test::Unit as well as more recent versions.
  * Added platform to gem specification to avoid bug in rubygems 0.9.5 - see http://www.dcmanges.com/blog/rubygems-0-9-5-platform-bug and http://rubygems.org/read/chapter/20#platform.
  * Make ExpectationRaiser deal with subclasses of Interrupt which seem to need a message supplied in the raise statement in Ruby 1.8.6 (but not 1.8.4 or 1.9). Not sure this is really Mocha's responsibility.

* Added deprecation warning in stubba.rb which is no longer needed and will be removed.

* Supply positioning information to evals to improve any error messages. See http://ola-bini.blogspot.com/2008/01/ruby-antipattern-using-eval-without.html

* Bug fixes
  * 18914 in revision 296 - http://rubyforge.org/tracker/index.php?func=detail&aid=18914&group_id=1917&atid=7477
  * 18917 in revision 295 - http://rubyforge.org/tracker/index.php?func=detail&aid=18917&group_id=1917&atid=7477
  * 18336 in revision 287 - http://rubyforge.org/tracker/index.php?func=detail&aid=18336&group_id=1917&atid=7477
  * 17835 in revision 255 - http://rubyforge.org/tracker/index.php?func=detail&aid=17835&group_id=1917&atid=7477
  * 17412 in revision 242 - http://rubyforge.org/tracker/index.php?func=detail&aid=17412&group_id=1917&atid=7477
  * 15977 in revision 198 - http://rubyforge.org/tracker/index.php?func=detail&aid=15977&group_id=1917&atid=7477
  * 11885 in revision 156 - http://rubyforge.org/tracker/index.php?func=detail&aid=11885&group_id=1917&atid=7477

## 0.5.5

- Renamed Matches parameter matcher to RegexpMatches for clarity.
- Added noframes tag to rdoc index to assist Google.

## 0.5.4

- Added matches parameter matcher for matching regular expressions.

## 0.5.3

- Attempt to fix packaging problems by switching to newer version (1.15.1) of gnutar and setting COPY_EXTENDED_ATTRIBUTES_DISABLE environment variable.
- Removed unused ExpectationSequenceError exception.
- Added instance_of and kind_of parameter matchers.
- Added Google Webmaster meta tag to rdoc template header.
- Put Google Webmaster meta tag in the right header i.e. the one for the index page.

## 0.5.2

- Fix bug 11885 - "never doesn't work with stub_everything" submitted by Alexander Lang. In fixing this bug, also fixed undiscoverd bug where expected & actual invocation counts were being incorrectly reported which seems to have been introduced when fixes were added for invocation dispatch (see MockedMethodDispatchAcceptanceTest).
- Previously when an expectation did not allow more invocations, it was treated as not matching. Now we prefer matching expectations which allow more invocations, but still match expectations which cannot allow more invocations. I think this may be overcomplicating things, but let's see how it goes.

## 0.5.1

- Fixed bug #11583 "Mocha 0.5.0 throwing unexpected warnings". Also switched on ruby warning for all rake test tasks. Fixed majority of warnings, but some left to fix.

## 0.5.0

- Parameter Matchers - I’ve added a few Hamcrest-style parameter matchers which are designed to be used inside Expectation#with. The following matchers are currently available: anything(), includes(), has_key(), has_value(), has_entry(), all_of() & any_of(). More to follow soon. The idea is eventually to get rid of the nasty parameter_block option on Expectation#with.

  object = mock()
  object.expects(:method).with(has_key('key_1'))
  object.method('key_1' => 1, 'key_2' => 2)
  # no verification error raised

  object = mock()
  object.expects(:method).with(has_key('key_1'))
  object.method('key_2' => 2)
  # verification error raised, because method was not called with Hash containing key: 'key_1'

- Values Returned and Exceptions Raised on Consecutive Invocations - Allow multiple calls to Expectation#returns and Expectation#raises to build up a sequence of responses to invocations on the mock. Added syntactic sugar method Expectation#then to allow more readable expectations.

  object = mock()
  object.stubs(:method).returns(1, 2).then.raises(Exception).then.returns(4)
  object.method # => 1
  object.method # => 2
  object.method # => raises exception of class Exception
  object.method # => 4

- Yields on Consecutive Invocations - Allow multiple calls to yields on single expectation to allow yield parameters to be specified for consecutive invocations.

  object = mock()
  object.stubs(:method).yields(1, 2).then.yields(3)
  object.method { |*values| p values } # => [1, 2]
  object.method { |*values| p values } # => [3]

- Multiple Yields on Single Invocation - Added Expectation#multiple_yields to allow a mocked or stubbed method to yield multiple times for a single invocation.

  object = mock()
  object.stubs(:method).multiple_yields([1, 2], [3])
  object.method { |*values| p values } # => [1, 2] # => [3]

- Invocation Dispatch - Expectations were already being matched in reverse order i.e. the most recently defined one was being found first. This is still the case, but we now stop matching an expectation when its maximum number of expected invocations is reached. c.f. JMock v1. A stub will never stop matching by default. Hopefully this means we can soon get rid of the need to pass a Proc to Expectation#returns.

  object = mock()
  object.stubs(:method).returns(2)
  object.expects(:method).once.returns(1)
  object.method # => 1
  object.method # => 2
  object.method # => 2
  # no verification error raised

  # The following should still work...

  Time.stubs(:now).returns(Time.parse('Mon Jan 01 00:00:00 UTC 2007'))
  Time.now # => Mon Jan 01 00:00:00 UTC 2007
  Time.stubs(:now).returns(Time.parse('Thu Feb 01 00:00:00 UTC 2007'))
  Time.now # => Thu Feb 01 00:00:00 UTC 2007

- Deprecate passing an instance of Proc to Expectation#returns.
- Explicitly include all Rakefile dependencies in project.
- Fixed old Stubba example.
- Fix so that it is possible for a stubbed method to raise an Interrupt exception without a message in Ruby 1.8.6
- Added responds_like and quacks_like.
- Capture standard object methods before Mocha adds any.
- Added Expectation#once method to make interface less surprising.
- Use Rake::TestTask to run tests. Created three separate tasks to run unit, integration & acceptance tests. Split inspect_test into one file per TestCase. Deleted superfluous all_tests file.
- Fiddled with mocha_inspect and tests to give more sensible results on x86 platform.
- Fixed bug #7834 "infinite_range.rb makes incorrect assumption about to_f" logged by James Moore.

## 0.4.0

- Allow naming of mocks (patch from Chris Roos).
- Specify multiple return values for consecutive calls.
- Improved consistency of expectation error messages.
- Allow mocking of Object instance methods e.g. kind_of?, type.
- Provide aliased versions of #expects and #stubs to allow mocking of these methods.
- Added at_least, at_most, at_most_once methods to expectation.
- Allow expects and stubs to take a hash of method and return values.
- Eliminate warning: "instance variable @yield not initialized" (patch from Xavier Shay).
- Restore instance methods on partial mocks (patch from Chris Roos).
- Allow stubbing of a method with non-word characters in its name (patch from Paul Battley).
- Removed coupling to Test::Unit.
- Allow specified exception instance to be raised (patch from Chris Roos).
- Make mock object_id appear in hex like normal Ruby inspect (patch from Paul Battley).
- Fix path to object.rb in rdoc rake task (patch from Tomas Pospisek).
- Reverse order in which expectations are matched, so that last expectation is matched first. This allows e.g. a call to #stubs to be effectively overridden by a call to #expects (patch from Tobias Lutke).
- Stubba & SmartTestCase modules incorporated into Mocha module so only need to require 'mocha' - no longer need to require 'stubba'.
- AutoMocha removed.

## 0.3.3

- Quick bug fix to restore instance methods on partial mocks (for Kevin Clark).

## 0.3.2

- Examples added.

## 0.3.1

- Dual licensing with MIT license added.

## 0.3.0

* Rails plugin.
* Auto-verify for expectations on concrete classes.
* Include each expectation verification in the test result assertion count.
* Filter out noise from assertion backtraces.
* Point assertion backtrace to line where failing expectation was created.
* New yields method for expectations.
* Create stubs which stub all method calls.
* Mocks now respond_to? expected methods.

## 0.2.1

* Rename MochaAcceptanceTest::Rover#move method to avoid conflict with Rake (in Ruby 1.8.4 only?)

## 0.2.0

* Small change to SetupAndTeardown#teardown_stubs suggested by Luke Redpath (http://www.lukeredpath.co.uk) to allow use of Stubba with RSpec (http://rspec.rubyforge.org).
* Reorganized directory structure and extracted addition of setup and teardown methods into SmartTestCase mini-library.
* Addition of auto-verify for Mocha (but not Stubba). This means there is more significance in the choice of expects or stubs in that any expects on a mock will automatically get verified.

So instead of...

  wotsit = Mocha.new
  wotsit.expects(:thingummy).with(5).returns(10)
  doobrey = Doobrey.new(wotsit)
  doobrey.hoojamaflip
  wotsit.verify

you need to do...

  wotsit = mock()
  wotsit.expects(:thingummy).with(5).returns(10)
  doobrey = Doobrey.new(wotsit)
  doobrey.hoojamaflip
  # no need to verify

There are also shortcuts as follows...

instead of...

  wotsit = Mocha.new
  wotsit.expects(:thingummy).returns(10)
  wotsit.expects(:summat).returns(25)

you can have...

  wotsit = mock(:thingummy => 5, :summat => 25)

and instead of...

  wotsit = Mocha.new
  wotsit.stubs(:thingummy).returns(10)
  wotsit.stubs(:summat).returns(25)

you can have...

  wotsit = stub(:thingummy => 5, :summat => 25)

## 0.1.2

* Minor tweaks

## 0.1.1

* Initial release.
