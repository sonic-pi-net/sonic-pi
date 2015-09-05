How to contribute to this project.

## Thin rules (to be made into sections)

* Work off the dev branch, not master.
* Pull requests should be named something related to PR, `hotfix-XXX` or `feature-YYY`.
* Do not edit the `version.rb` or the `gemspec`.
* All pull requests have to pass tests on `travis-ci` before I'll consider merging them.
* for tests of the actual parser, make sure you include a new `usage_scenario`, not update an existing one.  I'll merge stuff as it gets cluttered.

## Formatting and style

Run all your code through `ruby-beautify` with default settings before committing.

This should work (and if it doesn't, please let me know)

`ruby-beautify --overwrite lib/**/*.rb  bin/ruby-beautify spec/**/*_spec.rb spec/**/*_helper.rb`

Which should pretty up only the code files, and not our test files.  This will help insure pretty commits and that they will be accepted.
