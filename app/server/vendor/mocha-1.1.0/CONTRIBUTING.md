* Pull requests are welcomed.
* Fork the repository.
* Make your changes in a branch.
* Add tests for new behaviour. Modify/remove existing tests for changes to existing behaviour.
* Run `bin/build-matrix` from the root directory and ensure all the tests pass.
  * This script depends on `rbenv` being installed.
  * You must have all the ruby versions listed in `.travis.yml` under the `rvm` key installed (currently 1.8.7, 1.9.3 & 2.0.0).
  * I use `rbenv-aliases` to alias the patch versions.
  * Note that the build matrix takes quite a while to run.
* Send us a pull request from your fork/branch.
* Wait for your pull request to build on [Travis CI](https://travis-ci.org/freerange/mocha).
* I will not accept pull requests with failing tests.
