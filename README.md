# cl-quickcheck - a Common Lisp port of the QuickCheck unit test framework

# HOMEPAGE

[http://www.yellosoft.us/quickcheck](http://www.yellosoft.us/quickcheck)

# EXAMPLE

    $ clisp
    > (load 'example)
    ...
    T

# REQUIREMENTS

* a [Common Lisp](http://www.cliki.net/Common%20Lisp%20implementation), such as `clisp`
* [Quicklisp](http://www.quicklisp.org/)

## Optional

* [Ruby](https://www.ruby-lang.org/) 2+
* [Bundler](http://bundler.io/)
* [Cucumber](http://cukes.info/)
* [Guard](http://guardgem.org/)

# TESTING

Ensure the example script works as expected:

    $ bundle
    $ cucumber
    Feature: Run example tests

      Scenario: Running example tests            # features/run_example_tests.feature:3
        Given the program has finished           # features/step_definitions/steps.rb:1
        Then the output is correct for each test # features/step_definitions/steps.rb:5

    1 scenario (1 passed)
    2 steps (2 passed)
    0m4.913s

Guard can automatically run testing when the code changes:

    $ bundle
    $ guard -G Guardfile-cucumber
    >
    ...

# INSTALL

    $ clisp
    > (ql:quickload 'cl-quickcheck)
