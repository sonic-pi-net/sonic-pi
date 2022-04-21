### 2.0.0 - 2020-11-26

* Conform to TOML v1.0.0-rc3

#### Breanking Changes ####

TOML v0.5.0 introduced new value types: Local Date-Time, Local Date and Local Time which represent time without time zone information. Tomlrb also introduced corresponding classes starting from v2.0.0. By this change, some table values such as `2020-11-24T20:32:18`, `2020-11-24` or `20:32:18` are not treated as strings but as new the classes `Tomlrb::LocalDateTime`, `Tomlrb::LocalDate` and `Tomlrb::LocalTime` respectively. You can get the string values by calling `#to_s` method on any of those classes. Additionally, You can also get `Time` objects with the `#to_time` method. See [API documentation](https://www.rubydoc.info/gems/tomlrb) for the methods' details.

### 1.3.0 - 2020-03-19

* Fix error with falsy table values

### 1.2.9 - 2019-11-22

* Fixes and cleanups for ruby 2.7

### 1.2.8 - 2018-12-18

* Reduce gem size by excluding tests (tas50)
* Make integer and float parsing closer to the spec (sgarciac)

### 1.2.7 - 2018-07-12

* Datetime should be UTC when no offset or timezone are specified

### 1.2.6 - 2017-10-23

* Fix issue where an unclosed table could make the parsing loop infinitely.
* Proper string escaping

### 1.1.3 - 2015-11-24

* Bare integers can be used as keys as per the spec
