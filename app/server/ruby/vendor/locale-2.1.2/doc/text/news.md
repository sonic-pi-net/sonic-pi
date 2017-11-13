# News

## <a id="2-1-2">2.1.2</a>: 2015-09-15

### Improvements

  * Improved GNU gettext compatibility: Ignored `LANGUAGE` environment
    variable when either `LC_ALL`, `LC_MESSAGES` or `LANG` is C.
    [GitHub#6] [Reported by Hleb Valoshka]

### Thanks

  * Hleb Valoshka

## <a id="2-1-1">2.1.1</a>: 2015-05-16

### Improvements

  * Migrated to Fiddle from DL. Because DL is deprecated.
    [GitHub#5] [Patch by moguno]

### Thanks

  * moguno

## <a id="2-1-0">2.1.0</a>: 2013-12-15

### Fixes

  * Fixed a bug that memoization key hash collision on armv7hl.
    Memoization feature is removed for this fix. If you get
    performance issue. Please report it. We will solve the issue.
    [GitHub#3] [Reported by mtasaka]

### Thanks

  * mtasaka

## <a id="2-0-9">2.0.9</a>: 2013-09-20

Locale handling fix release.

### Improvements

  * Added license metadata to gem package.
    [GitHub#1] [Suggested by jordimassaguerpla]

### Fixes

  * Added missing .yardopts file.
  * Fixed a bug that wrong environment value is used.
    [Debian:#520181][Debian:#690572][GitHub#2]
    [Reported by Stefano Zacchiroli][Reported by Hleb Valoshka]

### Thanks

  * jordimassaguerpla
  * Stefano Zacchiroli
  * Hleb Valoshka

## <a id="2-0-8">2.0.8</a>: 2012-08-29

Package fix release.

### Fixes

  * Added missing .yardopts file.

## <a id="2-0-7">2.0.7</a>: 2012-08-29

Package fix release.

### Fixes

  * Added missing this file. [Reported by Takahiro Kambe]

### Thanks

  * Takahiro Kambe

## <a id="2-0-6">2.0.6</a>: 2012-08-29

Ruby 1.9 on Windows support release.

### Improvements

  * Worked with invalid LANGUAGE variable value. It's just
    ignored. [Patch by Alexey l.Froloff] [Reported by Friedrich, Axel]
  * Added workaround for Ruby 1.8.6.
  * Supported multiple `Locale.init` call in the same process.
  * Supportd Ruby 1.9 on Windows.
  * Supported `Locale.current = Locale.current`.
    [GitHub#mutoh/locale#5] [Debian#600713]
    [Reported by Martin Hradil and Hleb Valoshka]
  * Supported `Locale.init` on `$SAFE > 0`.
    [GitHub#mutoh/locale#5] [Reported by Hleb Valoshka]
  * Added a Rack middleware `Locale::Middleware` that initializes locale by
    client request.

### Tests

  * Fixed a problem that tests for Windows break other tests on non
    Windows platform. [Patch by J. Pablo Fernández]
  * [jruby] Fixed wrong variant format.
    [GitHub#mutoh/locale#5] [Patch by Hleb Valoshka]
  * Fixed wrong environment variable check. `LC_CTYPES` is checked
    instead of `LC_MESSAGES`.
    [GitHub#mutoh/locale#5] [Debian#520181]
    [Reported by Adeodato Simó] [Patch by Hleb Valoshka]

### Thanks

  * Masao Mutoh
  * Alexey l.Froloff
  * Friedrich, Axel
  * J. Pablo Fernández
  * Martin Hradil
  * Hleb Valoshka
  * Adeodato Simó
