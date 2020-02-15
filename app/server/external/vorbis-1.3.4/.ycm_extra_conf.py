def FlagsForFile(filename, **kwargs):
  return {
    'flags': [
      '-x', 'c',
      '-g', '-Wall', '-Wextra',
      '-D_REENTRANT', '-D__NO_MATH_INLINES', '-fsigned-char'
    ],
  }          
