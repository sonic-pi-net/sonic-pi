# How to build an MSI installer using WiX

1. Build `sonic-pi.exe` and run it to test.
2. Copy wix directory to top level sonic-pi directory
3. Remove files in `etc` and `app\gui` you don't want to include in the msi
   - `app\gui` should likely only have the `release` and `theme` dirs, everything else can go
4. Generate the wixobj files
   - `wix\gen_wix.bat`
5. Update version number
   - edit wix\sonic-pi.wxs
   - change version number (from X.Y.Z)
6. Remove duplicate info
   - edit `wix\gui.wxs`
   - remove sonic-pi.exe component and componentref (both have identical IDs)
7. Build the msi!
   - `wix\gen_msi.bat`
8. Your MSI should be found in the `wix` dir.

