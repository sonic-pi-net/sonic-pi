The Java code in the module's native/java subfolders have been used to generate
dex byte-code in various places in the JUCE framework. These are the steps
required to re-generate the dex byte-code from any Java source code inside the
native/java subfolders:

1. Create a new JUCE android project with the minimal sdk version which is
required for the Java source code you wish to compile.

2. If you are creating byte-code for new .java files, move the new files into
the native/javacore/app folder of the module, or create one if it doesn't
exist. Remember that .java files need to be in nested sub-folders which
resemble their package, i.e. a Java class com.rmsl.juce.HelloWorld.java should
be in the module's native/javacore/app/com/rmsl/juce folder.  If you wish to
modify existing .java files in the JUCE modules then just rename native/java to
native/javacore.

3. Build your project with Android Studio and run. The app will now use the
source code in the folder created in step 2 so you can debug your Java code
this way.

4. Once everything is working rebuild your app in release mode.

5. Go to your app's Builds/Android folder. Inside there you will find
build/intermediates/javac/release_Release/compileRelease_ReleaseJavaWithJavac/classes.
Inside of that folder, you will find all your Java byte-code compiled classes.
Remove any classes that you are not interested in (typically you'll find
Java.class and JuceApp.class which you will probably want to remove).

6. Inside of
build/intermediates/javac/release_Release/compileRelease_ReleaseJavaWithJavac/classes
execute the following dx command:

    <path-to-your-android-sdk>/build-tools/<latest-build-tool-version>/dx --dex --verbose --min-sdk-version=<your-min-sdk-of-your-classes> --output /tmp/JavaDexByteCode.dex .

    (Replace <your-min-sdk-of-your-classes> with the minimal sdk version you used in step 1.)

7. gzip the output:

    gzip /tmp/JavaDexByteCode.dex

8. The output /tmp/JavaDexByteCode.dex.gz is now the byte code that can be
included into JUCE. You can use the Projucer's BinaryData generator
functionality to get this into a convenient char array like form.
