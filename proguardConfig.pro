-injars      ReversiRaw.jar
-outjars     Reversi.jar
-libraryjars <java.home>/lib/rt.jar
-libraryjars <java.home>/lib/ext/jfxrt.jar

-target 1.8
-dontwarn scala.**
-verbose
-dontoptimize
-dontobfuscate
-dontusemixedcaseclassnames
-dontnote
-ignorewarnings


-keepclasseswithmembers public class nightra.reversi.ui.UI {
    public static void main(java.lang.String[]);
}

-keepclasseswithmembers public class nightra.reversi.ui.tree.TreeUI {
    public static void main(java.lang.String[]);
}
