java -jar packr.jar \
     -platform windows \
     -jdk "/tmp/openjdk-1.7.0-u80-unofficial-windows-amd64-image.zip" \
     -executable freespectro \
     -appjar desktop/build/libs/desktop-1.0.jar \
     -mainclass "com/mygdx/game/desktop/DesktopLauncher" \
     -vmargs "-Xmx256M" \
     -minimizejre "hard" \
     -outdir outwin
