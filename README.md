# Delphi Dabbling
The code in this repository are a collection of bits n bobs that I am fed up of re-creating in every application!

## uHelperListView
This is essentially the Component TListViewZaaz (modified TListView) as Class Helpers, hence no requirement to install a component! (Makes moving the code to a different IDE simpler)

## uSvgHelper
This is a simple unit to help save an SVG file as a .PNG file
Simple to call:
```delphi
procedure TMainForm.btnClcik(Sender: TObject);
begin
   // ensure uSvgHelper is in the uses
   ExportSvgToPngKeepingAspect(SkSVG1.svg.source, 330, 'engineer-330.png');
end;
```

## Launch a second form
One of the first things I did using RAD Studio, was launch a second TForm object (This was back way before Android / IOS existed)

Something to remember: Do you need to automatically create the form at application start?

If not, then in your project, click on the menu Project---> Options---> Forms and remove the appropriate forms from the Auto-create forms list box

### FMX Modern Style Delphi code (For Android / IOS to avoid it returning straight away!)
This approach uses the modern Delphi in code variable (var) declarations, i..e at create time, not in the section between the procedure/function and the first begin.

The boat is still out for me! 

```delphi
procedure TMainForm.btnShowSecondForm(Sender: TObject);
begin
   var := TTheSecondForm.Create(nil);
   frm.ShowModal (
      procedure(ModalResult: TModalResult)
      begin
         // Put something here to do AFTER form has closed and come back e.g.
         // RefreshFirstPage;
      end
   );
end;
```

Now whenever you create your pdf

### FMX Traditional Delphi code (For Android / IOS to avoid it returning straight away!)
```delphi
procedure TMainForm.btnShowSecondForm(Sender: TObject);
var
   frm: TTheSecondForm;
begin
   frm := TTheSecondForm.Create(nil);
   frm.ShowModal (
      procedure(ModalResult: TModalResult)
      begin
         // Put something here to do AFTER form has closed and come back e.g.
         // RefreshFirstPage;
      end
   );
end;
```

### VCL Modern style Delphi code
```delphi
procedure TMainForm.btnShowSecondForm(Sender: TObject);
begin
   var frm := TTheSecondForm.Create(self);
   try
      frm.ShowModal;
   finally
      frm.Free; // safe in VCL apps, not so much in FMX
   end;
end;
```

### VCL Traditional style Delphi code
```delphi
procedure TMainForm.btnShowSecondForm(Sender: TObject);
var
   frm: TTheSecondForm;
begin
   frm := TTheSecondForm.Create(self);
   try
      frm.ShowModal;
   finally
      frm.Free; // safe in VCL apps, not so much in FMX
   end;
end;
```


> [!IMPORTANT]
> ## Using PROVIDERS to launch external PDF document viewer on Android
> ### This has been tested under Delphi 12.3 Android64, Windows64

Recent versions of Android require that providers must be used to launch a external viewer for things like PDF which are not natively supported under Android
The following instructions should help you to configure the application manifest so it can use the provider. I spent hours using chatGPT, Copilot, Preplexity etc, and trawling hundreds of delphi documents, but not one showed the steps, the legendary Stephen A Ball has a video how to [@Stephan Ball](https://delphiaball.co.uk/2018/08/03/opening-a-pdf-on-android-with-delphi/) but sometimes simply how to do in writing helps!

> [!IMPORTANT]
> Something worth noting: In your application, when you create your file to be launched externally, ensure you are using the correct file path for it
> Research has suggested that the best palce (in android) is the TPath.GetCachePath();

### Step 1 : Create a fileprovider.xml file
Create a file names fileprovider.xml in your Delphi project, and give it the following contents...
```xml
<?xml version="1.0" encoding="utf-8"?>
<paths xmlns:android="http://schemas.android.com/apk/res/android">
    <cache-path name="cache" path="." />
    <external-files-path name="files" path="." />
</paths>
```

### Add it to the deployment
Ensure you have Android64 Bit platform selected:
In the Delphi IDE, Menu Project->Deployment to launch the deployment screen
Click "Add files" tool button and select the fileprovider.xml
In Remote Path set the path as <b>res\xml\ <b> (note the trailing \ backslash)
Ensure you do this for BOTH Release and Debug!

### Change the default AndroidManifest.template.xml
When you fist deploy your android application, the IDE automatically creates a AndroidManifest.template.xml file in the project root folder
It will typically look like this:
```xml
<?xml version="1.0" encoding="utf-8"?>
<!-- BEGIN_INCLUDE(manifest) -->
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    package="%package%"
    android:versionCode="%versionCode%"
    android:versionName="%versionName%"
    android:installLocation="%installLocation%">
    <uses-sdk android:minSdkVersion="%minSdkVersion%" android:targetSdkVersion="34" />
<%uses-permission%>
    <uses-feature android:glEsVersion="0x00020000" android:required="true"/>
    <queries>
<%queries-child-elements%>
    </queries>
    <application
        android:persistent="%persistent%"
        android:restoreAnyVersion="%restoreAnyVersion%"
        android:label="%label%"
        android:debuggable="%debuggable%"
        android:largeHeap="%largeHeap%"
        android:icon="%icon%"
        android:theme="%theme%"
        android:hardwareAccelerated="%hardwareAccelerated%"
        android:resizeableActivity="true"
        android:requestLegacyExternalStorage="true">
<%provider%>
<%application-meta-data%>
<%uses-libraries%>
        <!-- Trigger Google Play services to install the backported photo picker module. -->
        <service
            android:name="com.google.android.gms.metadata.ModuleDependencies"
            android:enabled="false"
            android:exported="false"
            tools:ignore="MissingClass">
            <intent-filter>
                <action android:name="com.google.android.gms.metadata.MODULE_DEPENDENCIES" />
            </intent-filter>

            <meta-data android:name="photopicker_activity:0:required" android:value="" />
        </service>
<%services%>
        <!-- Our activity is a subclass of the built-in NativeActivity framework class.
             This will take care of integrating with our NDK code. -->
        <activity
            android:name="com.embarcadero.firemonkey.FMXNativeActivity"
            android:exported="true"
            android:label="%activityLabel%"
            android:configChanges="orientation|keyboard|keyboardHidden|screenSize|screenLayout|uiMode"
            android:launchMode="singleTask">
            <!-- Tell NativeActivity the name of our .so -->
            <meta-data android:name="android.app.lib_name" android:value="%libNameValue%" />

            <intent-filter>
                <action android:name="android.intent.action.MAIN" />

                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>
<%activity%>
<%receivers%>
    </application>
</manifest>
<!-- END_INCLUDE(manifest) -->
```

In order to use the provider, we must change the line <b><%provider%><b> with the following:
```xml
<provider
    android:name="androidx.core.content.FileProvider"
    android:authorities="%package%.provider"
    android:exported="false"
    android:grantUriPermissions="true">
    <meta-data
        android:name="android.support.FILE_PROVIDER_PATHS"
        android:resource="@xml/fileprovider" />
</provider>
```

Essentially this gives you something like
```xml
<?xml version="1.0" encoding="utf-8"?>
<!-- BEGIN_INCLUDE(manifest) -->
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    package="%package%"
    android:versionCode="%versionCode%"
    android:versionName="%versionName%"
    android:installLocation="%installLocation%">
    <uses-sdk android:minSdkVersion="%minSdkVersion%" android:targetSdkVersion="34" />
<%uses-permission%>
    <uses-feature android:glEsVersion="0x00020000" android:required="true"/>
    <queries>
<%queries-child-elements%>
    </queries>
    <application
        android:persistent="%persistent%"
        android:restoreAnyVersion="%restoreAnyVersion%"
        android:label="%label%"
        android:debuggable="%debuggable%"
        android:largeHeap="%largeHeap%"
        android:icon="%icon%"
        android:theme="%theme%"
        android:hardwareAccelerated="%hardwareAccelerated%"
        android:resizeableActivity="true"
        android:requestLegacyExternalStorage="true">
        <!--
            Replaced the default so it handles pdf/html launching etc code by Stephan Ball
            Code found at  
            https://delphiaball.co.uk/2018/08/03/opening-a-pdf-on-android-with-delphi/
        -->
        <provider
            android:name="androidx.core.content.FileProvider"
            android:authorities="%package%.provider"
            android:exported="false"
            android:grantUriPermissions="true">
            <meta-data
                android:name="android.support.FILE_PROVIDER_PATHS"
                android:resource="@xml/fileprovider" />
        </provider>
<%application-meta-data%>
<%uses-libraries%>
        <!-- Trigger Google Play services to install the backported photo picker module. -->
        <service
            android:name="com.google.android.gms.metadata.ModuleDependencies"
            android:enabled="false"
            android:exported="false"
            tools:ignore="MissingClass">
            <intent-filter>
                <action android:name="com.google.android.gms.metadata.MODULE_DEPENDENCIES" />
            </intent-filter>

            <meta-data android:name="photopicker_activity:0:required" android:value="" />
        </service>
<%services%>
        <!-- Our activity is a subclass of the built-in NativeActivity framework class.
             This will take care of integrating with our NDK code. -->
        <activity
            android:name="com.embarcadero.firemonkey.FMXNativeActivity"
            android:exported="true"
            android:label="%activityLabel%"
            android:configChanges="orientation|keyboard|keyboardHidden|screenSize|screenLayout|uiMode"
            android:launchMode="singleTask">
            <!-- Tell NativeActivity the name of our .so -->
            <meta-data android:name="android.app.lib_name" android:value="%libNameValue%" />

            <intent-filter>
                <action android:name="android.intent.action.MAIN" />

                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>
<%activity%>
<%receivers%>
    </application>
</manifest>
<!-- END_INCLUDE(manifest) -->
```

### How to call the provider from code...
According to the web, recent version of Delphi targetting Android that use providers should place the files that want to be shown in the TPath.GetCachePath
One way would be to create a function that you pass the filename to, and it returns the filename with it's fully qualified path, for example 

```delphi
function GetProviderPath(const AFilename: string): string;
begin
  // Used to get the fully qualified device independant file name for the pdf worksheet
  {$IF DEFINED(Android)}
    Result := TPath.Combine(TPath.GetCachePath, AFilename);  // Cache for FileProvider sharing
  {$ELSEIF DEFINED(MSWINDOWS)}
    Result := TPath.Combine(TPath.GetDocumentsPath, AFilename);
  {$ELSEIF DEFINED(MACOS)}
    Result := TPath.Combine(TPath.GetDocumentsPath, AFilename);
  {$ELSEIF DEFINED(IOS)}
    Result := TPath.Combine(TPath.GetDocumentsPath, AFilename);
  {$ELSE}
    Result := TPath.Combine(TPath.GetDocumentsPath, AFilename);  // Default fallback
  {$ENDIF}
end;
```

Now when you create your PDF, you can pass the filename to the function and then show it using something along the lines of the following

```delphi
procedure LaunchPDFProvider(const AFilename: String);
{$IF Defined(Android)}
var
  Intent: JIntent;
  FileObj: JFile;
  Uri: Jnet_Uri;
  Authority: JString;
{$ENDIF}
{$IF Defined(IOS)}
var
  URL: NSURL;
{$ENDIF}
{$IF Defined(MACOS)}
var
  URL: NSURL;
{$ENDIF}
begin
  if AFilename.IsEmpty then
    Exit;
  if not FileExists(AFilename) then
    Exit;
  {$IF Defined(MSWINDOWS)}
    ShellExecute(0, nil, PChar(AFilename), nil, nil, SW_SHOWNORMAL);
  {$ENDIF}
  {$IF Defined(Android)}
    FileObj := TJFile.JavaClass.init(StringToJString(AFilename));
    Authority := StringToJString(JStringToString(TAndroidHelper.Context.getPackageName) + '.provider');
    Uri := TJContent_FileProvider.JavaClass.getUriForFile(TAndroidHelper.Context, Authority, FileObj);
    Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
    Intent.setDataAndType(Uri, StringToJString('application/pdf'));
    Intent.addFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
    Intent.addFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NO_HISTORY);
    TAndroidHelper.Activity.startActivity(Intent);
  {$ENDIF}
  {$IF Defined(IOS)}
    URL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(NSStr(AFilename)));
    SharedApplication.openURL(URL);
  {$ENDIF}
  {$IF Defined(MACOS)}
    URL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(NSStr(AFilename)));
    NSWorkspace.sharedWorkspace.openURL(URL);
  {$ENDIF}
end;
```


## Authors
😎 Mark Richards [@MascotZombie](https://thezombiecoders.co.uk)

## Version History
* 21/10/2025 **First Checkin**

## License
✅ Do what you want with this, any improvements would be helpfull

## 💬 Disclaimer
This software is provided "as is," without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose, and non-infringement.

The use of this software is entirely at your own risk. In no event shall the authors or copyright holders be liable for any claim, damages, or other liabilities, whether in an action of contract, tort, or otherwise, arising from, out of, or in connection with the software or the use or other dealings in the software.
