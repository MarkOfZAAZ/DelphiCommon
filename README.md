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

## Authors
😎 Mark Richards [@MascotZombie](https://thezombiecoders.co.uk)

## Version History
* 21/10/2025 **First Checkin**

## License
✅ Do what you want with this, any improvements would be helpfull

## 💬 Disclaimer
This software is provided "as is," without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose, and non-infringement.

The use of this software is entirely at your own risk. In no event shall the authors or copyright holders be liable for any claim, damages, or other liabilities, whether in an action of contract, tort, or otherwise, arising from, out of, or in connection with the software or the use or other dealings in the software.
