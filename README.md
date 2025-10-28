# Delphi Dabbling
The code in this repository are a collection of bits n bobs that I am fed up of re-creating in every application!

## uHelperListView
This is essentially the Component TListViewZaaz (modified TListView) as Class Helpers, hence no requirement to install a component! (Makes moving the code to a different IDE simpler)

## Authors
😎 Mark Richards [@MascotZombie](https://thezombiecoders.co.uk)

## Version History
* 21/10/2025 **First Checkin**

## License
✅ Do what you want with this, any improvements would be helpfull

## Launch a second form

### FMX (Android to avoid thread issues)
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
### FMX with error catching
procedure TMainForm.btnShowSecondForm(Sender: TObject);
var
   frm: TTheSecondForm;
begin
   try
      frm := TTheSecondForm.Create(nil);
      frm.ShowModal(
      procedure(ModalResult: TModalResult)
      begin
         try
            // Put something here to do AFTER form has closed and come back e.g.
            // RefreshFirstPage;
         except
            on E: Exception do
               ShowMessage('Error after closing form: ' + E.Message);
         end;
         frm.DisposeOf; // Ensures proper cleanup on mobile
      end
      );
   except
      on E: Exception do
         ShowMessage('Error showing form: ' + E.Message);
   end;
end;

### VCL
procedure TMainForm.btnShowSecondForm(Sender: TObject);
var
   frm: TTheSecondForm;
begin
   frm := TTheSecondForm.Create(self);
   try
      frm.ShowModal;
   finally
      frm.Free;
   end;
end;

## 💬 Disclaimer
This software is provided "as is," without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose, and non-infringement.

The use of this software is entirely at your own risk. In no event shall the authors or copyright holders be liable for any claim, damages, or other liabilities, whether in an action of contract, tort, or otherwise, arising from, out of, or in connection with the software or the use or other dealings in the software.
