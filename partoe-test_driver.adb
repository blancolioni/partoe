with Ada.Text_IO;

with Partoe.Count_Documents;

procedure Partoe.Test_Driver is
   Doc : Partoe.Count_Documents.Count_Partoe_Document :=
     (Partoe_Document with Node_Count => 0);
begin
   --  Doc.Read ("test.Partoe");
   Doc.Read ("/home/fraser/repos/audley/trunk/config/cyc/opencyc-latest.owl");

   Ada.Text_IO.Put_Line ("Found" &
                         Natural'Image (Doc.Node_Count) &
                         " nodes");
   Ada.Text_IO.Put_Line ("Done");

end Partoe.Test_Driver;
