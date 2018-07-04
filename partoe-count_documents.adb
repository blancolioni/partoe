package body Partoe.Count_Documents is

   -----------------
   -- On_Open_Tag --
   -----------------

   procedure On_Open_Tag
     (Document : in out Count_Partoe_Document;
      Tag_Name : String)
   is
   begin
      Document.Node_Count := Document.Node_Count + 1;
   end On_Open_Tag;

end Partoe.Count_Documents;
