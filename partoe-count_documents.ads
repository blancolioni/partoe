package Partoe.Count_Documents is

   type Count_Partoe_Document is
     new Partoe_Document with
      record
         Node_Count : Natural;
      end record;

   procedure On_Open_Tag
     (Document : in out Count_Partoe_Document;
      Tag_Name : String);

end Partoe.Count_Documents;
