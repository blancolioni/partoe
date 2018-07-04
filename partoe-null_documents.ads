package Partoe.Null_Documents is

   type Null_Partoe_Document is
     new Partoe_Document with null record;

   procedure On_Open_Tag
     (Document : in out Null_Partoe_Document;
      Tag_Name : String)
   is null;

   procedure On_Close_Tag
     (Document : in out Null_Partoe_Document)
   is null;

   procedure On_Attribute
     (Document        : in out Null_Partoe_Document;
      Attribute_Name  : String;
      Attribute_Value : String)
   is null;

end Partoe.Null_Documents;
