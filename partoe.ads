package Partoe is

   type Partoe_Document is tagged limited private;

   procedure On_Open_Tag
     (Document : in out Partoe_Document;
      Tag_Name : String)
   is null;

   procedure On_Close_Tag
     (Document : in out Partoe_Document;
      Tag_Name : String)
   is null;

   procedure On_Attribute
     (Document        : in out Partoe_Document;
      Attribute_Name  : String;
      Attribute_Value : String)
   is null;

   procedure On_Inline_Text
     (Document : in out Partoe_Document;
      Text     : in     String)
   is null;

   function Current_Line
     (Document : Partoe_Document'Class)
      return Positive;

   function Current_Column
     (Document : Partoe_Document'Class)
      return Positive;

   procedure Read
     (Document : in out Partoe_Document'Class;
      Path     : String);

private

   type Partoe_Document is tagged limited
      record
         Line, Col : Positive := 1;
      end record;

end Partoe;
