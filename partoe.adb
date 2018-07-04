with Partoe.Parser;

package body Partoe is

   --------------------
   -- Current_Column --
   --------------------

   function Current_Column
     (Document : Partoe_Document'Class)
      return Positive
   is
   begin
      return Document.Col;
   end Current_Column;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line
     (Document : Partoe_Document'Class)
      return Positive
   is
   begin
      return Document.Line;
   end Current_Line;

   ----------
   -- Read --
   ----------

   procedure Read
     (Document : in out Partoe_Document'Class;
      Path     : String)
   is
   begin
      Partoe.Parser.Run_Parser (Document, Path);
   end Read;

end Partoe;
