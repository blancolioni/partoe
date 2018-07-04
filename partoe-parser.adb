with Ada.Text_IO;

with Partoe.Parser.Tokens;                use Partoe.Parser.Tokens;
with Partoe.Parser.Manual;                use Partoe.Parser.Manual;

package body Partoe.Parser is

   procedure Parse_Node
     (Document : in out Partoe_Document'Class);

   ----------------
   -- Parse_Node --
   ----------------

   procedure Parse_Node
     (Document : in out Partoe_Document'Class)
   is
   begin
      Document.Line := Tok_Line;
      if Tok = Tok_Inline_Text then
         Document.On_Inline_Text (Tok_Text);
         Scan;
      elsif Tok = Tok_Begin_Open_Tag then
         Scan;
         if Tok = Tok_Identifier then
            declare
               Tag_Name : constant String := Tok_Text;
            begin
               Document.Line := Tok_Line;
               Document.On_Open_Tag (Tok_Text);
               Scan;

               while Tok /= Tok_End_Tag
                 and then Tok /= Tok_Short_End_Tag
               loop
                  if Tok = Tok_Identifier then
                     declare
                        Attribute_Name : constant String := Tok_Text;
                     begin
                        Document.Line := Tok_Line;
                        Scan;
                        if Tok = Tok_Equal then
                           Scan;
                        else
                           Fatal_Error ("missing '='");
                        end if;
                        if Tok = Tok_String_Constant then
                           Document.On_Attribute (Attribute_Name, Tok_Text);
                           Scan;
                        else
                           Fatal_Error ("missing attribute value");
                        end if;
                     end;
                  else
                     Fatal_Error ("missing attribute, > or /> near " &
                                  Token'Image (Tok) & " [" & Tok_Text &
                                  "]");
                  end if;
               end loop;

               if Tok = Tok_End_Tag then
                  Scan;

                  while Tok /= Tok_Begin_Close_Tag loop
                     Parse_Node (Document);
                  end loop;

                  Scan;

                  if Tok = Tok_Identifier and then
                    Tok_Text = Tag_Name
                  then
                     Document.Line := Tok_Line;
                     Document.On_Close_Tag (Tok_Text);
                     Scan;
                     if Tok = Tok_End_Tag then
                        Scan;
                     else
                        Fatal_Error ("missing '>'");
                     end if;
                  end if;
               elsif Tok = Tok_Short_End_Tag then
                  Document.Line := Tok_Line;
                  Document.On_Close_Tag (Tag_Name);
                  Scan;
               else
                  Fatal_Error ("missing '>' or '/>'");
               end if;
            end;
         else
            Fatal_Error ("missing tag name");
         end if;
      else
         Fatal_Error ("missing tag");
      end if;
   end Parse_Node;

   ----------------
   -- Run_Parser --
   ----------------

   procedure Run_Parser
     (Document : in out Partoe_Document'Class;
      Path     : String)
   is
   begin
      Open (Path);

      if Tok = Tok_Open_Meta_Tag then
         Scan;
         while Tok /= Tok_Close_Meta_Tag loop
            Scan;
         end loop;
         Scan;
      end if;

      while Tok /= Tok_End_Of_File loop
         Parse_Node (Document);
      end loop;

      Close;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("exception: tok = " & Token'Image (Tok) &
                               " [" & Tok_Text & "]");
   end Run_Parser;

end Partoe.Parser;
