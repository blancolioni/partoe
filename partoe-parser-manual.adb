with Ada.Text_IO;

package body Partoe.Parser.Manual is

   File : Ada.Text_IO.File_Type;

   Line_Number     : Natural   := 0;
   Current_Char    : Character := ' ';
   Current_Tok     : Token := Tok_None;
   Within_Tag      : Boolean := False;
   Within_Meta_Tag : Boolean := False;
   pragma Unreferenced (Within_Meta_Tag);

   Max_Text_Length : constant := 65536;
   Text_Buffer     : String (1 .. Max_Text_Length);
   Text_Length     : Natural := 0;
   End_Of_File     : Boolean := False;

   Current_Line    : String (1 .. Max_Text_Length);
   Line_Length     : Natural  := 0;
   Line_Index      : Positive := 1;

   procedure Skip;
   procedure Skip_Whitespace;

   function At_Whitespace return Boolean;
   function At_Identifier return Boolean;

   function Look_Ahead_And_Skip (Match : String) return Boolean;

   -------------------
   -- At_Identifier --
   -------------------

   function At_Identifier return Boolean is
   begin
      case Current_Char is
         when 'a' .. 'z' =>
            return True;
         when 'A' .. 'Z' =>
            return True;
         when '_' | ':' | '-' =>
            return True;
         when '0' .. '9' =>
            return True;
         when others =>
            return False;
      end case;
   end At_Identifier;

   -------------------
   -- At_Whitespace --
   -------------------

   function At_Whitespace return Boolean is
   begin
      return Current_Char = ' '
        or else Character'Pos (Current_Char) = 9
        or else Character'Pos (Current_Char) = 10
        or else Character'Pos (Current_Char) = 13
        or else Character'Pos (Current_Char) = 12;
   end At_Whitespace;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Ada.Text_IO.Close (File);
   end Close;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        ("Line" & Line_Number'Img & ": " & Current_Line (1 .. Line_Length));
      Ada.Text_IO.Put_Line ("Error: " & Message);
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error (Message : String) is
   begin
      Error (Message);
      raise Constraint_Error with "Fatal error";
   end Fatal_Error;

   -------------------------
   -- Look_Ahead_And_Skip --
   -------------------------

   function Look_Ahead_And_Skip (Match : String) return Boolean is
   begin
      if Line_Length - Line_Index >= Match'Length - 1
        and then Current_Line
        (Line_Index .. Line_Index + Match'Length - 1) = Match
      then
         Line_Index := Line_Index + Match'Length - 1;
         return True;
      else
         return False;
      end if;
   end Look_Ahead_And_Skip;

   ----------
   -- Open --
   ----------

   procedure Open (Path : String) is
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);
      Line_Number     := 0;
      Current_Char     := ' ';
      Current_Tok      := Tok_None;
      Within_Tag       := False;

      Text_Length      := 0;
      End_Of_File      := False;

      Line_Length  := 0;
      Line_Index  := 1;

      Scan;
   end Open;

   ----------
   -- Scan --
   ----------

   procedure Scan is
      Got_Non_Whitespace : Boolean := False;
   begin

      Text_Length := 0;

      if End_Of_File then
         Current_Tok := Tok_End_Of_File;
         return;
      end if;

      if not Within_Tag then

         Skip_Whitespace;

         if Look_Ahead_And_Skip ("<![CDATA[") then

            while not Look_Ahead_And_Skip ("]]>") loop

               Text_Length := Text_Length + 1;
               Text_Buffer (Text_Length) := Current_Char;
               Skip;

            end loop;

            Got_Non_Whitespace := True;

         elsif Look_Ahead_And_Skip ("<!--") then
            loop
               while not Look_Ahead_And_Skip ("-->") loop

                  Skip;

               end loop;

               Skip;

               Skip_Whitespace;

               exit when not Look_Ahead_And_Skip ("<!--");
            end loop;

            Got_Non_Whitespace := False;

         else

            while Current_Char /= '<'
              and then not End_Of_File
            loop
               Text_Length := Text_Length + 1;
               Text_Buffer (Text_Length) := Current_Char;
               if not Got_Non_Whitespace
                 and then not At_Whitespace
               then
                  Got_Non_Whitespace := True;
               end if;
               Skip;
            end loop;
         end if;

         if Got_Non_Whitespace then
            Current_Tok := Tok_Inline_Text;
            --  Ada.Text_IO.Put_Line (Tok_Text);
            return;
         end if;
      end if;

      Skip_Whitespace;

      case Current_Char is
         when '<' =>
            Skip;
            Within_Tag := True;
            if Current_Char = '?' then
               Skip;
               Within_Meta_Tag := True;
               Current_Tok := Tok_Open_Meta_Tag;
            elsif Current_Char = '/' then
               Skip;
               Current_Tok := Tok_Begin_Close_Tag;
            else
               Current_Tok := Tok_Begin_Open_Tag;
            end if;
         when '?' =>
            Skip;
            if Current_Char = '>' then
               Skip;
               Current_Tok := Tok_Close_Meta_Tag;
               Within_Tag := False;
            else
               Fatal_Error ("bad tag");
            end if;
         when '/' =>
            Skip;
            if Current_Char = '>' then
               Skip;
               Current_Tok := Tok_Short_End_Tag;
               Within_Tag := False;
            else
               Fatal_Error ("bad use of /");
            end if;
         when '=' =>
            Skip;
            Current_Tok := Tok_Equal;
         when '>' =>
            Skip;
            Current_Tok := Tok_End_Tag;
            Within_Tag := False;
         when '"' =>
            Skip;
            Text_Length := 0;
            while not End_Of_File
              and then Current_Char /= '"'
            loop
               Text_Length := Text_Length + 1;
               Text_Buffer (Text_Length) := Current_Char;
               Skip;
            end loop;
            if Current_Char = '"' then
               Skip;
            else
               Error ("unterminated string");
            end if;

            Current_Tok := Tok_String_Constant;
         when others =>
            Text_Length := 0;
            while At_Identifier loop
               Text_Length := Text_Length + 1;
               Text_Buffer (Text_Length) := Current_Char;
               Skip;
            end loop;
            Current_Tok := Tok_Identifier;
      end case;

   end Scan;

   ----------
   -- Skip --
   ----------

   procedure Skip is
   begin
      Line_Index := Line_Index + 1;
      while Line_Index > Line_Length loop
         if Ada.Text_IO.End_Of_File (File) then
            End_Of_File := True;
            Current_Char := ' ';
            return;
         else
            Line_Number := Line_Number + 1;
            Ada.Text_IO.Get_Line (File, Current_Line, Line_Length);
            --  Ada.Text_IO.Put_Line (Current_Line (1 .. Line_Length));
            Line_Index := 1;
         end if;
      end loop;
      Current_Char := Current_Line (Line_Index);
   end Skip;

   ---------------------
   -- Skip_Whitespace --
   ---------------------

   procedure Skip_Whitespace is
   begin
      while not End_Of_File
        and then At_Whitespace
      loop
         Skip;
      end loop;
   end Skip_Whitespace;

   ---------
   -- Tok --
   ---------

   function Tok return Token is
   begin
      return Current_Tok;
   end Tok;

   --------------
   -- Tok_Line --
   --------------

   function Tok_Line return Positive is
   begin
      return Line_Number;
   end Tok_Line;

   --------------
   -- Tok_Text --
   --------------

   function Tok_Text return String is
   begin
      return Text_Buffer (1 .. Text_Length);
   end Tok_Text;

end Partoe.Parser.Manual;
