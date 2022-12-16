with Ada.Directories;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

with Partoe;

package body Partoe.DOM is

   type Partoe_Doc_Loader is
     new Partoe.Partoe_Document with
      record
         Doc          : Partoe_Document;
         Current_Node : Partoe_Node;
         File_Path    : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure On_Open_Tag
     (Document : in out Partoe_Doc_Loader;
      Tag_Name : String);

   overriding procedure On_Close_Tag
     (Document : in out Partoe_Doc_Loader;
      Tag_Name : String);

   overriding procedure On_Attribute
     (Document        : in out Partoe_Doc_Loader;
      Attribute_Name  : String;
      Attribute_Value : String);

   overriding procedure On_Inline_Text
     (Document : in out Partoe_Doc_Loader;
      Text     : in     String);

   function Has_Name (Element : Root_Partoe_Element'Class;
                      Name    : String)
                      return Boolean
   is (Ada.Strings.Fixed.Equal_Case_Insensitive
       (Left  => Element.Name,
        Right => Name));

   ------------
   -- Append --
   ------------

   procedure Append
     (Node  : not null access Root_Partoe_Node;
      Child : Partoe_Node)
   is
   begin
      Child.Parent := Partoe_Node (Node);
      Node.Child_Nodes.Append (Child);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Node      : not null access Root_Partoe_Node;
      Attribute : Partoe_Attribute)
   is
   begin
      Node.Attributes.Append (Attribute);
   end Append;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Element : Root_Partoe_Node;
      Index   : Positive)
      return Partoe_Attribute
   is
   begin
      return Element.Attributes (Index);
   end Attribute;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Element : Root_Partoe_Node;
      Name    : String)
      return Partoe_Attribute
   is
   begin
      for Attribute of Element.Attributes loop
         if Attribute.Name = Name then
            return Attribute;
         end if;
      end loop;
      return null;
   end Attribute;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Node          : Root_Partoe_Node;
      Name          : String)
      return String
   is
   begin
      return Node.Attribute (Name, "");
   end Attribute;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Node          : Root_Partoe_Node;
      Name          : String;
      Default_Value : String)
      return String
   is
   begin
      if Node.Has_Attribute (Name) then
         return Node.Attribute (Name).Text;
      else
         return Default_Value;
      end if;
   end Attribute;

   ---------------------
   -- Attribute_Count --
   ---------------------

   function Attribute_Count (Element : Root_Partoe_Node) return Natural is
   begin
      return Element.Attributes.Last_Index;
   end Attribute_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Element : Root_Partoe_Node;
      Index   : Positive)
      return Partoe_Node
   is
   begin
      return Element.Child_Nodes.Element (Index);
   end Child;

   -----------
   -- Child --
   -----------

   function Child
     (Element : Root_Partoe_Node;
      Name    : String)
      return Partoe_Node
   is
   begin
      for Child of Element.Children loop
         if Child.Has_Name (Name) then
            return Child;
         end if;
      end loop;
      return null;
   end Child;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Element : Root_Partoe_Node) return Natural is
   begin
      return Element.Child_Nodes.Last_Index;
   end Child_Count;

   --------------
   -- Children --
   --------------

   function Children
     (Element : Root_Partoe_Node;
      Name    : String)
      return Array_Of_Partoe_Nodes
   is
      Count  : Natural := 0;
      Result : Array_Of_Partoe_Nodes (1 .. Element.Child_Nodes.Last_Index);
   begin
      for Child of Element.Children loop
         if Child.Has_Name (Name) then
            Count := Count + 1;
            Result (Count) := Child;
         end if;
      end loop;
      return Result (1 .. Count);
   end Children;

   --------------
   -- Children --
   --------------

   function Children
     (Element : Root_Partoe_Node)
      return Array_Of_Partoe_Nodes
   is
      Result : Array_Of_Partoe_Nodes (1 .. Element.Child_Nodes.Last_Index);
   begin
      for I in Result'Range loop
         Result (I) := Element.Child_Nodes.Element (I);
      end loop;
      return Result;
   end Children;

   ------------
   -- Create --
   ------------

   function Create
     (Name : String)
      return Partoe_Node
   is
   begin
      return Node : constant Partoe_Node := new Root_Partoe_Node'
        (Line         => 0,
         Col          => 0,
         Element_Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Element_Text => Ada.Strings.Unbounded.Null_Unbounded_String,
         Parent       => null,
         Attributes   => <>,
         Child_Nodes  => <>);
   end Create;

   ----------------------
   -- Create_Attribute --
   ----------------------

   function Create_Attribute
     (Name  : String;
      Value : String)
      return Partoe_Attribute
   is
   begin
      return Attribute : constant Partoe_Attribute :=
        new Root_Partoe_Attribute'
          (Line         => 0,
           Col          => 0,
           Element_Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
           Element_Text => Ada.Strings.Unbounded.To_Unbounded_String (Value));
   end Create_Attribute;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
     (Element : Root_Partoe_Node;
      Name    : String)
      return Boolean
   is
   begin
      for Attr of Element.Attributes loop
         if Attr.Has_Name (Name) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Attribute;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Root    : not null access Root_Partoe_Node'Class;
      Process : not null access
        procedure (Node : Partoe_Node))
   is
   begin
      Process (Partoe_Node (Root));
      for Child of Root.Children loop
         Iterate (Child, Process);
      end loop;
   end Iterate;

   ----------
   -- Load --
   ----------

   function Load (Path : String) return Partoe_Document is
      Doc : Partoe_Doc_Loader;
      Containing_Path : constant String :=
                          Ada.Directories.Containing_Directory (Path);
   begin
      Doc.File_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Containing_Path);
      Doc.Doc := new Root_Partoe_Document;
      Doc.Current_Node := Partoe_Node (Doc.Doc);

      Partoe.Read (Doc, Path);

      return Doc.Doc;
   end Load;

   ----------
   -- Name --
   ----------

   function Name (Element : Root_Partoe_Element'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Element.Element_Name);
   end Name;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Document        : in out Partoe_Doc_Loader;
      Attribute_Name  : String;
      Attribute_Value : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Document.Current_Node.Attributes.Append
        (new Root_Partoe_Attribute'
           (Document.File_Path,
            Document.Current_Line,
            Document.Current_Column,
            To_Unbounded_String (Attribute_Name),
            To_Unbounded_String (Attribute_Value)));
   end On_Attribute;

   ------------------
   -- On_Close_Tag --
   ------------------

   overriding procedure On_Close_Tag
     (Document : in out Partoe_Doc_Loader;
      Tag_Name : String)
   is
      pragma Assert (Document.Current_Node.Has_Name (Tag_Name));
   begin
      Document.Current_Node := Document.Current_Node.Parent;
   end On_Close_Tag;

   --------------------
   -- On_Inline_Text --
   --------------------

   overriding procedure On_Inline_Text
     (Document : in out Partoe_Doc_Loader;
      Text     : in     String)
   is
   begin
      Document.Current_Node.Element_Text :=
        Ada.Strings.Unbounded.To_Unbounded_String (Text);
   end On_Inline_Text;

   -----------------
   -- On_Open_Tag --
   -----------------

   overriding procedure On_Open_Tag
     (Document : in out Partoe_Doc_Loader;
      Tag_Name : String)
   is
      New_Node : constant Partoe_Node :=
                   new Root_Partoe_Node;
   begin
      New_Node.File := Document.File_Path;
      New_Node.Line := Document.Current_Line;
      New_Node.Col  := Document.Current_Column;
      New_Node.Parent := Document.Current_Node;
      New_Node.Element_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Tag_Name);
      Document.Current_Node.Child_Nodes.Append (New_Node);
      Document.Current_Node := New_Node;
   end On_Open_Tag;

   ---------------
   -- Outer_XML --
   ---------------

   function Outer_XML
     (Node : not null access Root_Partoe_Node)
      return String
   is

      function Attributes (Start : Positive) return String
      is (if Start <= Node.Attribute_Count
          then " " & Node.Attribute (Start).Name
          & "=""" & Node.Attribute (Start).Text & """"
          & Attributes (Start + 1)
          else "");

      function Child_Nodes (Start : Positive) return String
      is (if Start <= Node.Child_Count
          then Node.Child (Start).Outer_XML & Child_Nodes (Start + 1)
          else "");

      function Open_Tag return String
      is ("<" & Node.Name & Attributes (1) & ">");

      function Close_Tag return String
      is ("</" & Node.Name & ">");

   begin
      return Open_Tag & Child_Nodes (1) & Node.Text & Close_Tag;
   end Outer_XML;

   -----------------
   -- Select_Node --
   -----------------

   function Select_Node
     (Element : not null access Root_Partoe_Node;
      Path    : String)
      return Partoe_Node
   is
      Ext_Path : constant String := Path & "/";
      Start : Positive := Ext_Path'First;
      Node  : Partoe_Node := Partoe_Node (Element);
   begin
      for I in Ext_Path'Range loop
         if Ext_Path (I) = '/' then
            declare
               Name : constant String := Ext_Path (Start .. I - 1);
            begin
               if Name = "" then
                  null;
               else
                  Node := Node.Child (Name);
                  exit when Node = null;
               end if;
            end;
            Start := I + 1;
         end if;
      end loop;
      return Node;
   end Select_Node;

   ------------------
   -- Select_Nodes --
   ------------------

   function Select_Nodes
     (Element : not null access Root_Partoe_Node;
      Path    : String)
      return Array_Of_Partoe_Nodes
   is
      No_Result : Array_Of_Partoe_Nodes (1 .. 0);
      Last_Sep  : constant Natural :=
                    Ada.Strings.Fixed.Index
                      (Source  => Path,
                       Pattern => "/",
                       Going   => Ada.Strings.Backward);
      Child     : constant Partoe_Node := Element.Select_Node (Path);
   begin
      if Child = null then
         return No_Result;
      elsif Child.Parent = null then
         return (1 => Child);
      else
         return Child.Parent.Children (Path (Last_Sep + 1 .. Path'Last));
      end if;
   end Select_Nodes;

   ----------
   -- Text --
   ----------

   function Text (Element : Root_Partoe_Element'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Element.Element_Text);
   end Text;

end Partoe.DOM;
