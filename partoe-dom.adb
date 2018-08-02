with Ada.Strings.Fixed;

with Partoe;

package body Partoe.DOM is

   type Partoe_Doc_Loader is
     new Partoe.Partoe_Document with
      record
         Doc               : Partoe_Document;
         Current_Node      : Partoe_Node;
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
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Attribute of Element.Attributes loop
         if Attribute.Name = Name then
            return Attribute;
         end if;
      end loop;
      return null;
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
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Child of Element.Children loop
         if Child.Name = Name then
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
      use type Ada.Strings.Unbounded.Unbounded_String;
      Count  : Natural := 0;
      Result : Array_Of_Partoe_Nodes (1 .. Element.Child_Nodes.Last_Index);
   begin
      for Child of Element.Children loop
         if Child.Name = Name then
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

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
     (Element : Root_Partoe_Node;
      Name    : String)
      return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Attr of Element.Attributes loop
         if Attr.Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Has_Attribute;

   ----------
   -- Load --
   ----------

   function Load (Path : String) return Partoe_Document is
      Doc : Partoe_Doc_Loader;
   begin
      Doc.Doc := new Root_Partoe_Document;
      Doc.Current_Node := Partoe_Node (Doc.Doc);

      Partoe.Read (Doc, Path);

      return Doc.Doc;
   end Load;

   ----------
   -- Name --
   ----------

   function Name (Element : Root_Partoe_Element) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Element.Name);
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
           (Document.Current_Line,
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
      use Ada.Strings.Unbounded;
      pragma Assert (Document.Current_Node.Name = Tag_Name);
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
      Document.Current_Node.Text :=
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
      New_Node.Line := Document.Current_Line;
      New_Node.Col  := Document.Current_Column;
      New_Node.Parent := Document.Current_Node;
      New_Node.Name := Ada.Strings.Unbounded.To_Unbounded_String (Tag_Name);
      Document.Current_Node.Child_Nodes.Append (New_Node);
      Document.Current_Node := New_Node;
   end On_Open_Tag;

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

   function Text (Element : Root_Partoe_Element) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Element.Text);
   end Text;

end Partoe.DOM;
