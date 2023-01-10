private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package Partoe.DOM is

   type Root_Partoe_Element is abstract tagged private;

   type Partoe_Element is access all Root_Partoe_Element'Class;

   function Name (Element : Root_Partoe_Element'Class) return String;
   function Text (Element : Root_Partoe_Element'Class) return String;
   function Has_Name (Element : Root_Partoe_Element'Class;
                      Name    : String)
                      return Boolean;
   function File_Path (Element : Root_Partoe_Element) return String;

   type Root_Partoe_Attribute is new Root_Partoe_Element with private;

   type Partoe_Attribute is access all Root_Partoe_Attribute'Class;

   function Create_Attribute
     (Name  : String;
      Value : String)
      return Partoe_Attribute;

   type Root_Partoe_Node is new Root_Partoe_Element with private;

   type Partoe_Node is access all Root_Partoe_Node'Class;

   type Array_Of_Partoe_Nodes is array (Positive range <>) of Partoe_Node;

   function Create
     (Name : String)
      return Partoe_Node;

   function Parent (This : Root_Partoe_Node'Class) return Partoe_Node;

   function Child_Count (Element : Root_Partoe_Node) return Natural;
   function Child (Element : Root_Partoe_Node;
                   Index   : Positive)
                   return Partoe_Node;
   function Child (Element : Root_Partoe_Node;
                   Name    : String)
                   return Partoe_Node;

   function Children (Element : Root_Partoe_Node;
                      Name    : String)
                      return Array_Of_Partoe_Nodes;

   function Children (Element : Root_Partoe_Node)
                      return Array_Of_Partoe_Nodes;

   procedure Append
     (Node  : not null access Root_Partoe_Node;
      Child : Partoe_Node);

   procedure Append
     (Node      : not null access Root_Partoe_Node;
      Attribute : Partoe_Attribute);

   function Attribute_Count (Element : Root_Partoe_Node) return Natural;

   function Has_Attribute
     (Element : Root_Partoe_Node;
      Name    : String)
      return Boolean;

   function Attribute (Element : Root_Partoe_Node;
                       Index   : Positive)
                       return Partoe_Attribute;

   function Attribute (Element : Root_Partoe_Node;
                       Name    : String)
                       return Partoe_Attribute;

   function Attribute
     (Node          : Root_Partoe_Node;
      Name          : String)
      return String
     with Pre => Node.Has_Attribute (Name);

   function Attribute
     (Node          : Root_Partoe_Node;
      Name          : String;
      Default_Value : String)
      return String;

   function Select_Node (Element : not null access Root_Partoe_Node;
                         Path    : String)
                         return Partoe_Node;

   function Select_Nodes (Element : not null access Root_Partoe_Node;
                          Path    : String)
                          return Array_Of_Partoe_Nodes;

   function Outer_XML (Node : not null access Root_Partoe_Node) return String;

   procedure Iterate
     (Root : not null access Root_Partoe_Node'Class;
      Process : not null access
        procedure (Node : Partoe_Node));

   type Root_Partoe_Document is new Root_Partoe_Node with private;

   type Partoe_Document is access all Root_Partoe_Document'Class;

   function Load (Path : String) return Partoe_Document;

private

   package Partoe_Node_Vectors is
     new Ada.Containers.Vectors (Positive, Partoe_Node);

   package Partoe_Attribute_Vectors is
     new Ada.Containers.Vectors (Positive, Partoe_Attribute);

   type Root_Partoe_Element is abstract tagged
      record
         File         : Ada.Strings.Unbounded.Unbounded_String;
         Line         : Natural;
         Col          : Natural;
         Element_Name : Ada.Strings.Unbounded.Unbounded_String;
         Element_Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function File_Path (Element : Root_Partoe_Element) return String
   is (Ada.Strings.Unbounded.To_String (Element.File));

   type Root_Partoe_Attribute is new Root_Partoe_Element with null record;

   type Root_Partoe_Node is new Root_Partoe_Element with
      record
         Parent      : Partoe_Node;
         Attributes  : Partoe_Attribute_Vectors.Vector;
         Child_Nodes : Partoe_Node_Vectors.Vector;
      end record;

   function Parent (This : Root_Partoe_Node'Class) return Partoe_Node
   is (This.Parent);

   type Root_Partoe_Document is new Root_Partoe_Node with null record;

end Partoe.DOM;
