with Partoe.Parser.Tokens;                 use Partoe.Parser.Tokens;

private package Partoe.Parser.Manual is

   procedure Open (Path : String);
   procedure Close;

   procedure Scan;

   function Tok return Token;
   function Tok_Text return String;
   function Tok_Line return Positive;

   procedure Fatal_Error (Message : String);
   procedure Error (Message : String);

end Partoe.Parser.Manual;
