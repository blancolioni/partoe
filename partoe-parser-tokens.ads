private package Partoe.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_String_Constant,

       Tok_Inline_Text,

       Tok_Begin_Open_Tag, Tok_Begin_Close_Tag,
       Tok_End_Tag, Tok_Short_End_Tag,
       Tok_Open_Meta_Tag, Tok_Close_Meta_Tag,
       Tok_Equal);

end Partoe.Parser.Tokens;
