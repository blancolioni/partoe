with GCS.Lexer;
with GCS.Styles;

with Partoe.Parser.Tokens;                 use Partoe.Parser.Tokens;

pragma Elaborate_All (GCS.Lexer);

private package Partoe.Parser.Lexical is
  new GCS.Lexer (Token              => Token,
                 Tok_None           => Tok_None,
                 Tok_End_Of_File    => Tok_End_Of_File,
                 Tok_Bad_Character  => Tok_Bad_Character,
                 Tok_Identifier     => Tok_Identifier,
                 Tok_String         => Tok_String_Constant,
                 Tok_Character      => Tok_String_Constant,
                 Tok_Integer        => Tok_None,
                 Tok_Float          => Tok_None,
                 First_Keyword      => Tok_None,
                 Keywords           => "",
                 First_Symbol       => Tok_Begin_Open_Tag,
                 Symbols            => "< </ > /> <? ?> =",
                 Identifier_Start   => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "_",
                 Identifier_Body    => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "0123456789" &
                                       "_",
                 Line_Comment_Start => "",
                 Block_Comment_Start => "<!--",
                 Block_Comment_End   => "-->",
                 Properties         => (GCS.Styles.Multi_Characters => True,
                                        others => False));



