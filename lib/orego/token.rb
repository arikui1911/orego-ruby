module Orego
  Token = Struct.new(:tag, :value, :location)

  %i[
    EOF
    NEWLINE

    IDENTIFIER
    LITERAL_INT
    LITERAL_FLOAT
    LITERAL_STRING

    LPAREN
    RPAREN
    LBRACKET
    RBRACKET
    LBRACE
    RBRACE
    ARROW
    COMMA
    SEMICOLON
    COLON
    
    LET
    EQ
    NE
    GE
    LE
    GT
    LT
    ADD
    SUB
    MUL
    DIV
    MOD
    ADD_LET
    SUB_LET
    MUL_LET
    DIV_LET
    MOD_LET
    BANG
    
    KW_DEF
    KW_IF
    KW_ELSE
    KW_ELSIF
    KW_WHILE
    KW_BREAK
    KW_CONTINUE
    KW_RETURN
    KW_TRUE
    KW_FALSE
    KW_NIL   
  ].each{|s| Token.const_set s, s }
end
