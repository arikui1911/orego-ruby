require 'test/unit'
require 'orego/lexer'

class TestLexer < Test::Unit::TestCase
  data(
    'left paren' => ['(', Orego::Token::LPAREN],
    'right paren' => [')', Orego::Token::RPAREN],
    'left bracket' => ['[', Orego::Token::LBRACKET],
    'right bracket' => [']', Orego::Token::RBRACKET],
    'left brace' => ['{', Orego::Token::LBRACE],
    'arrow' => ['->', Orego::Token::ARROW],
    'comma' => [',', Orego::Token::COMMA],
    'semicolon' => [';', Orego::Token::SEMICOLON],
    'colon' => [':', Orego::Token::COLON],
    'let' => ['=', Orego::Token::LET],
    'eq' => ['==', Orego::Token::EQ],
    'ne' => ['!=', Orego::Token::NE],
    'ge' => ['>=', Orego::Token::GE],
    'le' => ['<=', Orego::Token::LE],
    'gt' => ['>', Orego::Token::GT],
    'lt' => ['<', Orego::Token::LT],
    'add' => ['+', Orego::Token::ADD],
    'sub' => ['-', Orego::Token::SUB],
    'mul' => ['*', Orego::Token::MUL],
    'div' => ['/', Orego::Token::DIV],
    'mod' => ['%', Orego::Token::MOD],
    'add let' => ['+=', Orego::Token::ADD_LET],
    'sub let' => ['-=', Orego::Token::SUB_LET],
    'mul let' => ['*=', Orego::Token::MUL_LET],
    'div let' => ['/=', Orego::Token::DIV_LET],
    'mod let' => ['%=', Orego::Token::MOD_LET],
    'bang' => ['!', Orego::Token::BANG],
    'def' => ['def', Orego::Token::KW_DEF],
    'if' => ['if', Orego::Token::KW_IF],
    'else' => ['else', Orego::Token::KW_ELSE],
    'elsif' => ['elsif', Orego::Token::KW_ELSIF],
    'while' => ['while', Orego::Token::KW_WHILE],
    'break' => ['break', Orego::Token::KW_BREAK],
    'continue' => ['continue', Orego::Token::KW_CONTINUE],
    'return' => ['return', Orego::Token::KW_RETURN],
    'true' => ['true', Orego::Token::KW_TRUE],
    'false' => ['false', Orego::Token::KW_FALSE],
    'nil' => ['nil', Orego::Token::KW_NIL],
  )
  test 'lex statics' do |(src, tag)|
    assert_equal(
      Orego::Token.new(tag, src, Orego::Location.new(0, 0, 0, src.length-1)),
      Orego::Lexer.new(src).next_token,
    )
  end

  sub_test_case 'identifier' do
    data(
      'basic one' => ['hoge', :hoge],
      'underbar' => ['_', :_],
      'one char' => ['x', :x],
      'with number' => ['x123', :x123],
      'conc underbar' => ['hoge_piyo', :hoge_piyo],
      'hyphen NG' => ['hoge-piyo', :hoge, 3],
    )
    test 'lex' do |(src, val, ec)|
      ec ||= src.length - 1
      assert_equal Orego::Token.new(Orego::Token::IDENTIFIER, val, Orego::Location.new(0, 0, 0, ec)), Orego::Lexer.new(src).next_token
    end
  end

  sub_test_case 'int literal' do
    data(
      'basic' => ['666', 666],
      'zero' => ['0', 0],
      'consecutive zero' => ['0000', 0, 0],
    )
    test 'lex' do |(src, val, ec)|
      ec ||= src.length - 1
      assert_equal(
        Orego::Token.new(Orego::Token::LITERAL_INT, val, Orego::Location.new(0, 0, 0, ec)),
        Orego::Lexer.new(src).next_token,
      )
    end
  end

  sub_test_case 'float literal' do
    data(
      'basic' => ['1.23', 1.23],
      'zero' => ['0.0', 0.0],
      'consecutive zero in float part' => ['1.001', 1.001],
    )
    test 'lex' do |(src, val, ec)|
      ec ||= src.length - 1
      assert_equal(
        Orego::Token.new(Orego::Token::LITERAL_FLOAT, val, Orego::Location.new(0, 0, 0, ec)),
        Orego::Lexer.new(src).next_token,
      )
    end
  end

  sub_test_case 'string literal' do
    data(
      'basic' => ['"Hello"', 'Hello', 0, 0, 0, 6],
      'empty' => ['""', '', 0, 0, 0, 1],
      'escape sequences' => [%Q`"\\t\\v\\n\\r\\f\\b\\a\\e\\s"`, "\t\v\n\r\f\b\a\e\s", 0, 0, 0, 19],
      'multi line' => [<<~EOS, "Hello,\nOrego!", 0, 0, 1, 6],
        "Hello,
        Orego!"
      EOS
    )
    test 'lex' do |(src, val, *a)|
      assert_equal(
        Orego::Token.new(Orego::Token::LITERAL_STRING, val, Orego::Location.new(*a)),
        Orego::Lexer.new(src).next_token,
      )
    end
  end

  sub_test_case 'newline inserting' do
    data(
      'right paren with eof' => [')', Orego::Token::RPAREN, ')', [0, 0, 0, 0], [0, 1, 0, 1]],
      'right bnracket with eof' => [']', Orego::Token::RBRACKET, ']', [0, 0, 0, 0], [0, 1, 0, 1]],
      'right brace with eof' => ['}', Orego::Token::RBRACE, '}', [0, 0, 0, 0], [0, 1, 0, 1]],
      'break with eof' => ["break", Orego::Token::KW_BREAK, "break", [0, 0, 0, 4], [0, 5, 0, 5]],
      'continue with eof' => ["continue", Orego::Token::KW_CONTINUE, "continue", [0, 0, 0, 7], [0, 8, 0, 8]],
      'return with eof' => ["return", Orego::Token::KW_RETURN, "return", [0, 0, 0, 5], [0, 6, 0, 6]],
      'true with eof' => ["true", Orego::Token::KW_TRUE, "true", [0, 0, 0, 3], [0, 4, 0, 4]],
      'false with eof' => ["false", Orego::Token::KW_FALSE, "false", [0, 0, 0, 4], [0, 5, 0, 5]],
      'nil with eof' => ["nil", Orego::Token::KW_NIL, "nil", [0, 0, 0, 2], [0, 3, 0, 3]],
      'identifier with comment' => ["hoge  # ident\n", Orego::Token::IDENTIFIER, :hoge, [0, 0, 0, 3], [0, 13, 0, 13]],
      'int literal with nl' => ["666\n", Orego::Token::LITERAL_INT, 666, [0, 0, 0, 2], [0, 3, 0, 3]],
      'float literal with nl' => ["1.23\n", Orego::Token::LITERAL_FLOAT, 1.23, [0, 0, 0, 3], [0, 4, 0, 4]],
      'string literal with nl' => [%Q`"a"\n`, Orego::Token::LITERAL_STRING, 'a', [0, 0, 0, 2], [0, 3, 0, 3]],
    )
    test 'lex' do |(src, tag, val, loc, nl_loc)|
      lexer = Orego::Lexer.new(src)
      assert_equal(
        Orego::Token.new(tag, val, Orego::Location.new(*loc)),
        lexer.next_token,
      )
      assert_equal(
        Orego::Token.new(Orego::Token::NEWLINE, "\n", Orego::Location.new(*nl_loc)),
        lexer.next_token,
      )
    end

    test 'lex right brace and newline inserting' do
      lexer = Orego::Lexer.new("{ 666 }")
      assert_equal(
        Orego::Token.new(Orego::Token::LBRACE, '{', Orego::Location.new(0, 0, 0, 0)),
        lexer.next_token,
      )
      assert_equal(
        Orego::Token.new(Orego::Token::LITERAL_INT, 666, Orego::Location.new(0, 2, 0, 4)),
        lexer.next_token,
      )
      assert_equal(
        Orego::Token.new(Orego::Token::NEWLINE, "\n", Orego::Location.new(0, 6, 0, 6)),
        lexer.next_token,
      )
      assert_equal(
        Orego::Token.new(Orego::Token::RBRACE, '}', Orego::Location.new(0, 6, 0, 6)),
        lexer.next_token,
      )
    end
  end
end
