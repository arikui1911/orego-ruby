require 'test/unit'
require 'orego/lexer'
require 'orego/token'

class TestLexer < Test::Unit::TestCase
  include Orego

  data 'left paren',    ['(', Token::LPAREN]
  data 'right paren',   [')', Token::RPAREN]
  data 'left bracket',  ['[', Token::LBRACKET]
  data 'right bracket', [']', Token::RBRACKET]
  data 'left brace',    ['{', Token::LBRACE]
  data 'arrow',         ['->', Token::ARROW]
  data 'comma',         [',', Token::COMMA]
  data 'semicolon',     [';', Token::SEMICOLON]
  data 'colon',         [':', Token::COLON]
  data 'let',           ['=', Token::LET]
  data 'eq',            ['==', Token::EQ]
  data 'ne',            ['!=', Token::NE]
  data 'ge',            ['>=', Token::GE]
  data 'le',            ['<=', Token::LE]
  data 'gt',            ['>', Token::GT]
  data 'lt',            ['<', Token::LT]
  data 'add',           ['+', Token::ADD]
  data 'sub',           ['-', Token::SUB]
  data 'mul',           ['*', Token::MUL]
  data 'div',           ['/', Token::DIV]
  data 'mod',           ['%', Token::MOD]
  data 'add let',       ['+=', Token::ADD_LET]
  data 'sub let',       ['-=', Token::SUB_LET]
  data 'mul let',       ['*=', Token::MUL_LET]
  data 'div let',       ['/=', Token::DIV_LET]
  data 'mod let',       ['%=', Token::MOD_LET]
  data 'bang',          ['!', Token::BANG]
  data 'def',           ['def', Token::KW_DEF]
  data 'if',            ['if', Token::KW_IF]
  data 'else',          ['else', Token::KW_ELSE]
  data 'elsif',         ['elsif', Token::KW_ELSIF]
  data 'while',         ['while', Token::KW_WHILE]
  data 'break',         ['break', Token::KW_BREAK]
  data 'continue',      ['continue', Token::KW_CONTINUE]
  data 'return',        ['return', Token::KW_RETURN]
  data 'true',          ['true', Token::KW_TRUE]
  data 'false',         ['false', Token::KW_FALSE]
  data 'nil',           ['nil', Token::KW_NIL]

  data 'identifier; basic one',     ['hoge', Token::IDENTIFIER, :hoge]
  data 'identifier; underbar',      ['_', Token::IDENTIFIER, :_]
  data 'identifier; one char',      ['x', Token::IDENTIFIER, :x]
  data 'identifier; with number',   ['x123', Token::IDENTIFIER, :x123]
  data 'identifier; conc underbar', ['hoge_piyo', Token::IDENTIFIER, :hoge_piyo]
  data 'identifier; hyphen NG',     ['hoge-piyo', Token::IDENTIFIER, :hoge, 3]

  data 'int literal; basic',            ['666', Token::LITERAL_INT, 666]
  data 'int literal; zero',             ['0', Token::LITERAL_INT, 0]
  data 'int literal; consecutive zero', ['0000', Token::LITERAL_INT, 0, 0]

  data 'float literal; basic', ['1.23', Token::LITERAL_FLOAT, 1.23]
  data 'float literal; zero', ['0.0', Token::LITERAL_FLOAT, 0.0]
  data 'float literal; consecutive zero in float part', ['1.001', Token::LITERAL_FLOAT, 1.001]

  data 'string literal; basic', ['"Hello"', Token::LITERAL_STRING, 'Hello', 0, 0, 0, 6]
  data 'string literal; empty', ['""', Token::LITERAL_STRING, '', 0, 0, 0, 1]
  data 'string literal; escape sequences', [%Q`"\\t\\v\\n\\r\\f\\b\\a\\e\\s"`, Token::LITERAL_STRING, "\t\v\n\r\f\b\a\e\s", 0, 0, 0, 19]
  data 'string literal; multi line', [<<~EOS, Token::LITERAL_STRING, "Hello,\nOrego!", 0, 0, 1, 6]
    "Hello,
    Orego!"
  EOS

  test 'lex' do |(src, tag, val, *pos)|
    val ||= src
    pos = case pos.size
    when 0
      [0, 0, 0, src.length-1]
    when 1
      [0, 0, 0, pos.first]
    when 4
      pos
    else
      raise ArgumentError
    end
    assert_equal(
      Orego::Token.new(tag, val, Orego::Location.new(*pos)),
      Orego::Lexer.new(src).next_token,
    )
  end

  test 'invalid character' do
    assert_raise LexicalError do
      Orego::Lexer.new('@').next_token
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
