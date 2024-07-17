require 'orego/error'
require 'orego/token'

module Orego
  module AST
    _ = ->(*a){ Struct.new(:location, *a) }

    Program = _[:filename, :statements]
    Block = _[:statements]
    Statements = _[:list]
    ExpressionStatement = _[:expression]
    Identifier = _[:name]
    BoolLiteral = _[:value]
    NilLiteral = _[]
    IntLiteral = _[:value]
    FloatLiteral = _[:value]
    StringLiteral = _[:value]
    PrefixedExpression = _[:operator, :operand]
    ArrayLiteral = _[:elements]
    HashLiteral = _[:pairs]
    FunctionLiteral = _[:parameters, :body]

    module Operator
      %i[
        PLUS
        MINUS
        NOT
    ].each{|s| const_set s, s }
    end
  end

  class ParseError < Error
    def initialize(filename, loc, msg, ...)
      super("#{loc}: #{msg}", ...)
      @filename = filename
      @location = loc
    end

    attr_reader :filename, :location
  end

  class Parser
    def initialize(lexer, filename)
      @lexer = lexer
      @filename = filename
      @buf = []
    end

    def parse
      _, stmts = parse_statements(Token::EOF)
      AST::Program.new(stmts.location, @filename, stmts)
    end

    private

    def next_token
      @buf.empty? ? @lexer.next_token : @buf.pop
    end

    def pushback(t)
      @buf.push t if t
      nil
    end

    def try(*tags)
      t = next_token()
      return t if tags.include?(t.tag)
      pushback t
      nil
    end

    def expect(*tags)
      t = next_token()
      tags.include?(t.tag) or parse_error(t, "expect #{tags}, but #{t.value.inspect}:#{t.tag}")
      t
    end

    def parse_error(token, msg)
      raise ParseError.new(@filename, token.location, msg)
    end

    def parse_block
      lb = expect(Token::LBRACE)
      rb, stmts = parse_statements(Token::RBRACE)
      AST::Block.new(lb.location.between(rb.location), stmts)
    end

    def parse_statements(term)
      t = nil
      buf = []
      loop do
        t = next_token()
        break if t.tag == term
        pushback t
        s = parse_statement()
        buf << s if s
      end
      return t, AST::Statements.new(buf.empty? ? Location.new(0, 0, 0, 0) : buf.first.location.between(buf.last.location), buf)
    end

    @@statement_table = {}

    def parse_statement
      t = next_token()
      m = @@statement_table[t.tag] and return send(m, t)
      pushback t
      parse_expression_statement
    end

    private_class_method def self.define_statement(name, first_token_tag, &b)
      m = "parse_#{name}".intern
      @@statement_table[first_token_tag] = m
      define_method(m, &b)
    end

    define_statement :empty_statement, Token::SEMICOLON do |t|
      # do nothing
    end

    define_statement :def, Token::KW_DEF do |kw|
      raise NotImplementedError
    end
    define_statement :if, Token::KW_IF do |kw|
      raise NotImplementedError
    end
    define_statement :while, Token::KW_WHILE do |kw|
      raise NotImplementedError
    end
    define_statement :break, Token::KW_BREAK do |kw|
      raise NotImplementedError
    end
    define_statement :continue, Token::KW_CONTINUE do |kw|
      raise NotImplementedError
    end
    define_statement :return, Token::KW_RETURN do |kw|
      raise NotImplementedError
    end

    def parse_expression_statement
      x = parse_expression()
      t = expect(Token::NEWLINE, Token::SEMICOLON)
      AST::ExpressionStatement.new(x.location, x)
    end

    module Precedence
      %i[
        LOWEST
        EQUALITY
        COMPARE
        ADDITIVE
        MULTIVE
        PREFIX
        CALL
        HIGHEST      
      ].each_with_index{|c, i| const_set c, i }
    end

    @@prefixed_table = {}

    private_class_method def self.define_prefixed(name, *prefix_token_tags, &b)
      m = "parse_#{name}".intern
      @@prefixed_table.update prefix_token_tags.to_h{|k| [k, m] }
      define_method(m, &b)
    end

    @@infixed_table = {}
    @@precedences = {}

    private_class_method def self.define_infixed(name, infix_token_and_prec_table, &b)
      tab = infix_token_and_prec_table.flat_map{|k, v| [*k].map{|t| [t, v] } }.to_h
      @@precedences.update tab
      m = "parse_#{name}".intern
      @@infixed_table.update tab.to_h{|k, v| [k, m] }
      define_method(m, &b)
    end

    def parse_expression(prec = Precedence::LOWEST)
      t = next_token()
      m = @@prefixed_table[t.tag] or parse_error(t, "unexpected token on first of expression - #{t.value.inspect}:#{t.tag}")
      left = send(m, t)
      loop do
        t = next_token()
        np = @@precedences[t.tag]
        if !np || (prec != Precedence::HIGHEST && prec >= np)
          pushback t
          break
        end
        left = send(@@infixed_table[t.tag], left, t)
      end
      left
    end

    define_prefixed :identifier, Token::IDENTIFIER do |t|
      AST::Identifier.new(t.location, t.value)
    end

    define_prefixed :bool_literal, Token::KW_TRUE, Token::KW_FALSE do |t|
      AST::BoolLiteral.new(t.location, t.tag == Token::KW_TRUE)
    end

    define_prefixed :nil_literal, Token::KW_NIL do |t|
      AST::NilLiteral.new(t.location)
    end

    define_prefixed :int_literal, Token::LITERAL_INT do |t|
      AST::IntLiteral.new(t.location, t.value)
    end

    define_prefixed :float_literal, Token::LITERAL_FLOAT do |t|
      AST::FloatLiteral.new(t.location, t.value)
    end

    define_prefixed :string_literal, Token::LITERAL_STRING do |t|
      AST::StringLiteral.new(t.location, t.value)
    end

    define_prefixed :grouped, Token::LPAREN do |t|
      x = parse_expression()
      expect Token::RPAREN
      x
    end

    PREFIX_OPS = {
      Token::ADD => AST::Operator::PLUS,
      Token::SUB => AST::Operator::MINUS,
      Token::BANG => AST::Operator::NOT,
    }

    define_prefixed :prefixed, *PREFIX_OPS.keys do |t|
      x = parse_expression(Precedence::PREFIX)
      AST::PrefixedExpression.new(t.location.between(x), PREFIX_OPS[t.tag], x)
    end

    define_prefixed :array_literal, Token::LBRACKET do |lb|
      rb, elems = parse_comma_list(Token::RBRACKET){ parse_expression() }
      AST::ArrayLiteral.new(lb.location.between(rb.location), elems)
    end

    define_prefixed :hash_literal, Token::LBRACE do |lb|
      rb, pairs = parse_comma_list(Token::RBRACE){
        k = parse_expression()
        expect Token::COLON
        v = parse_expression()
        [k, v]
      }
      AST::HashLiteral.new(lb.location.between(rb.location), pairs)
    end

    define_prefixed :function_literal, Token::ARROW do |arrow|
      t = expect(Token::LPAREN, Token::LBRACE)
      params = (t.tag == Token::LBRACE ? [] : parse_comma_list(Token::RPAREN){
        t = expect(Token::IDENTIFIER)
        AST::Identifier.new(t.location, t.value)
      })
      b = parse_block()
      AST::FunctionLiteral.new(arrow.location.between(b.location), params, b)
    end

    define_infixed(
      :infixed,
      [Token::EQ, Token::NE] => Precedence::EQUALITY,
      [Token::GE, Token::LE, Token::GT, Token::LT] => Precedence::COMPARE,
      [Token::ADD, Token::SUB] => Precedence::ADDITIVE,
      [Token::MUL, Token::DIV, Token::MOD] => Precedence::MULTIVE,
    ) do |left, t|
      raise NotImplementedError
    end

    define_infixed :call, Token::LPAREN => Precedence::CALL do |fn, t|
      raise NotImplementedError
    end

    define_infixed :index_access, Token::LBRACKET => Precedence::CALL do |container, t|
      raise NotImplementedError
    end

    define_infixed :let, Token::LET => Precedence::HIGHEST do |left, t|
      raise NotImplementedError
    end

    def parse_comma_list(closer)
      t = try(closer) and return t, []  # 空リストかどうかの試行
      list = [yield()]                  # 最初の要素の処理
      token = loop {
        # 最後の要素の直後になるかどうか試行
        t = next_token()
        case t.tag
        when Token::COMMA
          # do nothing
        when closer         # 改行せず最後の要素と同じ行に閉じカッコ
          break t
        when Token::NEWLINE # 字句解析で付けられる改行トークンの処理
          break expect(closer)
        else
          parse_error t, "unexpected token as delimiter - #{t.value}:#{t.tag}; comma expected"
        end

        t = try(closer) and break t   # 最後の要素の後ろにもカンマを許す
        list << yield()               # 次の要素
      }
      return token, list
    end
  end
end
