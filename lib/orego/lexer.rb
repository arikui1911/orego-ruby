require 'orego/error'
require 'orego/token'
require 'orego/location'
require 'strscan'

module Orego
  class LexicalError < Error
    def initialize(loc, msg, ...)
      super("#{loc}: #{msg}", ...)
      @location = loc
    end

    attr_reader :location
  end

  # Lexical analizer for Orego syntax
  class Lexer
    def initialize(src)
      @src = src
      @fib = Fiber.new(&method(:lex))
    end

    # @return [Location]
    # @raise [LexicalError]
    def next_token
      @fib&.resume
    end

    private

    def emit_direct(token)
      @last = token
      Fiber.yield token
    end

    def emit(tag, val, ...)
      emit_direct Token.new(tag, val, Location.new(...))
    end

    def lex
      ss = StringScanner.new('')
      m = :lex_default
      lineno = 0
      @src.each_line do |line|
        ss.string = line.chomp + "\n"
        m = send(m, ss, lineno) until ss.eos?
        if m == :lex_default && newline_inserting_required?
          emit Token::NEWLINE, "\n", lineno, ss.pos-1, lineno, ss.pos-1
        end
        lineno += 1
      end
      emit Token::EOF, nil, lineno, 0, lineno, 0
      @fib = nil
    end

    STMT_ENDS = [
      Token::RPAREN,
      Token::RBRACKET,
      Token::RBRACE,
      Token::KW_BREAK,
      Token::KW_CONTINUE,
      Token::KW_RETURN,
      Token::KW_TRUE,
      Token::KW_FALSE,
      Token::KW_NIL,
      Token::IDENTIFIER,
      Token::LITERAL_INT,
      Token::LITERAL_FLOAT,
      Token::LITERAL_STRING,
    ]

    def newline_inserting_required?
      STMT_ENDS.include? @last&.tag
    end

    STATICS = {
      '(' => Token::LPAREN,
      ')' => Token::RPAREN,
      '[' => Token::LBRACKET,
      ']' => Token::RBRACKET,
      '{' => Token::LBRACE,
      '->' => Token::ARROW,
      ',' => Token::COMMA,
      ';' => Token::SEMICOLON,
      ':' => Token::COLON,
      '=' => Token::LET,
      '==' => Token::EQ,
      '!=' => Token::NE,
      '>=' => Token::GE,
      '<=' => Token::LE,
      '>' => Token::GT,
      '<' => Token::LT,
      '+' => Token::ADD,
      '-' => Token::SUB,
      '*' => Token::MUL,
      '/' => Token::DIV,
      '%' => Token::MOD,
      '+=' => Token::ADD_LET,
      '-=' => Token::SUB_LET,
      '*=' => Token::MUL_LET,
      '/=' => Token::DIV_LET,
      '%=' => Token::MOD_LET,
      '!' => Token::BANG,
      'def' => Token::KW_DEF,
      'if' => Token::KW_IF,
      'else' => Token::KW_ELSE,
      'elsif' => Token::KW_ELSIF,
      'while' => Token::KW_WHILE,
      'break' => Token::KW_BREAK,
      'continue' => Token::KW_CONTINUE,
      'return' => Token::KW_RETURN,
      'true' => Token::KW_TRUE,
      'false' => Token::KW_FALSE,
      'nil' => Token::KW_NIL,
    }

    STATIC_RE = Regexp.union(*STATICS.keys.sort_by{|k| -k.length })

    def lex_default(ss, lineno)
      col = ss.pos
      case
      when ss.scan(/\s+/)
        # do nothing
      when ss.scan(/#/)
        ss.terminate
      when ss.scan(/\}/)
        emit Token::NEWLINE, "\n", lineno, col, lineno, col if newline_inserting_required?
        emit Token::RBRACE, ss.matched, lineno, col, lineno, ss.pos-1
      when ss.scan(/"/)
        @string = Token.new(Token::LITERAL_STRING, ::String.new, Location.new(lineno, col))
        return :lex_string
      when ss.scan(/(0|[1-9][0-9]*)\.[0-9]+/)
        emit Token::LITERAL_FLOAT, Kernel.Float(ss.matched), lineno, col, lineno, ss.pos-1
      when ss.scan(/(0|[1-9][0-9]*)/)
        emit Token::LITERAL_INT, Kernel.Integer(ss.matched), lineno, col, lineno, ss.pos-1
      when ss.scan(STATIC_RE)
        emit STATICS.fetch(ss.matched), ss.matched, lineno, col, lineno, ss.pos-1
      when ss.scan(/[_a-zA-Z][_a-zA-Z0-9]*/)
        emit Token::IDENTIFIER, ss.matched.intern, lineno, col, lineno, ss.pos-1
      when ss.scan(/./)
        raise LexicalError.new(Location.new(lineno, col, lineno, col), "invalid character - #{ss.matched.inspect}")
      else
        raise Exception, 'must not happen'
      end
      __method__
    end

    ESC = {
      't' => "\t",
      'v' => "\v",
      'n' => "\n",
      'r' => "\r",
      'f' => "\f",
      'b' => "\b",
      'a' => "\a",
      'e' => "\e",
      's' => "\s",
    }

    def lex_string(ss, lineno)
      case
      when ss.scan(/"/)
        @string.value.freeze
        @string.location.end_line = lineno
        @string.location.end_column = ss.pos-1
        emit_direct @string
        return :lex_default
      when ss.scan(/\\(.)/)
        raise if ss[1] == "\n"
        @string.value << (ESC[ss[1]] || ss[1]) unless ss[1] == "\n"
      when ss.scan(/[^\"]+/)
        @string.value << ss.matched
      else
        raise Exception, 'must not happen'
      end
      __method__
    end
  end
end
