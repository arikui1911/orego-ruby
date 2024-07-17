require 'test/unit'
require 'orego/parser'
require 'orego/lexer'

class TestParser < Test::Unit::TestCase
  sub_test_case 'prefixed expression' do
    data(
      'identifier' => ['hoge', Orego::AST::Identifier, :hoge],
      'true literal' => ['true', Orego::AST::BoolLiteral, true],
      'false literal' => ['false', Orego::AST::BoolLiteral, false],
      'int literal' => ['666', Orego::AST::IntLiteral, 666],
      'float literal' => ['1.23', Orego::AST::FloatLiteral, 1.23],
      'string literal' => ['"Hello"', Orego::AST::StringLiteral, 'Hello'],
    )
    test 'parse primary' do |(src, klass, val)|
      assert_equal(
        make_1expr_tree(klass.new(Orego::Location.new(0, 0, 0, src.length-1), val)),
        do_parse(src),
      )
    end

    test 'parse nil literal' do
      src = 'nil'
      klass = Orego::AST::NilLiteral
      assert_equal(
        make_1expr_tree(klass.new(Orego::Location.new(0, 0, 0, src.length-1))),
        do_parse(src),
      )
    end
  end

  test 'parse grouped' do
    src = '(666)'
    klass = Orego::AST::IntLiteral
    val = 666
    assert_equal(
      make_1expr_tree(klass.new(Orego::Location.new(0, 1, 0, 3), val)),
      do_parse(src),
    )
  end

  def do_parse(src)
    Orego::Parser.new(Orego::Lexer.new(src), '*test*').parse
  end

  def make_1stmt_tree(stmt)
    Orego::AST::Program.new(
      stmt.location,
      '*test*',
      Orego::AST::Statements.new(stmt.location, [stmt]),
    )
  end

  def make_1expr_tree(expr)
    make_1stmt_tree Orego::AST::ExpressionStatement.new(expr.location, expr)
  end
end
