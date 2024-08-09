require 'test/unit'
require 'orego/parser'
require 'orego/lexer'

class TestParser < Test::Unit::TestCase
  include Orego
  include Orego::AST

  def self.sub_test_case(...)
  end

  def do_parse(src)
    Orego::Parser.new(Orego::Lexer.new(src), '*test*').parse
  end

  def self.make_1stmt_tree(stmt)
    Orego::AST::Program.new(
      stmt.location,
      '*test*',
      Orego::AST::Statements.new(stmt.location, [stmt]),
    )
  end

  def self.make_1expr_tree(expr)
    make_1stmt_tree Orego::AST::ExpressionStatement.new(expr.location, expr)
  end

  def self.data_primary(src, klass, ...)
    [src, make_1expr_tree(klass.new(Location.new(0, 0, 0, src.length-1), ...))]
  end

  def self.data_prefixed(src, op, klass, val)
    [src, make_1expr_tree(PrefixedExpression.new(
      Location.new(0, 0, 0, src.length-1), op,
      klass.new(Location.new(0, 1, 0, src.length-1), val),
    ))]
  end

  data 'identifier',     data_primary('hoge', Identifier, :hoge)
  data 'true literal',   data_primary('true', BoolLiteral, true)
  data 'false literal',  data_primary('false', BoolLiteral, false)
  data 'int literal',    data_primary('666', IntLiteral, 666)
  data 'float literal',  data_primary('1.23', FloatLiteral, 1.23)
  data 'string literal', data_primary('"Hello"', StringLiteral, 'Hello')
  data 'nil literal',    data_primary('nil', NilLiteral)
  data 'grouped',        ['(666)', make_1expr_tree(IntLiteral.new(Location.new(0, 1, 0, 3), 666))]
  data 'plus',           data_prefixed('+666', Operator::PLUS, IntLiteral, 666)
  data 'minus',          data_prefixed('-999', Operator::MINUS, IntLiteral, 999)
  data 'not',            data_prefixed('!true', Operator::NOT, BoolLiteral, true)

  def self.data_array(src, *elems)
    [src, make_1expr_tree(ArrayLiteral.new(Location.new(0, 0, 0, src.length-1), elems.map.with_index{|(klass, val), i|
      klass.new(Location.new(0, i*3+1, 0, i*3+1), val)
    }))]
  end

  data 'array; empty',                data_array('[]')
  data 'array; one',                  data_array('[1]', [IntLiteral, 1])
  data 'array; several',              data_array('[a, b, c]', [Identifier, :a], [Identifier, :b], [Identifier, :c])
  data 'array; one, extra comma',     data_array('[1,]', [IntLiteral, 1])
  data 'array; several, extra comma', data_array('[a, b, c,]', [Identifier, :a], [Identifier, :b], [Identifier, :c])

  def self.data_array_ml(src, *elems)
    [src, make_1expr_tree(ArrayLiteral.new(Location.new(0, 0, src.lines.size-1, 0), elems.map.with_index{|(klass, val), i|
      klass.new(Location.new(i+1, 2, i+1, 2), val)
    }))]
  end

  data 'array; multiline, empty', data_array_ml(<<~EOS)
    [
    ]
  EOS
  data 'array; multiline, one', data_array_ml(<<~EOS, [Identifier, :a])
    [
      a
    ]
  EOS
  data 'array; multiline, several', data_array_ml(<<~EOS, [Identifier, :a], [Identifier, :b], [Identifier, :c])
    [
      a,
      b,
      c
    ]
  EOS
  data 'array; multiline, one, extra comma', data_array_ml(<<~EOS, [Identifier, :a])
    [
      a,
    ]
  EOS
  data 'array; multiline, several, extra comma', data_array_ml(<<~EOS, [Identifier, :a], [Identifier, :b], [Identifier, :c])
    [
      a,
      b,
      c,
    ]
  EOS

  def self.data_hash(src, *pairs)
    [src, make_1expr_tree(HashLiteral.new(Location.new(0, 0, 0, src.length-1), pairs.map.with_index{|(key_c, key_v, val_c, val_v), i|
      kc = i*6+1
      vc = i*6+4
      [key_c.new(Location.new(0, kc, 0, kc), key_v), val_c.new(Location.new(0, vc, 0, vc), val_v)]
    }))]
  end

  data 'hash; empty',   data_hash('{}')
  data 'hash; one',     data_hash('{x: 1}', [Identifier, :x, IntLiteral, 1])
  data 'hash; several', data_hash('{a: 1, b: 2, c: 3}', [Identifier, :a, IntLiteral, 1], [Identifier, :b, IntLiteral, 2], [Identifier, :c, IntLiteral, 3])
  data 'hash; one, extra comma',     data_hash('{x: 1,}', [Identifier, :x, IntLiteral, 1])
  data 'hash; several, extra comma', data_hash('{a: 1, b: 2, c: 3,}', [Identifier, :a, IntLiteral, 1], [Identifier, :b, IntLiteral, 2], [Identifier, :c, IntLiteral, 3])

  def self.data_hash_ml(src, *pairs)
    [src, make_1expr_tree(HashLiteral.new(Location.new(0, 0, pairs.size+1, 0), pairs.map.with_index{|(key_c, key_v, val_c, val_v), i|
      [key_c.new(Location.new(i+1, 2, i+1, 2), key_v), val_c.new(Location.new(i+1, 5, i+1, 5), val_v)]
    }))]
  end

  data 'hash; empty, multiline', data_hash_ml(<<~EOS)
    {
    }
  EOS
  data 'hash; one, multiline', data_hash_ml(<<~EOS, [Identifier, :x, IntLiteral, 1])
    {
      x: 1
    }
  EOS
  data 'hash; several, multiline', data_hash_ml(<<~EOS, [Identifier, :a, IntLiteral, 1], [Identifier, :b, IntLiteral, 2], [Identifier, :c, IntLiteral, 3])
    {
      a: 1,
      b: 2,
      c: 3
    }
  EOS
  data 'hash; one, multiline, extra comma', data_hash_ml(<<~EOS, [Identifier, :x, IntLiteral, 1])
    {
      x: 1,
    }
  EOS
  data 'hash; several, multiline, extra comma', data_hash_ml(<<~EOS, [Identifier, :a, IntLiteral, 1], [Identifier, :b, IntLiteral, 2], [Identifier, :c, IntLiteral, 3])
    {
      a: 1,
      b: 2,
      c: 3,
    }
  EOS

  def self.data_function(src, *params)
    [src, make_1expr_tree(FunctionLiteral.new(
      Location.new(0, 0, 0, src.length-1),
      params.map.with_index{|(klass, val), i|
        n = (i + 1) * 3
        klass.new(Location.new(0, n, 0, n), val)
      },
      Block.new(Location.new(0, src.length-2, 0, src.length-1), Statements.new(Location.new(0, 0, 0, 0), [])),
    ))]
  end

  data 'function; omitted args', data_function('->{}')
  data 'function; no args',      data_function('->(){}')
  data 'function; one args',     data_function('->(x){}', [Identifier, :x])
  data 'function; several args', data_function('->(a, b, c){}', [Identifier, :a], [Identifier, :b], [Identifier, :c])
  data 'function; one args, extra comma',     data_function('->(x,){}', [Identifier, :x])
  data 'function; several args, extra comma', data_function('->(a, b, c,){}', [Identifier, :a], [Identifier, :b], [Identifier, :c])

  def self.data_function_ml(src, *params)
    n = src.lines.size
    [src, make_1expr_tree(FunctionLiteral.new(
      Location.new(0, 0, n-1, 0),
      params.map.with_index{|(klass, val), i| klass.new(Location.new(i+2, 2, i+2, 2), val) },
      Block.new(Location.new(n-2, 0, n-1, 0), Statements.new(Location.new(0, 0, 0, 0), [])),
    ))]
  end

  data 'function; omitted args, multiline', data_function_ml(<<~EOS)
    ->
    {
    }
  EOS
  data 'function; no args, multiline', data_function_ml(<<~EOS)
    ->
    (
    )
    {
    }
  EOS
  data 'function; one arg, multiline', data_function_ml(<<~EOS, [Identifier, :x])
    ->
    (
      x
    )
    {
    }
  EOS
  data 'function; several args, multiline', data_function_ml(<<~EOS, [Identifier, :a], [Identifier, :b], [Identifier, :c])
    ->
    (
      a,
      b,
      c
    )
    {
    }
  EOS
  data 'function; one arg, multiline, extra comma', data_function_ml(<<~EOS, [Identifier, :x])
    ->
    (
      x,
    )
    {
    }
  EOS
  data 'function; several args, multiline, extra comma', data_function_ml(<<~EOS, [Identifier, :a], [Identifier, :b], [Identifier, :c])
    ->
    (
      a,
      b,
      c,
    )
    {
    }
  EOS

  test 'prefixed expression' do |(src, expected)|
    assert_equal expected, do_parse(src)
  end

  data 'array; empty, extra comma', '[,]'
  data 'array; empty, multiline, extra comma', <<~EOS
    [
      ,
    ]
  EOS
  data 'hash; empty, extra comma', '{,}'
  data 'hash; empty, multiline, extra comma', <<~EOS
    {
      ,
    }
  EOS
  data 'function; empty, extra comma', '->(,){}'
  data 'function; empty, multiline, extra comma', <<~EOS
    ->(
      ,
    )
  EOS
  test 'prefixed expression; syntax error' do |src|
    assert_raise(Orego::ParseError){ do_parse src }
  end

  def self.data_infixed(src, op)
    n = src.length-1
    [src, make_1expr_tree(InfixedExpression.new(
        Location.new(0, 0, 0, n), op,
        IntLiteral.new(Location.new(0, 0, 0, 0), 1),
        IntLiteral.new(Location.new(0, n, 0, n), 2),
    ))]
  end

  data 'equal',                 data_infixed('1 == 2', Operator::EQ)
  data 'not equal',             data_infixed('1 != 2', Operator::NE)
  data 'greater than or equal', data_infixed('1 >= 2', Operator::GE)
  data 'less than or equal',    data_infixed('1 <= 2', Operator::LE)
  data 'greater than',          data_infixed('1 > 2', Operator::GT)
  data 'less than',             data_infixed('1 < 2', Operator::LT)
  data 'addition',              data_infixed('1 + 2', Operator::ADD)
  data 'subtraction',           data_infixed('1 - 2', Operator::SUB)
  data 'multiplication',        data_infixed('1 * 2', Operator::MUL)
  data 'division',              data_infixed('1 / 2', Operator::DIV)
  data 'modulo',                data_infixed('1 % 2', Operator::MOD)

  def self.data_infixed_conc(src, op1, op2, either)
    one, two, three = Array.new(3){|i| IntLiteral.new(Location.new(0, 4*i, 0, 4*i), i+1) }
    ex = case either
    in :left
      l = InfixedExpression.new(one.location.between(two.location), op1, one, two)
      InfixedExpression.new(one.location.between(three.location), op2, l, three)
    in :right
      r = InfixedExpression.new(two.location.between(three.location), op2, two, three)
      InfixedExpression.new(one.location.between(three.location), op1, one, r)
    end
    [src, make_1expr_tree(ex)]
  end

  data 'same op lconc', data_infixed_conc('1 + 2 + 3', Operator::ADD, Operator::ADD, :left)
  data 'mul has priority over add', data_infixed_conc('1 + 2 * 3', Operator::ADD, Operator::MUL, :right)

  def self.data_call(src, *args)
    [src, make_1expr_tree(Call.new(
      Location.new(0, 0, 0, src.length-1),
      Identifier.new(Location.new(0, 0, 0, 0), :f),
      args.map.with_index{|(klass, val), i| klass.new(Location.new(0, i*3+2, 0, i*3+2), val) },
    ))]
  end

  data 'call; empty',                data_call('f()')
  data 'call; one',                  data_call('f(1)', [IntLiteral, 1])
  data 'call; several',              data_call('f(1, 2, 3)', [IntLiteral, 1], [IntLiteral, 2], [IntLiteral, 3])
  data 'call; one, extra comma',     data_call('f(1,)', [IntLiteral, 1])
  data 'call; several, extra comma', data_call('f(1, 2, 3,)', [IntLiteral, 1], [IntLiteral, 2], [IntLiteral, 3])

  def self.data_call_ml(src, *args)
    [src, make_1expr_tree(Call.new(
      Location.new(0, 0, src.lines.size-1, 0),
      Identifier.new(Location.new(0, 0, 0, 0), :f),
      args.map.with_index{|(klass, val), i| klass.new(Location.new(i+1, 2, i+1, 2), val) },
    ))]
  end

  data 'call; empty, multiline', data_call_ml(<<~EOS)
    f(
    )
  EOS
  data 'call; one, multiline', data_call_ml(<<~EOS, [IntLiteral, 1])
    f(
      1
    )
  EOS
  data 'call; several, multiline', data_call_ml(<<~EOS, [IntLiteral, 1], [IntLiteral, 2], [IntLiteral, 3])
    f(
      1,
      2,
      3
    )
  EOS
  data 'call; one, multiline, extra comma', data_call_ml(<<~EOS, [IntLiteral, 1])
    f(
      1,
    )
  EOS
  data 'call; several, multiline, extra comma', data_call_ml(<<~EOS, [IntLiteral, 1], [IntLiteral, 2], [IntLiteral, 3])
    f(
      1,
      2,
      3,
    )
  EOS

  def self.data_index_access(src, container, key)
    [src, make_1expr_tree(IndexAccess.new(
      Location.new(0, 0, 0, src.length-1),
      container.first.new(Location.new(0, 0, 0, 0), container.last),
      key.first.new(Location.new(0, 2, 0, 2), key.last),
    ))]
  end

  data 'index access; one key', data_index_access('a[1]', [Identifier, :a], [IntLiteral, 1])

  def self.data_index_access_ml(src, container, key)
    [src, make_1expr_tree(IndexAccess.new(
      Location.new(0, 0, 2, 0),
      container.first.new(Location.new(0, 0, 0, 0), container.last),
      key.first.new(Location.new(1, 2, 1, 2), key.last),
    ))]
  end

  data 'index access; one key, multiline', data_index_access_ml(<<~EOS, [Identifier, :a], [IntLiteral, 1])
    a[
      1
    ]
  EOS
  data 'index access; one key, multiline, extra comma', data_index_access_ml(<<~EOS, [Identifier, :a], [IntLiteral, 1])
    a[
      1,
    ]
  EOS

  def self.data_let()
  end
  
  test 'infixed expression' do |(src, expected)|
    assert_equal expected, do_parse(src)
  end

  data 'call; empty, extra comma', 'f(,)'
  data 'call; empty, multiline, extra comma', <<~EOS
    f(
      ,
    )
  EOS
  data 'index access; empty', 'a[]'
  data 'index access; several', 'a[1, 2]'
  test 'infixed expression; syntax error' do |src|
    assert_raise(Orego::ParseError){ do_parse src }
  end
end
