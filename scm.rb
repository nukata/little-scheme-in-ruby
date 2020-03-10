#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
# A little Scheme in Ruby 2.3, v0.1.0 R02.03.10/R02.03.11 by SUZUKI Hisao
# cf. https://github.com/nukata/little-scheme-in-cs
#     https://github.com/nukata/l2lisp-in-ruby

module LittleScheme
  LS = LittleScheme

  # A unique value which means the expression has no value
  NONE = :'#<VOID>'

  # A unique value which means the End Of File
  EOF = :'#<EOF>'

  # Cons cell
  class Cell
    include Enumerable
    attr_reader :car
    attr_accessor :cdr

    def initialize(car, cdr)
      @car = car
      @cdr = cdr
    end

    # Yield car, cadr, caddr and so on, à la for-each in Scheme.
    def each
      j = self
      begin
        yield j.car
        j = j.cdr
      end while Cell === j
      j.nil? or raise ImproperListException, j
    end
  end # Cell

  # The last tail of the list is not null.
  class ImproperListException < RuntimeError
    # The last tail of the list
    attr_reader :tail

    # Construct with the last tail which is not null.
    def initialize(tail)
      @tail = tail
    end
  end # ImproperListException

  # NB: Scheme's symbols are represented by Ruby's symbols.

  # ----------------------------------------------------------------------

  # Linked list of bindings which are mapping symbols to values
  class Environment
    include Enumerable

    # The bound symbol
    attr_reader :symbol

    # The value mapped from the bound symbol
    attr_accessor :value

    # The successor of the present binding, or nil
    attr_accessor :succ

    # Construct a binding on the top of succ.
    def initialize(symbol, value, succ)
      @symbol = symbol
      @value = value
      @succ = succ
    end

    # Yield each binding.
    def each
      env = self
      begin
        yield env
        env = env.succ
      end until env.nil?
    end

    # Search the binding for a symbol.
    def look_for(symbol)
      env = self.find {|e| e.symbol.equal? symbol}
      return env unless env.nil?
      raise NameError.new("#{symbol} not found", name: symbol)
    end

    # Build a new environment by prepending the bindings of symbols and data
    # to the present environment.
    def prepend_defs(symbols, data)
      if symbols.nil?
        return self if data.nil?
        raise ArgumentError,
              "surplus arg: #{LS.stringify data}"
      else
        raise ArgumentError,
              "surplus param: #{LS.stringify symbols}" if data.nil?
        return Environment.new(symbols.car, data.car,
                               prepend_defs(symbols.cdr, data.cdr))
      end
    end
  end # Environment

  # ----------------------------------------------------------------------

  # NB: Scheme's continuations have the following operations:
  #  :Then, :Begin, :Define, :SetQ, :Apply, :ApplyFun, :EvalArg, :ConsArgs,
  #  :RestoreEnv

  # Scheme's continuation as a stack of steps
  class Continuation
    attr_reader :stack
    
    # Construct a copy of another continuation, or an empty continuation.
    def initialize(other=nil)
      @stack = other.nil? ? [] : Array.new(other.stack)
    end

    # Copy steps from another continuation.
    def copy_from(other)
      @stack = Array.new(other.stack)
    end

    # Length of the continuation (it is expected to be an O(1) operation.)
    def length
      return @stack.length
    end

    # Push a step to the top of the continuation.
    def push(operation, value)
      @stack.push [operation, value]
    end

    # Override Object#inspect.  Return a quasi-stack trace.
    def inspect
      ss = @stack.map {|step| "#{step[0]} #{LS.stringify step[1]}"}
      return "#<#{ss.join "\n\t"}>"
    end

    # Pop a step, [operation, value], from the top of the continuation.
    def pop
      return @stack.pop
    end

    # Push :RestoreEnv unless on a tail call.
    def push_RestoreEnv(env)
      top = @stack.last
      push(:RestoreEnv, env) unless (top and top[0].equal? :RestoreEnv)
    end
  end # Continuation

  # ----------------------------------------------------------------------

  # Lambda expression with its environment
  class Closure
    # A list of symbols as formal parameters
    attr_reader :params

    # A list of expressions as a body
    attr_reader :body

    # An environment of the body
    attr_reader :env

    # Construct a new lambda expression.
    def initialize(parameters, body, env)
      @params = parameters
      @body = body
      @env = env
    end
  end # Closure

  # Built-in function
  class Intrinsic
    # Function's name
    attr_reader :name

    # Function's arity, -1 if variadic
    attr_reader :arity

    # Function's body
    attr_reader :fun

    # Construct a new built-in function.
    def initialize(name, arity, fun)
      @name = name
      @arity = arity
      @fun = fun
    end

    # Override Object#inspect.
    # Return a string which shows the function's name and arity.
    def inspect
      return "#<#{@name}:#{@arity}>"
    end
  end # Intrinsic

  # ----------------------------------------------------------------------

  # Exception thrown by the error procedure of SRFI-23
  class ErrorException < RuntimeError
    def initialize(reason, arg)
      super "Error: #{LS.stringify(reason, false)}: #{LS.stringify arg}"
    end
  end # ErrorException

  # ----------------------------------------------------------------------

  # Convert an expression to a string.
  def self.stringify(exp, quote=true)
    case exp
    when nil
      return "()"
    when false
      return "#f"
    when true
      return "#t"
    when Symbol
      return exp.to_s
    when String
      return quote ? exp.inspect : exp
    when Cell
      ss = []
      begin
        for e in exp
          ss << stringify(e, quote)
        end
      rescue ImproperListException => ex
        ss << "."
        ss << stringify(ex.tail, quote)
      end
      return "(#{ss.join " "})"
    when Environment
      ss = []
      for e in exp
        if e.equal? GLOBAL_ENV
          ss << "GlobalEnv"
          break
        elsif e.symbol.nil?     # frame marker
          ss << "|"
        else
          ss << e.symbol.to_s
        end
      end
      return "#<#{ss.join " "}>"
    when Closure
      return "#<" + stringify(exp.params) +
             ":" + stringify(exp.body) +
             ":" + stringify(exp.env) + ">"
    else
      return exp.inspect
    end
  end

  # ----------------------------------------------------------------------

  private_class_method def self.c(symbol, arity, fun, succ)
    return Environment.new(symbol,
                           Intrinsic.new(symbol.to_s, arity, fun),
                           succ)
  end

  private_class_method def self.globals
    j = nil
    env = GLOBAL_ENV.succ       # Skip the frame marker.
    for e in env
      j = Cell.new(e.symbol, j)
    end
    return j
  end

  G1 = c(:'+' , 2, lambda {|x| x.car + x.cdr.car},
         c(:'-', 2, lambda {|x| x.car - x.cdr.car},
           c(:'*', 2, lambda {|x| x.car * x.cdr.car},
             c(:'<', 2, lambda {|x| x.car < x.cdr.car},
               c(:'=', 2, lambda {|x| x.car == x.cdr.car},
                 c(:error, 2, lambda {|x|
                     raise ErrorException.new(x.car, x.cdr.car)
                   },
                   c(:globals, 0, lambda {|x| globals()},
                     Environment.new(:'call/cc', :'call/cc',
                                     Environment.new(:apply, :apply,
                                                     nil)))))))))
  GLOBAL_ENV = Environment.new(
    nil,                        # frame marker
    nil,
    c(:car, 1, lambda {|x| x.car.car},
      c(:cdr, 1, lambda {|x| x.car.cdr},
        c(:cons, 2, lambda {|x| Cell.new(x.car, x.cdr.car)},
          c(:eq?, 2, lambda {|x| x.car.equal? x.cdr.car},
            c(:eqv?, 2, lambda {|x| x.car == x.cdr.car},
              c(:pair?, 1, lambda {|x| Cell === x.car},
                c(:null?, 1, lambda {|x| x.car.nil?},
                  c(:not, 1, lambda {|x| x.car.equal? false},
                    c(:list, -1, lambda {|x| x},
                      c(:display, 1, lambda {|x|
                          print stringify(x.car, false)
                          return NONE
                        },
                        c(:newline, 0, lambda {|x|
                            puts
                            return NONE
                          },
                          c(:read, 0, lambda {|x| read_expression},
                            c(:'eof-object?', 1, lambda {|x| x.car.equal? EOF},
                              c(:symbol?, 1, lambda {|x| Symbol === x.car},
                                G1)))))))))))))))

  private_constant :G1

  # ----------------------------------------------------------------------

  # Evaluate an expression in an environment.
  def self.evaluate(exp, env)
    k = Continuation.new
    begin
      loop {
        loop {
          case exp
          when Cell
            kar = exp.car
            kdr = exp.cdr
            case kar
            when :quote         # (quote e)
              exp = kdr.car
              break
            when :if            # (if e1 e2 [e3])
              exp = kdr.car
              k.push(:Then, kdr.cdr)
            when :begin         # (begin e...)
              exp = kdr.car
              k.push(:Begin, kdr.cdr) unless kdr.cdr.nil?
            when :lambda        # (lambda (v...) e...)
              exp = Closure.new(kdr.car, kdr.cdr, env)
              break
            when :define        # (define v e)
              exp = kdr.cdr.car
              k.push(:Define, kdr.car)
            when :set!          # (set! v e)
              exp = kdr.cdr.car
              k.push(:SetQ, env.look_for(kdr.car))
            else                # (fun arg...)
              exp = kar
              k.push(:Apply, kdr)
            end
          when Symbol
            exp = env.look_for(exp).value
            break
          else                  # a number, #t, #f etc.
            break
          end
        }
        loop {
          # print "_#{k.length}"
          return exp if k.length == 0
          op, x = k.pop
          case op
          when :Then            # x is (e2 [e3]).
            if exp.equal? false
              if x.cdr.nil?
                exp = NONE
              else
                exp = x.cdr.car # e3
                break
              end
            else
              exp = x.car       # e2
              break
            end
          when :Begin           # x is (e...).
            k.push(:Begin, x.cdr) unless x.cdr.nil?
            exp = x.car
            break
          when :Define          # x is a variable name.
            # env.symbol should be nil i.e. a frame marker.
            env.succ = Environment.new(x, exp, env.succ)
            exp = NONE
          when :SetQ            # x is an Environment.
            x.value = exp
            exp = NONE
          when :Apply           # x is a list of args; exp is a function.
            if x.nil?
              exp, env = apply_function(exp, nil, k, env)
            else
              k.push(:ApplyFun, exp)
              until x.cdr.nil?
                k.push(:EvalArg, x.car)
                x = x.cdr
              end
              exp = x.car
              k.push(:ConsArgs, nil)
              break
            end
          when :ConsArgs
            # x is a list of evaluated args (to be a cdr);
            # exp is a newly evaluated arg (to be a car).
            args = Cell.new(exp, x)
            op, exp = k.pop
            case op
            when :EvalArg       # exp is the next arg.
              k.push(:ConsArgs, args)
              break
            when :ApplyFun      # exp is a function
              exp, env = apply_function(exp, args, k, env)
            else
              raise RuntimeError, "invalid operation: #{op}"
            end
          when :RestoreEnv      # x is an Environment.
            env = x
          else
            raise RuntimeError, "invalid operation: #{op}"
          end
        }
      }
    rescue ErrorException
      raise
    rescue => ex
      s =  "#{ex.class}: #{ex.message}"
      s += "\n\t#{stringify k}" if k.length > 0
      e = RuntimeError.new(s)
      e.set_backtrace ex.backtrace
      raise e
    end
  end

  # ----------------------------------------------------------------------

  # Apply a function, fun, to arguments, args, with a continuation, k.
  private_class_method def self.apply_function(fun, arg, k, env)
    loop {
      case fun
      when :'call/cc'
        k.push_RestoreEnv(env)
        fun = arg.car
        arg = Cell.new(Continuation.new(k), nil)
      when :apply
        fun = arg.car
        arg = arg.cdr.car
      else
        break
      end
    }
    case fun
    when Intrinsic
      if fun.arity >= 0
        if arg.nil? ? fun.arity > 0 : arg.count != fun.arity
          raise ArgumentError,
                "arity not matched: #{fun} and #{stringify arg}"
        end
      end
      return [fun.fun.call(arg), env]
    when Closure
      k.push_RestoreEnv(env)
      k.push(:Begin, fun.body)
      return [NONE,
              Environment.new(nil, # frame maker
                              nil,
                              fun.env.prepend_defs(fun.params, arg))]
    when Continuation
      k.copy_from(fun)
      return [arg.car, env]
    else
      raise ArgumentError,
            "not a function: #{stringify fun} with #{stringify arg}"
    end
  end

  # ----------------------------------------------------------------------

  # Split a string into an abstract sequence of tokens.
  # For "(a 1)" it yields "(", "a", "1" and ")".
  private_class_method def self.split_string_into_tokens(source)
    source.each_line {|line|
      ss = []                   # to store string literals
      x = []
      i = true
      line.split('"').each {|e| # NB: `.each` can be omitted in Ruby 2.7.
        if i
          x << e
        else
          ss << '"' + e         # Store a string literal.
          x << "#s"
        end
        i = not(i)
      }
      s = x.join(" ").split(";")[0] # Ignore "; ...".
      s = s.gsub("'", " ' ").gsub("(", " ( ").gsub(")", " ) ")
      s.split.each {|e|
        if e == "#s"
          yield ss.shift
        else
          yield e
        end
      }
    }
  end

  # Read an expression from tokens.
  # Tokens will be left with the rest of the token strings if any.
  private_class_method def self.read_from_tokens(tokens)
    token = tokens.shift
    raise IndexError if token.nil?
    case token
    when "("
      z = Cell.new(nil, nil)
      y = z
      until tokens.fetch(0) == ")"
        if tokens.fetch(0) == "."
          tokens.shift
          y.cdr = read_from_tokens tokens
          unless tokens.fetch(0) == ")"
            raise RuntimeError, ") is expected: #{tokens.fetch(0)}"
          end
          break
        end
        e = read_from_tokens tokens
        x = Cell.new(e, nil)
        y.cdr = x
        y = x
      end
      tokens.shift
      return z.cdr
    when ")"
      raise RuntimeError, "unexpected )"
    when "'"
      e = read_from_tokens tokens
      return Cell.new(:quote, Cell.new(e, nil)) # (quote e)
    when "#f"
      return false
    when "#t"
      return true
    else
      return token[1..-1] if token[0] == '"'
      return Integer token rescue Float token rescue token.to_sym
    end
  end

  # ----------------------------------------------------------------------

  # Tokens from the standard-in
  STDIN_TOKENS = []
  private_constant :STDIN_TOKENS

  # Read an expression from the console.
  def self.read_expression(prompt1="", prompt2="")
    loop {
      old = Array.new STDIN_TOKENS
      begin
        return read_from_tokens STDIN_TOKENS
      rescue IndexError
        print old.empty? ? prompt1 : prompt2
        $stdout.flush
        line = gets
        return EOF if line.nil?
        STDIN_TOKENS.replace old
        split_string_into_tokens(line) {|token|
          STDIN_TOKENS << token
        }
      rescue
        STDIN_TOKENS.clear
        raise
      end
    }
  end

  # Repeat Read-Eval-Print until End-Of-File.
  def self.read_eval_print_loop
    loop {
      begin
        exp = read_expression("> ", "| ")
        if (exp.equal? EOF)
          puts "Goodbye"
          return
        end
        result = evaluate(exp, GLOBAL_ENV)
        puts stringify result unless result.equal? NONE
      rescue => ex
        puts ex
        # raise
      end
    }
  end

  # Load a source code from a file.
  def self.load(file_name)
    source = File.open(file_name).read()
    tokens = []
    split_string_into_tokens(source) {|t| tokens << t}
    until tokens.empty?
      exp = read_from_tokens(tokens)
      evaluate(exp, GLOBAL_ENV)
    end
  end
end # LittleScheme

# ----------------------------------------------------------------------

if __FILE__ == $0               # The main routine
  LS = LittleScheme
  begin
    unless ARGV.empty?
      LS.load ARGV[0]
      exit 0 if ARGV[1] != "-"
    end
    LS.read_eval_print_loop
  rescue => ex
    puts ex
    puts ex.backtrace
    exit 1
  end
end
