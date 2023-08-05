use super::*;
use crate::parser::parse_program;

#[test]
fn eval() {
    struct TestCase<'a> {
        input: &'a str,
        expected_obj: Object,
    }

    let test_cases = vec![
        TestCase {
            input: "5",
            expected_obj: Object::Int(5),
        },
        TestCase {
            input: "10",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "true",
            expected_obj: TRUE,
        },
        TestCase {
            input: "false",
            expected_obj: FALSE,
        },
        TestCase {
            input: "!true",
            expected_obj: FALSE,
        },
        TestCase {
            input: "!false",
            expected_obj: TRUE,
        },
        TestCase {
            input: "!5",
            expected_obj: FALSE,
        },
        TestCase {
            input: "!!true",
            expected_obj: TRUE,
        },
        TestCase {
            input: "!!false",
            expected_obj: FALSE,
        },
        TestCase {
            input: "!!5",
            expected_obj: TRUE,
        },
        TestCase {
            input: "-5",
            expected_obj: Object::Int(-5),
        },
        TestCase {
            input: "--5",
            expected_obj: Object::Int(5),
        },
        TestCase {
            input: "5 + 5 + 5 + 5 - 10",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "2 * 2 * 2 * 2 * 2",
            expected_obj: Object::Int(32),
        },
        TestCase {
            input: "-50 + 100 + -50",
            expected_obj: Object::Int(0),
        },
        TestCase {
            input: "5 * 2 + 10",
            expected_obj: Object::Int(20),
        },
        TestCase {
            input: "5 + 2 * 10",
            expected_obj: Object::Int(25),
        },
        TestCase {
            input: "20 + 2 * -10",
            expected_obj: Object::Int(0),
        },
        TestCase {
            input: "50 / 2 * 2 + 10",
            expected_obj: Object::Int(60),
        },
        TestCase {
            input: "2 * (5 + 10)",
            expected_obj: Object::Int(30),
        },
        TestCase {
            input: "3 * 3 * 3 + 10",
            expected_obj: Object::Int(37),
        },
        TestCase {
            input: "3 * (3 * 3) + 10",
            expected_obj: Object::Int(37),
        },
        TestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected_obj: Object::Int(50),
        },
        TestCase {
            input: "if (true) { 10 }",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "if (false) { 10 }",
            expected_obj: NOOP,
        },
        TestCase {
            input: "if (1) { 10 }",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "if (1 < 2) { 10 }",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "if (1 > 2) { 10 }",
            expected_obj: NOOP,
        },
        TestCase {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected_obj: Object::Int(20),
        },
        TestCase {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "return 10;",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "return 10; 9;",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "return 2 * 5;",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "9; return 2 * 5; 9;",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "let a = 5; a;",
            expected_obj: Object::Int(5),
        },
        TestCase {
            input: "let a = 5 * 5; a;",
            expected_obj: Object::Int(25),
        },
        TestCase {
            input: "let a = 5; let b = a; b;",
            expected_obj: Object::Int(5),
        },
        TestCase {
            input: "let a = 5; let b = a; let c = a + b + 5; c;",
            expected_obj: Object::Int(15),
        },
        TestCase {
            input: "fn(x) { x + 2; };",
            expected_obj: Object::Function(
                vec!["x".to_owned()],
                ast::Statement::Block(vec![ast::Statement::Expression(ast::Expression::Infix(
                    ast::InfixOperator::Plus,
                    Box::new(ast::Expression::Identifier("x".to_owned())),
                    Box::new(ast::Expression::Int(2)),
                ))]),
                Rc::new(RefCell::new(Env::default())),
            ),
        },
        TestCase {
            input: "let identity = fn(x) { x; }; identity(5);",
            expected_obj: Object::Int(5),
        },
        TestCase {
            input: "let identity = fn(x) { return x; }; identity(5);",
            expected_obj: Object::Int(5),
        },
        TestCase {
            input: "let double = fn(x) { x * 2; }; double(5);",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "let add = fn(x, y) { x + y; }; add(5, 5);",
            expected_obj: Object::Int(10),
        },
        TestCase {
            input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            expected_obj: Object::Int(20),
        },
        TestCase {
            input: "
            let new_adder = fn(x) { fn(y) { x + y; }; };
            let add_two = new_adder(2);
            add_two(2);",
            expected_obj: Object::Int(4),
        },
        TestCase {
            input: "
            let counter = fn(x) {
              if (x < 1) {
                counter(x + 1);
              }
            };

            counter(0);",
            expected_obj: NOOP,
        },
    ];

    for test_case in test_cases {
        let program = parse_program(test_case.input).unwrap();
        let mut evaluator = Evaluator::new();

        let obj = evaluator.eval(program).unwrap();
        assert_eq!(obj, test_case.expected_obj);
    }
}

#[test]
fn errors() {
    struct TestCase<'a> {
        input: &'a str,
        expected_error: Error,
    }

    let test_cases = vec![
        TestCase {
            input: "5 + true;",
            expected_error: Error::InfixTypeMismatch(
                ast::InfixOperator::Plus,
                Object::Int(5),
                Object::Bool(true),
            ),
        },
        TestCase {
            input: "5 + true; 5;",
            expected_error: Error::InfixTypeMismatch(
                ast::InfixOperator::Plus,
                Object::Int(5),
                TRUE,
            ),
        },
        TestCase {
            input: "-true",
            expected_error: Error::UnsupportedNegate(TRUE),
        },
        TestCase {
            input: "true + false;",
            expected_error: Error::InfixTypeMismatch(ast::InfixOperator::Plus, TRUE, FALSE),
        },
        TestCase {
            input: "5; true + false; 5;",
            expected_error: Error::InfixTypeMismatch(ast::InfixOperator::Plus, TRUE, FALSE),
        },
        TestCase {
            input: "if (10 > 1) { true + false; }",
            expected_error: Error::InfixTypeMismatch(ast::InfixOperator::Plus, TRUE, FALSE),
        },
        TestCase {
            input: "
    if (10 > 1) {
      if (10 > 1) {
        return true + false;
      }

      return 1;
    }",
            expected_error: Error::InfixTypeMismatch(ast::InfixOperator::Plus, TRUE, FALSE),
        },
        TestCase {
            input: "foobar",
            expected_error: Error::IdNotFound("foobar".to_owned()),
        },
    ];

    for test_case in test_cases {
        let program = parse_program(test_case.input).unwrap();
        let mut evaluator = Evaluator::new();
        assert_eq!(
            evaluator.eval(program).unwrap_err(),
            test_case.expected_error
        );
    }
}
