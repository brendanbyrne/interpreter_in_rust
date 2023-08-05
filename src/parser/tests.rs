use super::*;

#[test]
fn let_statements() {
    struct TestCase<'a> {
        input: &'a str,
        expected_name: &'a str,
        expected_expression: ast::Expression,
    }

    let test_cases = vec![
        TestCase {
            input: "let x = 5;",
            expected_name: "x",
            expected_expression: ast::Expression::Int(5),
        },
        TestCase {
            input: "let y = 10;",
            expected_name: "y",
            expected_expression: ast::Expression::Int(10),
        },
        TestCase {
            input: "let foobar = 838383;",
            expected_name: "foobar",
            expected_expression: ast::Expression::Int(838383),
        },
    ];

    for test_case in test_cases {
        let mut program = parse_program(test_case.input).unwrap();
        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::Let(name, expression) = program.statements.pop().unwrap() {
            assert_eq!(name, test_case.expected_name);
            assert_eq!(expression, test_case.expected_expression);
        } else {
            panic!("Expected type ast::Statement::Let");
        }
    }
}

#[test]
fn return_statements() {
    struct TestCase<'a> {
        input: &'a str,
        expected_expression: ast::Expression,
    }

    let test_cases = vec![
        TestCase {
            input: "return 5;",
            expected_expression: ast::Expression::Int(5),
        },
        TestCase {
            input: "return 10;",
            expected_expression: ast::Expression::Int(10),
        },
        TestCase {
            input: "return 993322",
            expected_expression: ast::Expression::Int(993322),
        },
    ];

    for test_case in test_cases {
        let mut program = parse_program(test_case.input).unwrap();
        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::Return(expression) = program.statements.pop().unwrap() {
            assert_eq!(expression, test_case.expected_expression);
        } else {
            panic!("Expected type ast::Statement::Return");
        }
    }
}

#[test]
fn identifier_expression() {
    let mut program = parse_program("foobar").unwrap();
    assert_eq!(program.statements.len(), 1);

    if let ast::Expression::Identifier(name) = get_expression(&mut program) {
        assert_eq!("foobar", name);
    } else {
        panic!("Expected type ast::Expression::Indentifier");
    }
}

#[test]
fn integer_literal_expression() {
    let mut program = parse_program("5").unwrap();
    assert_eq!(program.statements.len(), 1);

    if let ast::Expression::Int(value) = get_expression(&mut program) {
        assert_eq!(5, value);
    } else {
        panic!("Expected type ast::Expression::Indentifier");
    }
}

#[test]
fn prefix_expression() {
    struct TestCase<'a> {
        input: &'a str,
        op: ast::PrefixOperator,
        value: ast::Expression,
    }

    let test_cases = vec![
        TestCase {
            input: "!5",
            op: ast::PrefixOperator::Not,
            value: ast::Expression::Int(5),
        },
        TestCase {
            input: "-15",
            op: ast::PrefixOperator::Negate,
            value: ast::Expression::Int(15),
        },
        TestCase {
            input: "!true",
            op: ast::PrefixOperator::Not,
            value: ast::Expression::Bool(true),
        },
        TestCase {
            input: "!false",
            op: ast::PrefixOperator::Not,
            value: ast::Expression::Bool(false),
        },
    ];

    for test_case in test_cases {
        let mut program = parse_program(test_case.input).unwrap();
        assert_eq!(program.statements.len(), 1);

        if let ast::Expression::Prefix(op, nested_expression) = get_expression(&mut program) {
            assert_eq!(op, test_case.op);
            assert_eq!(*nested_expression, test_case.value);
        } else {
            panic!("Expected type ast::Expression::Prefix");
        }
    }
}

#[test]
fn infix_expression() {
    struct TestCase<'a> {
        input: &'a str,
        op: ast::InfixOperator,
        lhs: ast::Expression,
        rhs: ast::Expression,
    }

    let test_cases = vec![
        TestCase {
            input: "5 + 5",
            op: ast::InfixOperator::Plus,
            lhs: ast::Expression::Int(5),
            rhs: ast::Expression::Int(5),
        },
        TestCase {
            input: "5 - 5",
            op: ast::InfixOperator::Minus,
            lhs: ast::Expression::Int(5),
            rhs: ast::Expression::Int(5),
        },
        TestCase {
            input: "5 * 5",
            op: ast::InfixOperator::Multiply,
            lhs: ast::Expression::Int(5),
            rhs: ast::Expression::Int(5),
        },
        TestCase {
            input: "5 / 5",
            op: ast::InfixOperator::Divide,
            lhs: ast::Expression::Int(5),
            rhs: ast::Expression::Int(5),
        },
        TestCase {
            input: "5 > 5",
            op: ast::InfixOperator::GreaterThan,
            lhs: ast::Expression::Int(5),
            rhs: ast::Expression::Int(5),
        },
        TestCase {
            input: "5 < 5",
            op: ast::InfixOperator::LessThan,
            lhs: ast::Expression::Int(5),
            rhs: ast::Expression::Int(5),
        },
        TestCase {
            input: "5 == 5",
            op: ast::InfixOperator::Equal,
            lhs: ast::Expression::Int(5),
            rhs: ast::Expression::Int(5),
        },
        TestCase {
            input: "5 != 5",
            op: ast::InfixOperator::NotEqual,
            lhs: ast::Expression::Int(5),
            rhs: ast::Expression::Int(5),
        },
        TestCase {
            input: "true == true",
            op: ast::InfixOperator::Equal,
            lhs: ast::Expression::Bool(true),
            rhs: ast::Expression::Bool(true),
        },
        TestCase {
            input: "true != false",
            op: ast::InfixOperator::NotEqual,
            lhs: ast::Expression::Bool(true),
            rhs: ast::Expression::Bool(false),
        },
        TestCase {
            input: "false == false",
            op: ast::InfixOperator::Equal,
            lhs: ast::Expression::Bool(false),
            rhs: ast::Expression::Bool(false),
        },
    ];

    for test_case in test_cases {
        let mut program = parse_program(test_case.input).unwrap();
        assert_eq!(program.statements.len(), 1);

        if let ast::Expression::Infix(op, lhs, rhs) = get_expression(&mut program) {
            assert_eq!(op, test_case.op);
            assert_eq!(*lhs, test_case.lhs);
            assert_eq!(*rhs, test_case.rhs);
        } else {
            panic!("Expected type ast::Expression::Infix");
        }
    }
}

#[test]
fn operator_precedence() {
    struct TestCase<'a> {
        input: &'a str,
        expected: &'a str,
        len: usize,
    }

    let test_cases = vec![
        TestCase {
            input: "-a * b",
            expected: "((-a) * b);",
            len: 1,
        },
        TestCase {
            input: "!-a",
            expected: "(!(-a));",
            len: 1,
        },
        TestCase {
            input: "a + b + c",
            expected: "((a + b) + c);",
            len: 1,
        },
        TestCase {
            input: "a * b * c",
            expected: "((a * b) * c);",
            len: 1,
        },
        TestCase {
            input: "a * b / c",
            expected: "((a * b) / c);",
            len: 1,
        },
        TestCase {
            input: "a + b / c",
            expected: "(a + (b / c));",
            len: 1,
        },
        TestCase {
            input: "a + b * c + d / e - f",
            expected: "(((a + (b * c)) + (d / e)) - f);",
            len: 1,
        },
        TestCase {
            input: "3 + 4; -5 * 5",
            expected: "(3 + 4);\n((-5) * 5);",
            len: 2,
        },
        TestCase {
            input: "5 > 4 == 3 < 4",
            expected: "((5 > 4) == (3 < 4));",
            len: 1,
        },
        TestCase {
            input: "5 < 4 != 3 > 4",
            expected: "((5 < 4) != (3 > 4));",
            len: 1,
        },
        TestCase {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            len: 1,
        },
        TestCase {
            input: "true",
            expected: "true;",
            len: 1,
        },
        TestCase {
            input: "false",
            expected: "false;",
            len: 1,
        },
        TestCase {
            input: "3 > 5 == false",
            expected: "((3 > 5) == false);",
            len: 1,
        },
        TestCase {
            input: "3 < 5 == true",
            expected: "((3 < 5) == true);",
            len: 1,
        },
        TestCase {
            input: "1 + (2 + 3) + 4",
            expected: "((1 + (2 + 3)) + 4);",
            len: 1,
        },
        TestCase {
            input: "(5 + 5) * 2",
            expected: "((5 + 5) * 2);",
            len: 1,
        },
        TestCase {
            input: "2 / (5 + 5)",
            expected: "(2 / (5 + 5));",
            len: 1,
        },
        TestCase {
            input: "-(5 + 5)",
            expected: "(-(5 + 5));",
            len: 1,
        },
        TestCase {
            input: "!(true == true)",
            expected: "(!(true == true));",
            len: 1,
        },
        TestCase {
            input: "a + add(b * c) + d",
            expected: "((a + add((b * c))) + d);",
            len: 1,
        },
        TestCase {
            input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            len: 1,
        },
        TestCase {
            input: "add(a + b + c * d / f + g)",
            expected: "add((((a + b) + ((c * d) / f)) + g));",
            len: 1,
        },
    ];

    for test_case in test_cases {
        let program = parse_program(test_case.input).unwrap();
        assert_eq!(program.statements.len(), test_case.len);
        assert_eq!(program.to_string(), test_case.expected);
    }
}

#[test]
fn if_expression() {
    let mut program = parse_program("if (x < y) { x }").unwrap();

    use ast::Expression::{Identifier, Infix};
    use ast::InfixOperator::LessThan;
    use ast::Statement::{Block, Expression};

    let expected_cond = Infix(
        LessThan,
        Box::new(Identifier("x".to_owned())),
        Box::new(Identifier("y".to_owned())),
    );

    let expected_if_true = Block(vec![Expression(Identifier("x".to_owned()))]);

    assert_eq!(program.statements.len(), 1);

    if let ast::Expression::If(cond, if_true) = get_expression(&mut program) {
        assert_eq!(*cond, expected_cond);
        assert_eq!(*if_true, expected_if_true);
    } else {
        panic!("Expected If expression");
    }
}

#[test]
fn if_else_expression() {
    let mut program = parse_program("if (x < y) { x } else { y }").unwrap();

    use ast::Expression::{Identifier, Infix};
    use ast::InfixOperator::LessThan;
    use ast::Statement::{Block, Expression};

    let expected_cond = Infix(
        LessThan,
        Box::new(Identifier("x".to_owned())),
        Box::new(Identifier("y".to_owned())),
    );

    let expected_if_true = Block(vec![Expression(Identifier("x".to_owned()))]);

    let expected_if_false = Block(vec![Expression(Identifier("y".to_owned()))]);

    assert_eq!(program.statements.len(), 1);

    if let ast::Expression::IfElse(cond, if_true, if_false) = get_expression(&mut program) {
        assert_eq!(*cond, expected_cond);
        assert_eq!(*if_true, expected_if_true);
        assert_eq!(*if_false, expected_if_false);
    } else {
        panic!("Expected ast::Expression::If expression");
    }
}

#[test]
fn function_literal_expression() {
    let mut program = parse_program("fn(x, y) { x + y; }").unwrap();
    assert_eq!(program.statements.len(), 1);

    use ast::Expression::{Identifier, Infix};
    use ast::InfixOperator::Plus;
    use ast::Statement::{Block, Expression};

    let x = Identifier("x".to_owned());
    let y = Identifier("y".to_owned());
    let expected_params = vec![x.clone(), y.clone()];
    let expected_body = Block(vec![Expression(Infix(Plus, Box::new(x), Box::new(y)))]);

    if let ast::Expression::Function(params, body) = get_expression(&mut program) {
        assert_eq!(params, expected_params);
        assert_eq!(*body, expected_body);
    } else {
        panic!("Expected ast::Expression::Function");
    }
}

#[test]
fn function_parameters() {
    struct TestCase<'a> {
        input: &'a str,
        expected_params: Vec<ast::Expression>,
    }

    fn make_id(id: &str) -> ast::Expression {
        return ast::Expression::Identifier(id.to_owned());
    }

    let test_cases = vec![
        TestCase {
            input: "fn() {};",
            expected_params: vec![],
        },
        TestCase {
            input: "fn(x) {};",
            expected_params: vec![make_id("x")],
        },
        TestCase {
            input: "fn(x, y, z) {};",
            expected_params: vec![make_id("x"), make_id("y"), make_id("z")],
        },
    ];

    for test_case in test_cases {
        let mut program = parse_program(test_case.input).unwrap();

        if let ast::Expression::Function(params, _) = get_expression(&mut program) {
            assert_eq!(params, test_case.expected_params);
        } else {
            panic!("Expected ast::Expression::Function");
        }
    }
}

#[test]
fn call_expression() {
    let mut program = parse_program("add(1, 2 * 3, 4 + 5);").unwrap();
    assert_eq!(program.statements.len(), 1);

    use ast::Expression::{Identifier, Infix, Int};
    use ast::InfixOperator::{Multiply, Plus};

    let expected_name = Identifier("add".to_owned());
    let expected_inputs = vec![
        Int(1),
        Infix(Multiply, Box::new(Int(2)), Box::new(Int(3))),
        Infix(Plus, Box::new(Int(4)), Box::new(Int(5))),
    ];

    if let ast::Expression::Call(name, inputs) = get_expression(&mut program) {
        assert_eq!(*name, expected_name);
        assert_eq!(inputs, expected_inputs);
    } else {
        panic!("Expected ast::Expression::Call");
    }
}

fn get_expression(program: &mut Program) -> ast::Expression {
    if let ast::Statement::Expression(expression) = program.statements.pop().unwrap() {
        return expression;
    } else {
        panic!("Expected ast::Statement::Expression");
    }
}
