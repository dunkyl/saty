extern crate pest;
// #[macro_use]
extern crate pest_derive;

// use std::fmt::Result;

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use pest::iterators::Pair;

use pest::Parser;

#[derive(pest_derive::Parser)]
#[grammar = "saty.pest"]
pub struct SatyParser;

type SatyOpBBB = fn(bool, bool) -> bool;
type SatyOpBB = fn(bool) -> bool;
type SatyOpListB = fn(Vec<bool>) -> bool;

struct Ops {
    // op: fn(bool) -> bool,
}

impl Ops {
    fn not(b: bool) -> bool {
        !b
    }
    fn and(b1: bool, b2: bool) -> bool {
        b1 && b2
    }
    fn or(b1: bool, b2: bool) -> bool {
        b1 || b2
    }
    fn xor(b1: bool, b2: bool) -> bool {
        b1 ^ b2
    }
    fn nand(b1: bool, b2: bool) -> bool {
        !(b1 && b2)
    }
    fn nor(b1: bool, b2: bool) -> bool {
        !(b1 || b2)
    }
    fn xnor(b1: bool, b2: bool) -> bool {
        !(b1 ^ b2)
    }
    fn all(bs: Vec<bool>) -> bool {
        bs.iter().all(|b| *b)
    }
    fn any(bs: Vec<bool>) -> bool {
        bs.iter().any(|b| *b)
    }
    fn none(bs: Vec<bool>) -> bool {
        bs.iter().all(|b| !b)
    }
    fn one(bs: Vec<bool>) -> bool {
        bs.iter().any(|b| *b) && bs.iter().all(|b| !b)
    }

}

enum Expr {
    Literal(bool),
    Var(String),
    List(Vec<Expr>),
    Prefix(String, Box<Expr>),
    Infix(String, Box<Expr>, Box<Expr>),
}

struct Assignment {
    name: String,
    expr: Expr,
}

struct Saty {
    assignments: Vec<Assignment>,
    final_expr: Expr,
}

impl Expr {
    fn eval(&self, variables: &HashMap<&str, bool>) -> bool {
        match self {
            Expr::Literal(b) => *b,
            Expr::Var(name) => *variables.get(name.as_str()).unwrap(),
            Expr::Prefix(op, expr) =>
                match expr.as_ref() {
                    Expr::List(exprs) => {
                        let bs = exprs.iter().map(|e| e.eval(variables)).collect();
                        match op.as_str() {
                            "ALL" => Ops::all(bs),
                            "ANY" => Ops::any(bs),
                            "NONE" => Ops::none(bs),
                            "ONE" => Ops::one(bs),
                            _ => panic!("Unknown prefix operator for type list: {}", op),
                        }
                    },
                    _ => {
                        match op.as_str() {
                            "NOT" => Ops::not(expr.eval(variables)),
                            _ => panic!("unknown prefix operator for type bool: {}", op),
                        }
                    }
                    
                    
                    
                    
            },
            Expr::Infix(op, lhs, rhs) => match op.as_str() {
                "AND" => Ops::and(lhs.eval(variables), rhs.eval(variables)),
                "OR" => Ops::or(lhs.eval(variables), rhs.eval(variables)),
                "XOR" => Ops::xor(lhs.eval(variables), rhs.eval(variables)),
                "NAND" => Ops::nand(lhs.eval(variables), rhs.eval(variables)),
                "NOR" => Ops::nor(lhs.eval(variables), rhs.eval(variables)),
                "XNOR" => Ops::xnor(lhs.eval(variables), rhs.eval(variables)),
                _ => panic!("unknown infix operator: {}", op),
            },
            _ => {
                panic!("unknown evaluation for expression type: {:?}", self);
            }
        }
    }

    fn new(item: Pair<Rule>) -> Result<Expr, String> {

        let rule = item.as_rule();
        let text = item.as_str();
    
        let contents: Vec<_> = item.into_inner().collect();
    
        match rule {
            Rule::literal => {
                let value = contents[0].as_str() == "TRUE";
                Ok(Expr::Literal(value))
            }
            Rule::variable => {
                Ok(Expr::Var(text.to_string()))
            }
            Rule::apply_prefix => {
                let op = contents[0].as_str();
                let operand = contents[1].to_owned();
                // let expr = contents[1].as_str();
                let expr = Expr::new(operand)?;
                Ok(Expr::Prefix(op.to_string(), Box::new(expr)))
            }
            Rule::apply_infix => {
                let lhs = contents[0].to_owned();
                let op = contents[1].as_str();
                let rhs = contents[2].to_owned();
                let lhs_expr = Expr::new(lhs)?;
                let rhs_expr = Expr::new(rhs)?;
                Ok(Expr::Infix(op.to_string(), Box::new(lhs_expr), Box::new(rhs_expr)))
            }
            Rule::list => {
                let mut exprs = Vec::new();
                for item in contents {
                    let expr = Expr::new(item)?;
                    exprs.push(expr);
                }
                Ok(Expr::List(exprs))
            }
            _ => {
                Err(format!("Unknown rule: {:?} {}", rule, text))
            }
        }
    }
}

impl Saty {

    fn eval<'a>(&self, input_variables: HashMap<&'a str, bool>) -> bool {
        let mut assigned_values = input_variables.clone();
        for Assignment { name, expr } in self.assignments.iter() {
            let value = expr.eval(&assigned_values);
            assigned_values.insert(name.as_str(), value);
        }
        self.final_expr.eval(&assigned_values)
    }

    fn new<'a >(text: impl ToString, allowed_variables: impl Into<Option<Vec<&'a str>>>) -> Result<Saty, String> {
        // item.as_rule()
        
        // let text = item.as_str();
        let text_string = text.to_string();
        let text_str = text_string.as_str();
        let mut pairs = SatyParser::parse(
            Rule::file, 
            text_str).ok().ok_or("Failed to parse saty")?;
    
        let pair = pairs.next().ok_or("Input was empty")?;
    
        let rule = pair.as_rule();
    
        let contents: Vec<_> = pair.into_inner().collect();
    
        if contents.len() == 1 {
            return Err("Empty input cannot be converted to Saty".to_string());
        }
        else if rule != Rule::file {
            Err(format!("Expected rule, got {:?}", rule))
        }
        else {
            let n_assigns = contents.len() - 2;
            let assign_pairs = contents.iter().cloned().take(n_assigns);
            let assignments = assign_pairs.map(Assignment::new).collect::<Result<Vec<Assignment>, String>>()?;
            let final_pair = contents.iter().skip(n_assigns).next().ok_or("No final expression")?;
            Ok(Saty {
                assignments,
                final_expr: Expr::new(final_pair.to_owned())?,
            })
        }
    }
}

impl Assignment {
    fn new(item: Pair<Rule>) -> Result<Assignment, String> {
        // let rule = item.as_rule();
        let text = item.as_str();

        let contents: Vec<_> = item.into_inner().collect();
        if contents.len() != 2 {
            Err(format!("Malformed assignment statement {}", text))
        }
        else {
            let name_pair = contents[0].to_owned();
            let expr_pair = contents[1].to_owned();
            if name_pair.as_rule() != Rule::variable {
                Err(format!("Malformed assignment statement {}", text))
            }
            // else if expr_pair.as_rule() != Rule::expr {
            //     Err(format!("Malformed assignment statement {}", text))
            // }
            else {
                let name = name_pair.as_str();
                let expr = Expr::new(expr_pair)?;
                Ok(Assignment {
                    name: name.to_string(),
                    expr: expr,
                })
            }
        }
    }
}


impl Debug for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Assignment({}, {:?})", self.name, self.expr)
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::List(exprs) => {
                write!(f, "[")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", expr)?;
                }
                write!(f, "]")
            }
            Expr::Prefix(op, expr) => write!(f, "{} ({:?})", op, expr),
            Expr::Infix(op, lhs, rhs) => write!(f, "({:?} {} {:?})", lhs, op, rhs),
        }
    }
}

impl Debug for Saty {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Saty {{assignments: [")?;
        for (i, a) in self.assignments.iter().enumerate() {
            write!(f, "{:?}", a)?;
            if i < self.assignments.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "], final_expr: {:?} }}", self.final_expr)
    }
}

fn main() {

    let allowed_variables = vec!["orange", "peach", "pink", "red", "green", "blue", "yellow"];

    let input = "(NOT ANY [ orange peach (pink) ]) OR red AND NOT green";
    println!("{}", input);

    // parse result into an AST
    let saty = Saty::new(input, allowed_variables).unwrap();
    println!("{:?}", saty);

    let mut input_variables = HashMap::new();
    input_variables.insert("orange", true);
    input_variables.insert("peach", true);
    input_variables.insert("pink", false);
    input_variables.insert("red", true);
    input_variables.insert("green", false);

    let result = saty.eval(input_variables);
    println!("{:?}", result);
}
