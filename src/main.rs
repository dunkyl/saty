extern crate pest;
// #[macro_use]
extern crate pest_derive;

// use std::fmt::Result;

use std::fmt::Debug;

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

fn make_expr(item: Pair<Rule>) -> Result<Expr, String> {

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
            let expr = make_expr(operand)?;
            Ok(Expr::Prefix(op.to_string(), Box::new(expr)))
        }
        Rule::apply_infix => {
            let lhs = contents[0].to_owned();
            let op = contents[1].as_str();
            let rhs = contents[2].to_owned();
            let lhs_expr = make_expr(lhs)?;
            let rhs_expr = make_expr(rhs)?;
            Ok(Expr::Infix(op.to_string(), Box::new(lhs_expr), Box::new(rhs_expr)))
        }
        Rule::list => {
            let mut exprs = Vec::new();
            for item in contents {
                let expr = make_expr(item)?;
                exprs.push(expr);
            }
            Ok(Expr::List(exprs))
        }
        _ => {
            Err(format!("Unknown rule: {:?} {}", rule, text))
        }
    }
}

fn make_assignment(item: Pair<Rule>) -> Result<Assignment, String> {
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
            let expr = make_expr(expr_pair)?;
            Ok(Assignment {
                name: name.to_string(),
                expr: expr,
            })
        }
    }
}

fn make_saty(item: Pair<Rule>) -> Result<Saty, String> {
    // item.as_rule()
    let rule = item.as_rule();
    // let text = item.as_str();

    let contents: Vec<_> = item.into_inner().collect();

    // for inner in contents.iter() {
    //     println!("{:?}", inner);
    // }

    if contents.len() == 1 {
        return Err("Empty input cannot be converted to Saty".to_string());
    }
    else if rule != Rule::file {
        Err(format!("Expected rule, got {:?}", rule))
    }
    else {
        let n_assigns = contents.len() - 2;
        let assign_pairs = contents.iter().cloned().take(n_assigns);
        let assignments = assign_pairs.map(make_assignment).collect::<Result<Vec<Assignment>, String>>()?;
        let final_pair = contents.iter().skip(n_assigns).next().ok_or("No final expression")?;
        Ok(Saty {
            assignments,
            final_expr: make_expr(final_pair.to_owned())?,
        })
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
    let input = "(NOT ANY [ orange peach (pink) ]) OR red AND NOT green";
    println!("{}", input);

    // pest parses the input
    let successful_parse =
        SatyParser::parse(
            Rule::file, 
            input).expect("failed to parse input").next().unwrap();

    // turn the parse result into an AST
    println!("{:?}", make_saty(successful_parse));
}
