extern crate pest;
// #[macro_use]
extern crate pest_derive;

// use std::fmt::Result;

use std::collections::{HashMap, HashSet};
use std::fmt::{Debug};
use std::hash::Hash;
use std::slice::Iter;

use pest::error::InputLocation;
use pest::iterators::Pair;

use pest::Parser;

#[derive(pest_derive::Parser)]
#[grammar = "saty.pest"]
pub struct SatyParser;

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
    fn all(bs: &mut Iter<bool>) -> bool {
        bs.all(|b| *b)
    }
    fn any(bs: &mut Iter<bool>) -> bool {
        bs.any(|b| *b)
    }
    fn none(bs: &mut Iter<bool>) -> bool {
        bs.all(|b| !b)
    }
    fn one(bs: &mut Iter<bool>) -> bool {
        let mut found = false;
        for b in bs {
            if *b {
                if found {
                    return false;
                }
                found = true;
            }
        }
        return true;
    }

}

enum InfixOp {
    And,
    Or,
    Nor,
    Xnor,
    Nand,
}

impl From<&InfixOp> for fn(bool, bool) -> bool {
    fn from(op: &InfixOp) -> Self {
        match op {
            InfixOp::And => Ops::and,
            InfixOp::Or => Ops::or,
            InfixOp::Nor => Ops::nor,
            InfixOp::Xnor => Ops::xnor,
            InfixOp::Nand => Ops::nand,
        }
    }
}

impl std::fmt::Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            InfixOp::And => write!(f, "AND"),
            InfixOp::Or => write!(f, "OR"),
            InfixOp::Nor => write!(f, "NOR"),
            InfixOp::Xnor => write!(f, "XNOR"),
            InfixOp::Nand => write!(f, "NAND"),
        }
    }
}

enum PrefixBoolOp {
    Not,
}

impl From<&PrefixBoolOp> for fn(bool) -> bool {
    fn from(op: &PrefixBoolOp) -> Self {
        match op {
            PrefixBoolOp::Not => Ops::not,
        }
    }
}

impl std::fmt::Display for PrefixBoolOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PrefixBoolOp::Not => write!(f, "NOT"),
        }
    }
}

enum PrefixOpType {
    Bool(PrefixBoolOp),
    List(PrefixListOp)
}

enum PrefixListOp {
    Any,
    All,
    None,
    One,
}

impl std::fmt::Display for PrefixListOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PrefixListOp::All => write!(f, "ALL"),
            PrefixListOp::Any => write!(f, "ANY"),
            PrefixListOp::None => write!(f, "NONE"),
            PrefixListOp::One => write!(f, "ONE"),
        }
    }
}

impl From<&PrefixListOp> for fn(&mut Iter<bool>) -> bool {
    fn from(op: &PrefixListOp) -> Self {
        match op {
            PrefixListOp::Any => Ops::any,
            PrefixListOp::All => Ops::all,
            PrefixListOp::None => Ops::none,
            PrefixListOp::One => Ops::one,
        }
    }
}


enum Expr {
    Literal(bool),
    Var(String),
    List(Vec<Expr>),
    PrefixBool(PrefixBoolOp, Box<Expr>),
    PrefixList(PrefixListOp, Vec<Expr>),
    Infix(InfixOp, Box<Expr>, Box<Expr>),
}

struct Assignment {
    name: String,
    expr: Expr,
}

struct Saty {
    allowed_variables: Vec<String>,
    assignments: Vec<Assignment>,
    final_expr: Expr,
}

#[derive(Debug)]
struct MissingInputVariable {
    name: String,
}

#[derive(Debug)]
struct ParseError {
    position: usize,
    message: String,
}

#[derive(Debug)]
struct TypeError {
    position: usize,
    message: String,
}

#[derive(Debug)]
struct UnexpectedInputVariable {
    position: usize,
    message: String,
}

#[derive(Debug)]
struct UnexpectedKeyword {
    position: usize,
    message: String,
}

#[derive(Debug)]
enum EvalError {
    MissingInputVariable(MissingInputVariable),
}

#[derive(Debug)]
enum CompileError {
    UnexpectedInputVariable(UnexpectedInputVariable),
    ParseError(ParseError),
    UnexpectedKeyword(UnexpectedKeyword),
    TypeError(TypeError),
}

impl Expr {
    fn eval(&self, variables: &HashMap<&str, bool>) -> Result<bool, EvalError> {
        match self {
            Expr::Literal(b) => Ok(*b),
            Expr::Var(name) => {
                let val =
                    variables.get(name.as_str());
                if let Some (&b) = val {
                    Ok(b)
                } else {
                    Err(EvalError::MissingInputVariable(MissingInputVariable {
                        name: name.clone(),
                    }))
                }
            },
            Expr::PrefixBool(op, expr) => {
                let f: fn (bool) -> bool = op.into();
                Ok(f(expr.eval(variables)?))
            },
            Expr::PrefixList(op, exprs) => {
                let f: fn (&mut Iter<bool>) -> bool = op.into();
                let mut bs = Vec::new();
                for expr in exprs {
                    bs.push(expr.eval(variables)?);
                }
                Ok(f(&mut bs.iter()))
            },
            Expr::Infix(op, lhs, rhs) => {
                let f: fn (bool, bool) -> bool = op.into();
                Ok(f(lhs.eval(variables)?, rhs.eval(variables)?))
            },
            _ => {
                panic!("unknown evaluation for expression type: {:?}", self);
            }
        }
    }

    fn new(item: Pair<Rule>) -> Result<Expr, CompileError> {

        let rule = item.as_rule();
        let text = item.as_str();
        let pos = item.as_span().start();
    
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
                let operand = contents[1].to_owned();
                let expr = Expr::new(operand)?;
                match expr {
                    Expr::List(items) => {
                        let op = match contents[0].as_str() {
                            "ANY" => PrefixListOp::Any,
                            "ALL" => PrefixListOp::All,
                            "NONE" => PrefixListOp::None,
                            "ONE" => PrefixListOp::One,
                            _ => {
                                return Err(CompileError::TypeError(TypeError {
                                    position: contents[0].as_span().start(),
                                    message: format!("expected list operator, but got: {}", contents[0].as_str()),
                                }));
                            }
                        };
                        Ok(Expr::PrefixList(op, items))
                    },
                    _ => {
                        let op = match contents[0].as_str() {
                            "NOT" => PrefixBoolOp::Not,
                            _ => {
                                return Err(CompileError::TypeError(TypeError {
                                    position: contents[0].as_span().start(),
                                    message: format!("expected bool operator, but got: {}", contents[0].as_str()),
                                }));
                            }
                        };
                        Ok(Expr::PrefixBool(op, Box::new(expr)))
                    }
                }
                
            },
            Rule::apply_infix => {
                let lhs = contents[0].to_owned();
                let rhs = contents[2].to_owned();
                let lhs_expr = Expr::new(lhs)?;
                let rhs_expr = Expr::new(rhs)?;
                let op =
                    match contents[1].as_str() {
                        "AND" => InfixOp::And,
                        "OR" => InfixOp::Or,
                        "NOR" => InfixOp::Nor,
                        "XNOR" => InfixOp::Xnor,
                        "NAND" => InfixOp::Nand,
                        s => return Err(CompileError::UnexpectedKeyword(UnexpectedKeyword {
                            position: pos,
                            message: s.to_string()})),
                    };
                Ok(Expr::Infix(op, Box::new(lhs_expr), Box::new(rhs_expr)))
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
                Err(CompileError::ParseError(ParseError {
                    position: pos,
                    message: format!("unexpected rule: {:?}", rule),
                }))
            }
        }
    }
}

impl Saty {

    fn eval<'a>(&self, input_variables: HashMap<&'a str, bool>) -> Result<bool, EvalError> {
        let mut assigned_values = input_variables.clone();
        for Assignment { name, expr } in self.assignments.iter() {
            let value = expr.eval(&assigned_values);
            assigned_values.insert(name.as_str(), value?);
        }
        self.final_expr.eval(&assigned_values)
    }

    fn new<'a >(text: impl ToString, allowed_variables: Iter<impl ToString>)
    -> Result<Saty, CompileError> {

        // parse
        let text_string = text.to_string();
        let text_str = text_string.as_str();
        let mut pairsOrErr = SatyParser::parse(
            Rule::file, 
            text_str);
        let mut pairs = pairsOrErr.or_else(|e| {
            let loc = match e.location {
                InputLocation::Pos(i) => i,
                InputLocation::Span((i, _)) => i};
            Err(CompileError::ParseError(ParseError {
                position: loc,
                message: e.to_string(),
            }))
        })?;
        
    
        let pair = pairs.next().ok_or(CompileError::ParseError(ParseError {
            position: 0,
            message: "Input is empty".to_string(),
        }))?;
        let rule = pair.as_rule();
        let pos = pair.as_span().start();
        let contents: Vec<_> = pair.into_inner().collect();
            
        let saty = 
        // make AST
        if contents.len() == 1 {
            Err(CompileError::ParseError(ParseError {
                position: pos,
                message: "Empty input cannot be converted to Saty".to_string(),
            }))
        }
        else if rule != Rule::file {
            Err(CompileError::ParseError(ParseError {
                position: pos,
                message: format!("Expected rule, got {:?}", rule),
            }))
        }
        else {
            let n_assigns = contents.len() - 2;
            let assign_pairs =
                contents.iter().cloned().take(n_assigns);
            let mut assignments = Vec::new();
            for pair in assign_pairs {
                assignments.push(Assignment::new(pair)?);
            }
            let final_pair =
                contents.iter().skip(n_assigns).next().ok_or(CompileError::ParseError(ParseError {
                    position: 0,
                    message: "Missing final expression".to_string(),
                }))?;
            Ok(Saty {
                allowed_variables: allowed_variables.map(
                    |v| v.to_string()
                ).collect(),
                assignments,
                final_expr: Expr::new(final_pair.to_owned())?,
            })
        }?;

        // check for allowed variables in tree
        let mut variables = HashSet::<String>::new();
        for allowed in saty.allowed_variables.iter() {
            variables.insert(allowed.to_string());
        }
        fn check_variables(expr: &Expr, vars_: &mut HashSet<String>) -> Result<(), CompileError> {
            match expr {
                Expr::Var(name) => {
                    let name_is_defined = vars_.contains(name);
                    if !name_is_defined {
                        Err(CompileError::ParseError(ParseError {
                            position: 0, // TODO get position
                            message: format!("Variable {} is not defined", name),
                        }))
                    }
                    else {
                        Ok(())
                    }
                },
                Expr::PrefixBool(_, expr) => {
                    check_variables(expr.as_ref(), vars_)
                },
                Expr::PrefixList(_, exprs) => {
                    for expr in exprs {
                        check_variables(expr, vars_)?;
                    }
                    Ok(())
                },
                Expr::Infix(_, lhs, rhs) => {
                    check_variables(lhs.as_ref(), vars_)
                    .and(check_variables(rhs.as_ref(), vars_))
                },
                Expr::List(exprs) => {
                    for expr in exprs {
                        check_variables(expr, vars_)?;
                    }
                    Ok(())
                },
                _ => Ok(()),
            }
        }
        for assignment in saty.assignments.iter() {
            check_variables(&assignment.expr, &mut variables)?;
            let name_is_defined = variables.contains(&assignment.name);
            if name_is_defined {
                return Err(CompileError::ParseError(ParseError {
                    position: pos, // TODO: better position
                    message: format!("Variable {} is defined more than once", assignment.name),
                }));
            }
            variables.insert(assignment.name.clone());
        }
        check_variables(&saty.final_expr, &mut variables)?;

        Ok(saty)
    }
}

impl Assignment {
    fn new(item: Pair<Rule>) -> Result<Assignment, CompileError> {
        // let rule = item.as_rule();
        let text = item.as_str();
        let pos = item.as_span().start();

        let contents: Vec<_> = item.into_inner().collect();
        if contents.len() != 2 {
            Err(CompileError::ParseError(ParseError {
                position: pos,
                message: format!("Missing expression in assignment statement {}", text),
            }))
        }
        else {
            let name_pair = contents[0].to_owned();
            let expr_pair = contents[1].to_owned();
            if name_pair.as_rule() != Rule::variable {
                Err(CompileError::ParseError(ParseError {
                    position: pos,
                    message: format!("Malformed assignment statement {}", text),
                }))
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
            Expr::PrefixList(op, exprs) => {
                write!(f, "{}[", op)?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{:?}", expr)?;
                }
                write!(f, "]")
            }
            Expr::PrefixBool(op, expr) => write!(f, "{} ({:?})", op, expr),
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
    let saty = Saty::new(input, allowed_variables.iter()).unwrap();
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
