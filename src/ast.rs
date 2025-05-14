// Copyright (C) 2025 Devin Rockwell
//
// This file is part of graphene.
//
// graphene is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// graphene is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with graphene.  If not, see <https://www.gnu.org/licenses/>.

use crate::token::Token;

#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub pragmas: Vec<Pragma<'a>>,
    pub declarations: Vec<Declaration>,
}

impl<'a> Program<'a> {
    pub fn new() -> Self {
        Program {
            pragmas: Vec::new(),
            declarations: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Pragma<'a> {
    pub name: &'a Token,
    pub arguments: Vec<&'a Token>,
}

impl<'a> Pragma<'a> {
    pub fn new(name: &'a Token, arguments: Vec<&'a Token>) -> Self {
        Pragma { name, arguments }
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Entity(EntityDeclaration),
    System {
        decorators: Vec<Decorator>,
        name: Token,
        generics: Vec<Generic>,
        parameters: Vec<(Token, Type)>,
        body: Vec<Expression>,
    },
    Function {
        decorators: Vec<Decorator>,
        name: Token,
        generics: Vec<Generic>,
        parameters: Vec<(Token, Type)>,
        return_type: Type,
        body: Vec<Expression>,
    },
    Extern {
        name: Token,
        parameters: Vec<(Token, Type)>,
        return_type: Type,
    },
    Decorator {
        name: Token,
        body: Vec<Expression>,
    },
    Macro {
        name: Token,
        body: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub name: Token,
    pub constraints: Vec<Type>,
}

#[derive(Debug, Clone)]
pub enum EntityDeclaration {
    Entity {
        is_const: bool,
        name: Token,
        component_initializers: Vec<ComponentInitializer>,
    },
    Component {
        name: Token,
        // TODO: generics
        field_declarations: Vec<FieldDeclaration>,
    },
    Enum {
        name: Token,
        variants: Vec<VariantDeclaration>,
    },
}

#[derive(Debug, Clone)]
pub struct ComponentInitializer {
    pub component: Type,
    pub fields: Vec<(Option<Token>, Expression)>,
}

#[derive(Debug, Clone)]
pub struct FieldDeclaration {
    pub is_noinit: bool,
    pub name: Token,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub enum VariantDeclaration {
    Basic(Token),
    Tuple(Token, Vec<Type>),
    Struct(Token, Vec<(Token, Type)>),
}

#[derive(Debug, Clone)]
pub struct Decorator {
    pub name: Token,
    pub arguments: Vec<Token>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),
    Entity(Token, Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Declaration(Declaration),
    Drop(Box<Expression>),
    Add(Box<Expression>, ComponentInitializer),
    Remove(Box<Expression>, ComponentInitializer),
    For {
        variable: Token,
        iterable: Box<Expression>,
        body: Vec<Expression>,
    },
    Break(Option<Box<Expression>>),
    Continue,
    While {
        condition: Box<Expression>,
        body: Vec<Expression>,
        else_body: Option<Vec<Expression>>,
    },
    Match {
        expression: Box<Expression>,
        cases: Vec<MatchArm>,
    },
    If {
        condition: Box<Expression>,
        body: Vec<Expression>,
        else_if: Vec<()>,
        else_body: Option<Vec<Expression>>,
    },
    Assignement {
        lhs: (Option<Box<Expression>>, Token),
        rhs: Box<Expression>,
    },
    Infix {
        lhs: Box<Expression>,
        operator: Token,
        rhs: Box<Expression>,
    },
    Prefix {
        operator: Token,
        rhs: Box<Expression>,
    },
    FunctionCall {
        name: Token,
        arguments: Vec<Expression>,
    },
    MacroCall {
        name: Token,
        arguments: Vec<Token>,
    },
    Call {
        name: Token,
        arguments: Vec<Token>,
    },
    Literal(Token),
    ArrayLiteral(Vec<Expression>),
    TupleLiteral(Vec<Expression>),
    MapLiteral(Vec<(Expression, Expression)>),
    SetLiteral(Vec<Expression>),
    True,
    False,
    Null,
    Variable(Token),
    Temp(Vec<Token>),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Literal(Token),
    Binding {
        name: Token,
        pattern: Box<Pattern>,
    },
    Enum {
        name: Token,
        arguments: Vec<Pattern>,
    },
    Tuple(Vec<Pattern>),
    Entity(Token, Vec<(Token, Pattern)>),
    Array(Vec<Pattern>),
    Map(Vec<(Pattern, Pattern)>),
    Set(Vec<Pattern>),
}
