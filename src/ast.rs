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
    pub declarations: Vec<Declaration<'a>>,
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
pub enum Declaration<'a> {
    Entity {
        decorators: Vec<Decorator<'a>>,
        entity: EntityDeclaration<'a>,
    },
    System {
        decorators: Vec<Decorator<'a>>,
        name: &'a Token,
        generics: Vec<Generic<'a>>,
        parameters: Vec<(Token, Type<'a>)>,
        body: Vec<Expression<'a>>,
    },
    Function {
        decorators: Vec<Decorator<'a>>,
        name: &'a Token,
        generics: Vec<Generic<'a>>,
        parameters: Vec<(&'a Token, Type<'a>)>,
        return_type: Type<'a>,
        body: Vec<Expression<'a>>,
    },
    Extern {
        name: &'a Token,
        parameters: Vec<(&'a Token, Type<'a>)>,
        return_type: Type<'a>,
    },
    Decorator {
        name: &'a Token,
        body: Vec<Expression<'a>>,
    },
    Macro {
        name: &'a Token,
        body: Vec<Expression<'a>>,
    },
}

#[derive(Debug, Clone)]
pub struct Generic<'a> {
    pub name: Token,
    pub constraints: Vec<Type<'a>>,
}

#[derive(Debug, Clone)]
pub enum StorageType {
    Const,
    Dyn,
    Mut,
    Immut,
}

#[derive(Debug, Clone)]
pub enum EntityDeclaration<'a> {
    Entity {
        storage_type: StorageType,
        name: &'a Token,
        component_initializers: Vec<ComponentInitializer<'a>>,
    },
    Component {
        name: Token,
        // TODO: generics
        field_declarations: Vec<FieldDeclaration<'a>>,
    },
    Enum {
        name: Token,
        variants: Vec<VariantDeclaration<'a>>,
    },
}

#[derive(Debug, Clone)]
pub struct ComponentInitializer<'a> {
    pub component: Type<'a>,
    pub fields: Vec<(Option<&'a Token>, Expression<'a>)>,
}

#[derive(Debug, Clone)]
pub struct FieldDeclaration<'a> {
    pub is_noinit: bool,
    pub name: Token,
    pub field_type: Type<'a>,
}

#[derive(Debug, Clone)]
pub enum VariantDeclaration<'a> {
    Basic(Token),
    Tuple(Token, Vec<Type<'a>>),
    Struct(Token, Vec<(Token, Type<'a>)>),
}

#[derive(Debug, Clone)]
pub struct Decorator<'a> {
    pub name: &'a Token,
    pub arguments: Vec<&'a Token>,
}

impl<'a> Decorator<'a> {
    pub fn new(name: &'a Token, arguments: Vec<&'a Token>) -> Self {
        Decorator { name, arguments }
    }
}

#[derive(Debug, Clone)]
pub enum Type<'a> {
    Tuple(Vec<Type<'a>>),
    Array(Box<Type<'a>>, &'a Token), // Token is the size of the array
    Slice(Box<Type<'a>>),
    Pointer(Box<Type<'a>>),
    Option(Box<Type<'a>>),
    Map(Box<Type<'a>>, Box<Type<'a>>),
    Set(Box<Type<'a>>),
    Entity(&'a Token, Vec<Type<'a>>),
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Declaration(Declaration<'a>),
    Drop(Box<Expression<'a>>),
    Add(Box<Expression<'a>>, ComponentInitializer<'a>),
    Remove(Box<Expression<'a>>, ComponentInitializer<'a>),
    For {
        variable: Token,
        iterable: Box<Expression<'a>>,
        body: Vec<Expression<'a>>,
    },
    Break(Option<Box<Expression<'a>>>),
    Continue,
    While {
        condition: Box<Expression<'a>>,
        body: Vec<Expression<'a>>,
        else_body: Option<Vec<Expression<'a>>>,
    },
    Match {
        expression: Box<Expression<'a>>,
        cases: Vec<MatchArm<'a>>,
    },
    If {
        condition: Box<Expression<'a>>,
        body: Vec<Expression<'a>>,
        else_if: Vec<()>,
        else_body: Option<Vec<Expression<'a>>>,
    },
    Assignment {
        lhs: Box<Expression<'a>>,
        rhs: Box<Expression<'a>>,
    },
    Infix {
        lhs: Box<Expression<'a>>,
        operator: &'a Token,
        rhs: Box<Expression<'a>>,
        suffix: Option<&'a Token>,
    },
    Prefix {
        operator: &'a Token,
        rhs: Box<Expression<'a>>,
        suffix: Option<&'a Token>,
    },
    Reference(Box<Expression<'a>>),
    Dereference(Box<Expression<'a>>),
    FunctionCall {
        name: &'a Token,
        arguments: Vec<Expression<'a>>,
    },
    MacroCall {
        name: &'a Token,
        arguments: Vec<&'a Token>,
    },
    Call {
        name: &'a Token,
        arguments: Vec<&'a Token>,
    },
    Literal(&'a Token),
    ArrayLiteral(Vec<Expression<'a>>),
    TupleLiteral(Vec<Expression<'a>>),
    MapLiteral(Vec<(Expression<'a>, Expression<'a>)>),
    SetLiteral(Vec<Expression<'a>>),
    Variable(&'a Token),
    FieldAccess {
        base: Box<Expression<'a>>,
        field: &'a Token,
    },
    Temp(Vec<&'a Token>),
}

#[derive(Debug, Clone)]
pub struct MatchArm<'a> {
    pub pattern: Pattern<'a>,
    pub body: Vec<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub enum Pattern<'a> {
    Wildcard,
    Literal(Token),
    Binding {
        name: Token,
        pattern: Box<Pattern<'a>>,
    },
    Enum {
        name: Token,
        arguments: Vec<Pattern<'a>>,
    },
    Tuple(Vec<Pattern<'a>>),
    Entity(Token, Vec<(Token, Pattern<'a>)>),
    Array(Vec<Pattern<'a>>),
    Map(Vec<(Pattern<'a>, Pattern<'a>)>),
    Set(Vec<Pattern<'a>>),
    Guarded {
        pattern: Box<Pattern<'a>>,
        guard: Box<Expression<'a>>,
    },
}
