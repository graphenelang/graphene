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

use crate::{
    ast::{
        ComponentInitializer, Declaration, Decorator, EntityDeclaration, Expression, Pragma,
        Program, StorageType, Type,
    },
    token::{Token, TokenType},
};

pub struct ParseError {
    pub message: String,
    pub line_number: usize,
    pub line: String,
    pub column: usize,
}
impl ParseError {
    pub fn new(message: String, line: String, line_number: usize, column: usize) -> Self {
        ParseError {
            message,
            line,
            line_number,
            column,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} | {}", self.line_number, self.line)?;
        let mut i = 0;
        while i < self.column + 3 {
            write!(f, " ")?;
            i += 1;
        }

        write!(f, "^ {}", self.message)
    }
}

pub struct Parser<'a> {
    /// Always ends with an EOF token, which `current` never moves past.
    tokens: &'a [Token],
    lines: Vec<&'a str>,
    current: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            lines: source.split('\n').collect(),
            current: 0,
            errors: Vec::new(),
        }
    }

    // ---- token cursor ----

    /// The current token, or `None` at end of input.
    fn peek(&self) -> Option<&'a Token> {
        self.tokens
            .get(self.current)
            .filter(|t| t.token_type != TokenType::EOF)
    }

    /// The token after the current one, or `None` if that is end of input.
    fn peek_next(&self) -> Option<&'a Token> {
        self.peek()?;
        self.tokens
            .get(self.current + 1)
            .filter(|t| t.token_type != TokenType::EOF)
    }

    /// Consumes and returns the current token, or returns `None` at end of input.
    fn next(&mut self) -> Option<&'a Token> {
        let token = self.peek()?;
        self.current += 1;
        Some(token)
    }

    /// The current token, including EOF. Used for error positions.
    fn current_token(&self) -> &'a Token {
        &self.tokens[self.current]
    }

    /// True if the current token has type `token_type`.
    fn check(&self, token_type: TokenType) -> bool {
        self.peek().is_some_and(|t| t.token_type == token_type)
    }

    /// True if the current token is the operator `value`.
    fn check_op(&self, value: &str) -> bool {
        self.peek()
            .is_some_and(|t| t.token_type == TokenType::Operator && t.value == value)
    }

    /// True if the current and next tokens are the single-grapheme symbols
    /// `first` and `second` with nothing between them, e.g. `->` or `@@`.
    /// Symbol tokens are always one grapheme wide, so touching means the second
    /// starts on the same line one column after the first.
    fn is_joined(&self, first: &str, second: &str) -> bool {
        let (Some(a), Some(b)) = (self.peek(), self.peek_next()) else {
            return false;
        };
        let is_symbol = |t: &Token| {
            matches!(
                t.token_type,
                TokenType::Operator
                    | TokenType::At
                    | TokenType::Equal
                    | TokenType::Colon
                    | TokenType::Comma
                    | TokenType::Dot
            )
        };
        is_symbol(a)
            && is_symbol(b)
            && a.value == first
            && b.value == second
            && a.line == b.line
            && b.column == a.column + 1
    }

    // ---- error helpers ----

    /// Records an error at the current token.
    fn error(&mut self, message: impl Into<String>) {
        let token = self.current_token();
        self.errors.push(ParseError::new(
            message.into(),
            self.lines[token.line - 1].to_owned(),
            token.line,
            token.column,
        ));
    }

    /// Like `peek`, but records `message` and fails at end of input.
    fn peek_or_error(&mut self, message: &str) -> Result<&'a Token, ()> {
        match self.peek() {
            Some(token) => Ok(token),
            None => {
                self.error(message);
                Err(())
            }
        }
    }

    /// Consumes the current token if it has type `expected`. Otherwise records
    /// `message` and skips the token.
    fn consume(&mut self, expected: TokenType, message: &str) -> Result<&'a Token, ()> {
        match self.peek() {
            Some(token) if token.token_type == expected => {
                self.next();
                Ok(token)
            }
            _ => {
                self.error(message);
                self.next();
                Err(())
            }
        }
    }

    /// After a list element: consumes a `,`, or records `message` unless the
    /// current token is `close`.
    fn comma_or(&mut self, close: TokenType, message: &str) -> Result<(), ()> {
        if self.check(TokenType::Comma) {
            self.next();
            Ok(())
        } else if self.check(close) {
            Ok(())
        } else {
            self.error(message);
            Err(())
        }
    }

    // ---- grammar ----

    pub fn parse(mut self) -> Result<Program<'a>, Vec<ParseError>> {
        let program = self.program();

        if self.peek().is_some() {
            self.error("expected end of file");
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors)
        }
    }

    fn program(&mut self) -> Program<'a> {
        let mut program = Program::new();
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Operator => {
                    self.error("expected pragma or declaration");
                    self.next();
                }
                TokenType::At if self.is_joined("@", "@") => {
                    self.next();
                    self.next(); // consume "@@"
                    if let Ok(pragma) = self.pragma() {
                        program.pragmas.push(pragma);
                    }
                }
                _ => {
                    if let Ok(declaration) = self.declaration() {
                        program.declarations.push(declaration);
                    }
                }
            }
        }

        program
    }

    fn pragma(&mut self) -> Result<Pragma<'a>, ()> {
        let name = self.consume(TokenType::Name, "expected pragma name")?;
        let arguments = self.raw_arguments()?;
        Ok(Pragma::new(name, arguments))
    }

    fn decorator(&mut self) -> Result<Decorator<'a>, ()> {
        let name = self.consume(TokenType::Name, "expected decorator name")?;
        let arguments = self.raw_arguments()?;
        Ok(Decorator::new(name, arguments))
    }

    /// An optional parenthesised argument list for a pragma or decorator,
    /// returned as raw tokens. Nested parentheses are kept.
    fn raw_arguments(&mut self) -> Result<Vec<&'a Token>, ()> {
        let mut arguments = Vec::new();
        if !self.check(TokenType::Lparen) {
            return Ok(arguments);
        }
        self.next(); // consume the opening parenthesis

        let mut open_parens = 0;
        loop {
            let token = self.peek_or_error("expected closing parenthesis")?;
            match token.token_type {
                TokenType::Lparen => open_parens += 1,
                TokenType::Rparen if open_parens == 0 => break,
                TokenType::Rparen => open_parens -= 1,
                _ => {}
            }
            arguments.push(token);
            self.next();
        }
        self.next(); // consume the closing parenthesis

        Ok(arguments)
    }

    fn declaration(&mut self) -> Result<Declaration<'a>, ()> {
        let current_token = self.current_token();
        let mut decorators = Vec::new();
        while current_token.token_type == TokenType::At {
            self.next();
            decorators.push(self.decorator()?);
        }
        match current_token.token_type {
            TokenType::Const | TokenType::Dyn | TokenType::Mut => {
                let storage = match current_token.token_type {
                    TokenType::Const => StorageType::Const,
                    TokenType::Dyn => StorageType::Dyn,
                    _ => StorageType::Mut,
                };
                self.next();
                if !self.check(TokenType::Entity) {
                    self.error(format!(
                        "expected \"entity\" folowing \"{}\"",
                        current_token.value
                    ));
                    return Err(());
                }
                self.next(); // consume "entity"

                let mut entity = self.entity_or_component()?;
                if let EntityDeclaration::Entity { storage_type, .. } = &mut entity {
                    *storage_type = storage;
                }
                Ok(Declaration::Entity { decorators, entity })
            }
            TokenType::Entity => {
                self.next();
                let entity = self.entity_or_component()?;
                Ok(Declaration::Entity {
                    decorators: Vec::new(),
                    entity,
                })
            }
            TokenType::Fn => {
                self.next();
                self.function(decorators)
            }
            _ => {
                self.error("expected decorator or declaration");
                self.next();
                Err(())
            }
        }
    }

    fn entity_or_component(&mut self) -> Result<EntityDeclaration<'a>, ()> {
        let name = self.consume(TokenType::Name, "expected entity name")?;

        if self.check(TokenType::Lbrace) || self.check_op("<") {
            todo!("component");
        } else if self.check(TokenType::Equal) {
            self.next();
            self.entity(name)
        } else {
            self.error("expected {, <, or =");
            Err(())
        }
    }

    fn entity(&mut self, name: &'a Token) -> Result<EntityDeclaration<'a>, ()> {
        self.peek_or_error("expected entity value")?;
        self.consume(TokenType::Lbrace, "expected {")?;

        let mut component_initializers = Vec::new();
        while !self.check(TokenType::Rbrace) {
            self.peek_or_error("expected closing brace")?;
            component_initializers.push(self.component_initializer()?);
        }
        self.next(); // consume the closing brace

        Ok(EntityDeclaration::Entity {
            storage_type: StorageType::Immut,
            name,
            component_initializers,
        })
    }

    fn component_initializer(&mut self) -> Result<ComponentInitializer<'a>, ()> {
        self.peek_or_error("expected component initializer")?;
        let component = self.type_declaration()?;

        let mut fields = Vec::new();
        if self.check(TokenType::Lparen) {
            self.next(); // consume the opening parenthesis
            while !self.check(TokenType::Rparen) {
                let token = self.peek_or_error("expected closing parenthesis")?;

                let mut field_name = None;
                if token.token_type == TokenType::Name {
                    field_name = Some(token);
                    self.next(); // consume the field name
                    self.consume(TokenType::Colon, "expected :")?;
                }

                let value = self.expression()?;
                fields.push((field_name, value));

                self.comma_or(TokenType::Rparen, "expected , or )")?;
            }
            self.next(); // consume the closing parenthesis
        }

        Ok(ComponentInitializer { component, fields })
    }

    fn type_declaration(&mut self) -> Result<Type<'a>, ()> {
        let current_token = self.peek_or_error("expected type declaration")?;
        match current_token.token_type {
            TokenType::Lparen => {
                self.next();
                let mut types = Vec::new();
                while !self.check(TokenType::Rparen) {
                    self.peek_or_error("expected closing parenthesis")?;
                    types.push(self.type_declaration()?);
                    self.comma_or(TokenType::Rparen, "expected , or )")?;
                }
                self.next(); // consume the closing parenthesis
                Ok(Type::Tuple(types))
            }
            TokenType::Operator if current_token.value == "[" => {
                self.next();
                let type_decl = self.type_declaration()?;
                if !self.check_op(";") {
                    if self.check_op("]") {
                        self.next();
                        return Ok(Type::Slice(Box::new(type_decl)));
                    }
                    self.error("expected ; or ]");
                    return Err(());
                }
                self.next(); // consume the semicolon

                let array_length = self.peek_or_error("expected array length")?;
                if !matches!(
                    array_length.token_type,
                    TokenType::Integer | TokenType::Name
                ) {
                    self.error("expected array length");
                    return Err(());
                }
                self.next();

                if !self.check_op("]") {
                    self.error("expected ]");
                    return Err(());
                }
                self.next(); // consume the closing bracket
                Ok(Type::Array(Box::new(type_decl), array_length))
            }
            TokenType::Name => {
                self.next();
                let mut generics = Vec::new();
                if self.check_op("<") {
                    self.next(); // consume the <
                    while self.current_token().token_type != TokenType::Operator
                        && self.current_token().value != ">"
                    {
                        self.peek_or_error("expected closing >")?;
                        generics.push(self.type_declaration()?);
                        if self.check(TokenType::Comma) {
                            self.next(); // consume the ,
                        } else if self.current_token().token_type != TokenType::Operator
                            && self.current_token().value != ">"
                        {
                            self.error("expected , or >");
                            return Err(());
                        }
                    }
                    self.next(); // consume the >
                }

                Ok(Type::Entity(current_token, generics))
            }
            TokenType::Operator if current_token.value == "*" => {
                self.next(); // consume the *
                Ok(Type::Pointer(Box::new(self.type_declaration()?)))
            }
            TokenType::Operator if current_token.value == "?" => {
                self.next(); // consume the ?
                Ok(Type::Option(Box::new(self.type_declaration()?)))
            }
            TokenType::Lbrace => {
                self.next(); // consume the {
                let key = self.type_declaration()?;
                let token = self.peek_or_error("expected : or }")?;
                match token.token_type {
                    TokenType::Rbrace => {
                        self.next();
                        return Ok(Type::Set(Box::new(key)));
                    }
                    TokenType::Colon => {
                        self.next(); // consume the :
                    }
                    _ => {
                        self.error("expected : or }");
                        return Err(());
                    }
                }

                self.peek_or_error("expected value type")?;
                let value = self.type_declaration()?;
                if !self.check(TokenType::Rbrace) {
                    self.error("expected closing }");
                    return Err(());
                }
                self.next(); // consume the closing }
                Ok(Type::Map(Box::new(key), Box::new(value)))
            }
            _ => {
                self.error("expected type declaration");
                Err(())
            }
        }
    }

    fn function(&mut self, decorators: Vec<Decorator<'a>>) -> Result<Declaration<'a>, ()> {
        let name = self.peek_or_error("expected function name")?;
        if !matches!(name.token_type, TokenType::Name | TokenType::Operator) {
            self.error("expected function name");
            self.next();
            return Err(());
        }
        self.next();

        self.consume(TokenType::Lparen, "expected (")?;

        let mut parameters = Vec::new();
        while !self.check(TokenType::Rparen) {
            self.peek_or_error("expected closing parenthesis")?;
            let param_name = self.consume(TokenType::Name, "expected parameter name")?;
            self.consume(TokenType::Colon, "expected :")?;
            let param_type = self.type_declaration()?;
            parameters.push((param_name, param_type));
            self.next();
        }
        self.next(); // consume the closing parenthesis

        let return_type = if self.is_joined("-", ">") {
            self.next();
            self.next(); // consume the ->
            self.type_declaration()?
        } else {
            Type::Tuple(Vec::new())
        };

        self.consume(TokenType::Lbrace, "expected {")?;

        let mut body = Vec::new();
        while !self.check(TokenType::Rbrace) {
            self.peek_or_error("expected closing }")?;
            body.push(self.expression()?);
        }
        self.next(); // consume the closing }

        Ok(Declaration::Function {
            name,
            decorators,
            generics: Vec::new(),
            parameters,
            return_type,
            body,
        })
    }

    fn expression(&mut self) -> Result<Expression<'a>, ()> {
        let token = self.peek_or_error("expected expression")?;
        match token.token_type {
            TokenType::At
            | TokenType::Entity
            | TokenType::Fn
            | TokenType::Const
            | TokenType::Mut
            | TokenType::Extern
            | TokenType::Enum
            | TokenType::System
            | TokenType::Macro => Ok(Expression::Declaration(self.declaration()?)),
            _ => self.assignment(),
        }
    }

    fn assignment(&mut self) -> Result<Expression<'a>, ()> {
        let left = self.operators()?;

        if !self.check(TokenType::Equal) {
            return Ok(left);
        }
        self.next(); // consume the =

        if matches!(
            left,
            Expression::Variable(..) | Expression::Dereference(..) | Expression::FieldAccess { .. }
        ) {
            let right = self.assignment()?;
            Ok(Expression::Assignment {
                lhs: Box::new(left),
                rhs: Box::new(right),
            })
        } else {
            self.error("invalid assignment target");
            Err(())
        }
    }

    /// True if the token after the current one can start an atom.
    fn next_is_atom_start(&self) -> bool {
        self.peek_next().is_some_and(|t| {
            matches!(
                t.token_type,
                TokenType::Integer
                    | TokenType::Float
                    | TokenType::String
                    | TokenType::Char
                    | TokenType::True
                    | TokenType::False
                    | TokenType::Null
                    | TokenType::Name
                    | TokenType::Lparen
            )
        })
    }

    fn unary(&mut self) -> Result<Expression<'a>, ()> {
        let Some(op) = self.peek().filter(|t| t.token_type == TokenType::Operator) else {
            return self.call();
        };
        self.next();
        let rhs = Box::new(self.unary()?);

        let suffix = if self.check(TokenType::Operator)
            && self
                .peek_next()
                .is_some_and(|t| t.token_type == TokenType::Operator)
        {
            self.next()
        } else {
            None
        };

        Ok(if suffix.is_none() && op.value == "&" {
            Expression::Reference(rhs)
        } else if suffix.is_none() && op.value == "*" {
            Expression::Dereference(rhs)
        } else {
            Expression::Prefix {
                operator: op,
                rhs,
                suffix,
            }
        })
    }

    fn operators(&mut self) -> Result<Expression<'a>, ()> {
        let mut left = self.unary()?;

        while let Some(op_token) = self.peek().filter(|t| t.token_type == TokenType::Operator) {
            let op_pos = self.current;
            self.next();
            let rhs = self.unary()?;

            let suffix = if self.check(TokenType::Operator) && !self.next_is_atom_start() {
                self.next()
            } else {
                None
            };

            if self.check(TokenType::Equal) {
                match &mut left {
                    Expression::Infix { suffix: s, .. } | Expression::Prefix { suffix: s, .. }
                        if s.is_none() =>
                    {
                        *s = Some(op_token);
                    }
                    _ => {}
                }
                self.current = op_pos + 1;
                break;
            }

            left = Expression::Infix {
                lhs: Box::new(left),
                operator: op_token,
                rhs: Box::new(rhs),
                suffix,
            };
        }

        Ok(left)
    }

    fn call(&mut self) -> Result<Expression<'a>, ()> {
        let mut expr = self.primary()?;

        loop {
            if self.check(TokenType::Lparen) {
                self.next(); // consume the opening parenthesis
                let mut arguments = Vec::new();
                while !self.check(TokenType::Rparen) {
                    self.peek_or_error("expected closing parenthesis")?;
                    arguments.push(self.expression()?);
                    self.comma_or(TokenType::Rparen, "expected , or )")?;
                }
                self.next(); // consume the closing parenthesis

                let Expression::Variable(name) = expr else {
                    self.error("expected function name");
                    return Err(());
                };
                expr = Expression::FunctionCall { name, arguments };
            } else if self.check(TokenType::Dot) {
                self.next(); // consume the dot
                let field = self.consume(TokenType::Name, "expected field name")?;
                expr = Expression::FieldAccess {
                    base: Box::new(expr),
                    field,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expression<'a>, ()> {
        let token = self.peek_or_error("expected expression")?;
        match token.token_type {
            TokenType::Integer
            | TokenType::Float
            | TokenType::String
            | TokenType::Char
            | TokenType::True
            | TokenType::False
            | TokenType::Null => {
                self.next();
                Ok(Expression::Literal(token))
            }
            TokenType::Name => {
                self.next();
                Ok(Expression::Variable(token))
            }
            TokenType::Lparen => {
                self.next(); // consume the opening parenthesis
                let expr = self.expression()?;
                self.consume(TokenType::Rparen, "expected closing parenthesis")?;
                Ok(expr)
            }
            _ => {
                self.error("expected expression");
                Err(())
            }
        }
    }
}
