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

use unicode_segmentation::{Graphemes, UnicodeSegmentation};

use crate::{
    ast::{
        ComponentInitializer, Declaration, Decorator, EntityDeclaration, Expression, Pragma,
        Program, Type,
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
    tokens: &'a Vec<Token>,
    lines: Vec<Graphemes<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: &'a Vec<Token>) -> Self {
        let lines = source
            .split('\n')
            .map(|line| line.graphemes(true))
            .collect();
        Parser { tokens, lines }
    }

    fn is_at_end(&self, current: &usize) -> bool {
        self.tokens[*current].token_type == TokenType::EOF
    }

    pub fn parse(&self) -> Result<Program, Vec<ParseError>> {
        let mut errors = Vec::new();
        let mut current = 0;
        let program = self.program(&mut current, &mut errors);

        if self.tokens[current].token_type != crate::token::TokenType::EOF {
            errors.push(ParseError::new(
                "expeced end of file".to_string(),
                self.lines[self.tokens[current].line - 1].clone().collect(),
                self.tokens[current].line,
                self.tokens[current].column,
            ));
        }

        if errors.len() > 0 {
            return Err(errors);
        }

        Ok(program)
    }

    fn program(&self, current: &mut usize, errors: &mut Vec<ParseError>) -> Program {
        let mut program = Program::new();
        while !self.is_at_end(current) {
            match self.tokens[*current] {
                Token {
                    token_type: TokenType::Operator,
                    value: ref literal,
                    ..
                } => {
                    if literal != "@@" {
                        errors.push(ParseError::new(
                            "expected pragma or declaration".to_string(),
                            self.lines[self.tokens[*current].line - 1].clone().collect(),
                            self.tokens[*current].line,
                            self.tokens[*current].column,
                        ));
                        *current += 1;
                        continue;
                    }
                    *current += 1;
                    let pragma = self.pragma(current, errors);
                    if let Ok(pragma) = pragma {
                        program.pragmas.push(pragma);
                    } else if let Err(err) = pragma {
                        continue;
                    }
                }
                _ => {
                    let declaration = self.declaration(current, errors);
                    if let Ok(declaration) = declaration {
                        program.declarations.push(declaration);
                    } else if let Err(err) = declaration {
                        continue;
                    }
                }
            }
        }

        program
    }

    fn pragma(&self, current: &mut usize, errors: &mut Vec<ParseError>) -> Result<Pragma, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected pragma name".to_string(),
                self.tokens[*current].value.clone(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        if self.tokens[*current].token_type != TokenType::Name {
            errors.push(ParseError::new(
                "expected pragma name".to_string(),
                self.tokens[*current].value.clone(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            *current += 1;
            return Err(());
        }
        let name = &self.tokens[*current];
        *current += 1;

        let mut arguments = Vec::new();
        let mut open_parens = 0;
        if self.tokens[*current].token_type == TokenType::Lparen {
            *current += 1;
            loop {
                if self.is_at_end(current) {
                    errors.push(ParseError::new(
                        "expected closing parenthesis".to_string(),
                        self.lines[self.tokens[*current].line - 2].clone().collect(),
                        self.tokens[*current].line - 1,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }
                if self.tokens[*current].token_type == TokenType::Lparen {
                    open_parens += 1;
                } else if self.tokens[*current].token_type == TokenType::Rparen {
                    if open_parens == 0 {
                        break;
                    }
                    open_parens -= 1;
                }
                arguments.push(&self.tokens[*current]);
                *current += 1;
            }
            if !self.is_at_end(current) {
                *current += 1; // consume the closing parenthesis
            }
        }
        Ok(Pragma::new(name, arguments))
    }

    fn declaration(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Declaration, ()> {
        let current_token = &self.tokens[*current];
        let mut decorators: Vec<Decorator> = Vec::new();
        while current_token.token_type == TokenType::At {
            *current += 1;
            let decorator = self.decorator(current, errors);
            if let Ok(decorator) = decorator {
                decorators.push(decorator);
            } else {
                return Err(());
            }
        }
        match current_token.token_type {
            // TODO: add const and dyn support
            TokenType::Entity => {
                *current += 1;
                let entity = self.entity_or_component(current, errors);
                if let Ok(entity) = entity {
                    return Ok(Declaration::Entity(entity));
                } else {
                    return Err(());
                }
            }
            _ => {
                errors.push(ParseError::new(
                    "expected decorator or declaration".to_string(),
                    self.lines[self.tokens[*current].line - 1].clone().collect(),
                    self.tokens[*current].line,
                    self.tokens[*current].column,
                ));
                *current += 1;
                return Err(());
            }
        }
    }

    fn decorator(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Decorator, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected decorator".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }
        let name = &self.tokens[*current];
        *current += 1;
        if name.token_type != TokenType::Name {
            errors.push(ParseError::new(
                "expected decorator name".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        let mut arguments = Vec::new();
        let mut open_parens = 0;
        if self.tokens[*current].token_type == TokenType::Lparen {
            *current += 1;
            loop {
                if self.is_at_end(current) {
                    errors.push(ParseError::new(
                        "expected closing parenthesis".to_string(),
                        self.lines[self.tokens[*current].line - 2].clone().collect(),
                        self.tokens[*current].line - 1,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }
                if self.tokens[*current].token_type == TokenType::Lparen {
                    open_parens += 1;
                } else if self.tokens[*current].token_type == TokenType::Rparen {
                    if open_parens == 0 {
                        break;
                    }
                    open_parens -= 1;
                }
                arguments.push(&self.tokens[*current]);
                *current += 1;
            }
            if !self.is_at_end(current) {
                *current += 1; // consume the closing parenthesis
            }
        }

        Ok(Decorator::new(name, arguments))
    }

    fn entity_or_component(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<EntityDeclaration, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected entity name".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }
        let name_index = *current;
        *current += 1;

        if self.tokens[name_index].token_type != TokenType::Name {
            errors.push(ParseError::new(
                "expected entity name".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        let current_token = &self.tokens[*current];
        if current_token.token_type == TokenType::Lbrace
            || (current_token.token_type == TokenType::Operator && current_token.value == "<")
        {
            todo!("component");
        } else if current_token.token_type == TokenType::Equal {
            *current += 1;
            let entity = self.entity(name_index, current, errors);
            if let Ok(entity) = entity {
                return Ok(entity);
            } else {
                return Err(());
            }
        } else {
            errors.push(ParseError::new(
                "expected {, <, or =".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }
    }

    fn entity(
        &self,
        name_index: usize,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<EntityDeclaration, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected entity value".to_string(),
                self.lines[self.tokens[*current].line - 2].clone().collect(),
                self.tokens[*current].line - 1,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        let mut result = EntityDeclaration::Entity {
            is_const: false,
            name: &self.tokens[name_index],
            component_initializers: Vec::new(),
        };

        let current_token = &self.tokens[*current];
        if current_token.token_type == TokenType::Lbrace {
            *current += 1;
        } else {
            *current += 1;
            errors.push(ParseError::new(
                "expected {".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        while self.tokens[*current].token_type.clone() != TokenType::Rbrace {
            if self.is_at_end(current) {
                errors.push(ParseError::new(
                    "expected closing brace".to_string(),
                    self.lines[self.tokens[*current].line - 2].clone().collect(),
                    self.tokens[*current].line - 1,
                    self.tokens[*current].column,
                ));
                return Err(());
            }
            let initializer = self.component_initializer(current, errors);
            if let Err(_) = initializer {
                return Err(());
            } else if let Ok(initializer) = initializer {
                if let EntityDeclaration::Entity {
                    component_initializers,
                    ..
                } = &mut result
                {
                    component_initializers.push(initializer.clone());
                }
            }
        }
        *current += 1; // consume the closing brace
        return Ok(result);
    }

    fn component_initializer(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<ComponentInitializer, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected component initializer".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        let component = self.type_declaration(current, errors)?;
        return Ok(ComponentInitializer {
            component,
            fields: Vec::new(),
        });
    }

    fn type_declaration(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Type, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected type declaration".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        let current_token = &self.tokens[*current];
        match current_token.token_type {
            TokenType::Lparen => {
                *current += 1;
                let mut types = Vec::new();
                while self.tokens[*current].token_type != TokenType::Rparen {
                    if self.is_at_end(current) {
                        errors.push(ParseError::new(
                            "expected closing parenthesis".to_string(),
                            self.lines[self.tokens[*current].line - 1].clone().collect(),
                            self.tokens[*current].line,
                            self.tokens[*current].column,
                        ));
                        return Err(());
                    }
                    let type_decl = self.type_declaration(current, errors)?;
                    types.push(type_decl);

                    if self.tokens[*current].token_type == TokenType::Comma {
                        *current += 1; // consume the comma
                    } else if self.tokens[*current].token_type != TokenType::Rparen {
                        errors.push(ParseError::new(
                            "expected , or )".to_string(),
                            self.lines[self.tokens[*current].line - 1].clone().collect(),
                            self.tokens[*current].line,
                            self.tokens[*current].column,
                        ));
                        return Err(());
                    }
                }
                *current += 1; // consume the closing parenthesis
                return Ok(Type::Tuple(types));
            }
            TokenType::Operator if current_token.value == "[" => {
                *current += 1;
                let type_decl = self.type_declaration(current, errors)?;
                if self.tokens[*current].value != ";" {
                    if self.tokens[*current].token_type == TokenType::Operator
                        && self.tokens[*current].value == "]"
                    {
                        *current += 1;
                        return Ok(Type::Slice(Box::new(type_decl)));
                    }
                    errors.push(ParseError::new(
                        "expected ; or ]".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }

                *current += 1; // consume the semicolon
                if self.is_at_end(current) {
                    errors.push(ParseError::new(
                        "expected array length".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }

                let array_length = &self.tokens[*current];
                if self.tokens[*current].token_type != TokenType::Integer
                    && self.tokens[*current].token_type != TokenType::Name
                {
                    errors.push(ParseError::new(
                        "expected array length".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }

                *current += 1;

                if self.tokens[*current].token_type != TokenType::Operator
                    || self.tokens[*current].value != "]"
                {
                    errors.push(ParseError::new(
                        "expected ]".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }
                *current += 1; // consume the closing bracket
                return Ok(Type::Array(Box::new(type_decl), array_length));
            }
            TokenType::Name => {
                let name = &self.tokens[*current];
                *current += 1;
                let mut generics = Vec::new();
                if !self.is_at_end(current)
                    && self.tokens[*current].token_type == TokenType::Operator
                    && self.tokens[*current].value == "<"
                {
                    *current += 1; // consume the <
                    while self.tokens[*current].token_type != TokenType::Operator
                        && self.tokens[*current].value != ">"
                    {
                        if self.is_at_end(current) {
                            errors.push(ParseError::new(
                                "expected closing >".to_string(),
                                self.lines[self.tokens[*current].line - 1].clone().collect(),
                                self.tokens[*current].line,
                                self.tokens[*current].column,
                            ));
                            return Err(());
                        }
                        let generic = self.type_declaration(current, errors)?;
                        generics.push(generic);
                        if !self.is_at_end(current)
                            && self.tokens[*current].token_type == TokenType::Comma
                        {
                            *current += 1; // consume the ,
                        } else if self.tokens[*current].token_type != TokenType::Operator
                            && self.tokens[*current].value != ">"
                        {
                            errors.push(ParseError::new(
                                "expected , or >".to_string(),
                                self.lines[self.tokens[*current].line - 1].clone().collect(),
                                self.tokens[*current].line,
                                self.tokens[*current].column,
                            ));
                            return Err(());
                        }
                    }
                    *current += 1; // consume the >
                }

                return Ok(Type::Entity(name, generics));
            }
            TokenType::Operator if current_token.value == "*" => {
                *current += 1; // consume the *
                return Ok(Type::Pointer(Box::new(
                    self.type_declaration(current, errors)?,
                )));
            }
            TokenType::Operator if current_token.value == "?" => {
                *current += 1; // consume the ?
                return Ok(Type::Option(Box::new(
                    self.type_declaration(current, errors)?,
                )));
            }
            TokenType::Lbrace => {
                *current += 1; // consume the {
                let key = self.type_declaration(current, errors)?;
                if self.is_at_end(current) {
                    errors.push(ParseError::new(
                        "expected : or }".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }
                if self.tokens[*current].token_type == TokenType::Rbrace {
                    *current += 1;
                    return Ok(Type::Set(Box::new(key)));
                } else if self.tokens[*current].token_type == TokenType::Colon
                    && self.tokens[*current].value == ":"
                {
                    *current += 1; // consume the :
                } else {
                    errors.push(ParseError::new(
                        "expected : or }".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }
                if self.is_at_end(current) {
                    errors.push(ParseError::new(
                        "expected value type".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }
                let value = self.type_declaration(current, errors)?;
                if self.is_at_end(current) || self.tokens[*current].token_type != TokenType::Rbrace
                {
                    errors.push(ParseError::new(
                        "expected closing }".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }
                *current += 1; // consume the closing }
                return Ok(Type::Map(Box::new(key), Box::new(value)));
            }
            _ => todo!(),
        }
    }

    fn expression(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Expression, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected expression".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }
        let expression = &self.tokens[*current];
        *current += 1;
        Ok(Expression::Temp(vec![expression]))
    }
}
