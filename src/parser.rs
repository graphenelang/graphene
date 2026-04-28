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

    fn consume(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
        expected: TokenType,
        message: &str,
    ) -> Result<&Token, ()> {
        if self.is_at_end(current) && expected != TokenType::EOF {
            errors.push(ParseError::new(
                message.to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }
        if self.tokens[*current].token_type != expected {
            errors.push(ParseError::new(
                message.to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            *current += 1; // consume the token
            return Err(());
        } else {
            *current += 1; // consume the expected token
            return Ok(&self.tokens[*current - 1]);
        }
    }

    pub fn parse(&self) -> Result<Program<'_>, Vec<ParseError>> {
        let mut errors = Vec::new();
        let mut current = 0;
        let program = self.program(&mut current, &mut errors);

        let _ = self.consume(
            &mut current,
            &mut errors,
            TokenType::EOF,
            "expected end of file",
        );

        if errors.len() > 0 {
            return Err(errors);
        }

        Ok(program)
    }

    fn program(&self, current: &mut usize, errors: &mut Vec<ParseError>) -> Program<'_> {
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
                    } else if let Err(_) = pragma {
                        continue;
                    }
                }
                _ => {
                    let declaration = self.declaration(current, errors);
                    if let Ok(declaration) = declaration {
                        program.declarations.push(declaration);
                    } else if let Err(_) = declaration {
                        continue;
                    }
                }
            }
        }

        program
    }

    fn pragma(&self, current: &mut usize, errors: &mut Vec<ParseError>) -> Result<Pragma<'_>, ()> {
        let name = self.consume(current, errors, TokenType::Name, "expected pragma name")?;

        let mut arguments = Vec::new();
        let mut open_parens = 0;
        if self.tokens[*current].token_type == TokenType::Lparen {
            *current += 1;
            loop {
                if self.is_at_end(current) {
                    errors.push(ParseError::new(
                        "expected closing parenthesis".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
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
    ) -> Result<Declaration<'_>, ()> {
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
            TokenType::Const | TokenType::Dyn | TokenType::Mut => {
                *current += 1;
                if self.is_at_end(current) || self.tokens[*current].token_type != TokenType::Entity
                {
                    errors.push(ParseError::new(
                        format!("expected \"entity\" folowing \"{}\"", current_token.value),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
                        self.tokens[*current].line,
                        self.tokens[*current].column,
                    ));
                    return Err(());
                }

                *current += 1; // consume "entity"

                let entity = self.entity_or_component(current, errors);
                if let Ok(mut entity) = entity {
                    if current_token.token_type == TokenType::Const {
                        if let EntityDeclaration::Entity { storage_type, .. } = &mut entity {
                            *storage_type = StorageType::Const;
                        }
                    } else if current_token.token_type == TokenType::Dyn {
                        if let EntityDeclaration::Entity { storage_type, .. } = &mut entity {
                            *storage_type = StorageType::Dyn;
                        }
                    } else if current_token.token_type == TokenType::Mut {
                        if let EntityDeclaration::Entity { storage_type, .. } = &mut entity {
                            *storage_type = StorageType::Mut;
                        }
                    }
                    return Ok(Declaration::Entity { decorators, entity });
                } else {
                    return Err(());
                }
            }
            TokenType::Entity => {
                *current += 1;
                let entity = self.entity_or_component(current, errors);
                if let Ok(entity) = entity {
                    return Ok(Declaration::Entity {
                        decorators: Vec::new(),
                        entity,
                    });
                } else {
                    return Err(());
                }
            }
            TokenType::Fn => {
                *current += 1;
                self.function(current, errors, decorators)
                    .or_else(|err| Err(err))
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
    ) -> Result<Decorator<'_>, ()> {
        let name = self.consume(current, errors, TokenType::Name, "expected decorator name")?;

        let mut arguments = Vec::new();
        let mut open_parens = 0;
        if self.tokens[*current].token_type == TokenType::Lparen {
            *current += 1;
            loop {
                if self.is_at_end(current) {
                    errors.push(ParseError::new(
                        "expected closing parenthesis".to_string(),
                        self.lines[self.tokens[*current].line - 1].clone().collect(),
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
    ) -> Result<EntityDeclaration<'_>, ()> {
        let name_index = *current;
        let _name = self.consume(current, errors, TokenType::Name, "expected entity name")?;

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
    ) -> Result<EntityDeclaration<'_>, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected entity value".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line - 1,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        let mut result = EntityDeclaration::Entity {
            storage_type: StorageType::Immut,
            name: &self.tokens[name_index],
            component_initializers: Vec::new(),
        };

        self.consume(current, errors, TokenType::Lbrace, "expected {")?;

        while self.tokens[*current].token_type.clone() != TokenType::Rbrace {
            if self.is_at_end(current) {
                errors.push(ParseError::new(
                    "expected closing brace".to_string(),
                    self.lines[self.tokens[*current].line - 1].clone().collect(),
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
                    component_initializers.push(initializer);
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
    ) -> Result<ComponentInitializer<'_>, ()> {
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

        let mut fields = Vec::new();

        if self.tokens[*current].token_type == TokenType::Lparen {
            *current += 1; // consume the opening parenthesis
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

                let mut field_name = None;

                if self.tokens[*current].token_type == TokenType::Name {
                    field_name = Some(&self.tokens[*current]);
                    *current += 1; // consume the field name

                    self.consume(current, errors, TokenType::Colon, "expected :")?;
                }

                let value = self.expression(current, errors)?;

                fields.push((field_name, value));

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
        }

        return Ok(ComponentInitializer { component, fields });
    }

    fn type_declaration(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Type<'_>, ()> {
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
            _ => {
                errors.push(ParseError::new(
                    "expected type declaration".to_string(),
                    self.lines[self.tokens[*current].line - 1].clone().collect(),
                    self.tokens[*current].line,
                    self.tokens[*current].column,
                ));
                return Err(());
            }
        }
    }

    fn function(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
        decorators: Vec<Decorator<'a>>,
    ) -> Result<Declaration<'_>, ()> {
        let name = if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected function name".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        } else if matches!(
            self.tokens[*current].token_type,
            TokenType::Name | TokenType::Operator
        ) {
            let n = &self.tokens[*current];
            *current += 1;
            n
        } else {
            errors.push(ParseError::new(
                "expected function name".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            *current += 1;
            return Err(());
        };

        self.consume(current, errors, TokenType::Lparen, "expected (")?;

        let mut parameters = Vec::new();

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

            let param_name =
                self.consume(current, errors, TokenType::Name, "expected parameter name")?;
            self.consume(current, errors, TokenType::Colon, "expected :")?;
            let param_type = self.type_declaration(current, errors)?;
            parameters.push((param_name, param_type));
            *current += 1;
        }
        *current += 1; // consume the closing parenthesis

        let return_type = if self.tokens[*current].token_type == TokenType::Operator
            && self.tokens[*current].value == "->"
        {
            *current += 1; // consume the ->
            self.type_declaration(current, errors)?
        } else {
            Type::Tuple(Vec::new())
        };

        self.consume(current, errors, TokenType::Lbrace, "expected {")?;

        let mut body = Vec::new();

        while self.tokens[*current].token_type != TokenType::Rbrace {
            if self.is_at_end(current) {
                errors.push(ParseError::new(
                    "expected closing }".to_string(),
                    self.lines[self.tokens[*current].line - 1].clone().collect(),
                    self.tokens[*current].line,
                    self.tokens[*current].column,
                ));
                return Err(());
            }
            let expr = self.expression(current, errors)?;
            body.push(expr);
        }
        *current += 1; // consume the closing }

        Ok(Declaration::Function {
            name,
            decorators,
            generics: Vec::new(),
            parameters,
            return_type,
            body,
        })
    }

    fn expression(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Expression<'_>, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected expression".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        match self.tokens[*current].token_type {
            TokenType::At
            | TokenType::Entity
            | TokenType::Fn
            | TokenType::Const
            | TokenType::Mut
            | TokenType::Extern
            | TokenType::Enum
            | TokenType::System
            | TokenType::Macro => {
                return Ok(Expression::Declaration(self.declaration(current, errors)?));
            }
            _ => self.assignment(current, errors),
        }
    }

    fn assignment(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Expression<'_>, ()> {
        let left = self.operators(current, errors)?;

        if self.is_at_end(current) || self.tokens[*current].token_type != TokenType::Equal {
            return Ok(left);
        }

        *current += 1; // consume the =

        if let Expression::Variable(..)
        | Expression::Dereference(..)
        | Expression::FieldAccess { .. } = left
        {
            let right = self.assignment(current, errors)?;

            return Ok(Expression::Assignment {
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
        } else {
            errors.push(ParseError::new(
                "invalid assignment target".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }
    }

    fn is_atom_start(&self, idx: usize) -> bool {
        idx < self.tokens.len()
            && matches!(
                self.tokens[idx].token_type,
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
    }

    fn next_is_operator(&self, idx: usize) -> bool {
        idx < self.tokens.len() && self.tokens[idx].token_type == TokenType::Operator
    }

    fn unary(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Expression<'_>, ()> {
        if !self.is_at_end(current) && self.tokens[*current].token_type == TokenType::Operator {
            let op = &self.tokens[*current];
            *current += 1;
            let rhs = Box::new(self.unary(current, errors)?);

            let suffix = if !self.is_at_end(current)
                && self.tokens[*current].token_type == TokenType::Operator
                && self.next_is_operator(*current + 1)
            {
                let s = &self.tokens[*current];
                *current += 1;
                Some(s)
            } else {
                None
            };

            return Ok(if suffix.is_none() && op.value == "&" {
                Expression::Reference(rhs)
            } else if suffix.is_none() && op.value == "*" {
                Expression::Dereference(rhs)
            } else {
                Expression::Prefix {
                    operator: op,
                    rhs,
                    suffix,
                }
            });
        }
        self.call(current, errors)
    }

    fn operators(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Expression<'_>, ()> {
        let mut left = self.unary(current, errors)?;

        loop {
            if self.is_at_end(current) || self.tokens[*current].token_type != TokenType::Operator {
                break;
            }

            let op_pos = *current;
            let op_token = &self.tokens[op_pos];

            *current += 1;
            let rhs = self.unary(current, errors)?;

            let suffix = if !self.is_at_end(current)
                && self.tokens[*current].token_type == TokenType::Operator
                && !self.is_atom_start(*current + 1)
            {
                let s = &self.tokens[*current];
                *current += 1;
                Some(s)
            } else {
                None
            };

            if !self.is_at_end(current) && self.tokens[*current].token_type == TokenType::Equal {
                match &mut left {
                    Expression::Infix { suffix: s, .. } | Expression::Prefix { suffix: s, .. }
                        if s.is_none() =>
                    {
                        *s = Some(op_token);
                    }
                    _ => {}
                }
                *current = op_pos + 1;
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

    fn call(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Expression<'_>, ()> {
        let mut expr = self.primary(current, errors)?;

        loop {
            if self.tokens[*current].token_type == TokenType::Lparen {
                *current += 1; // consume the opening parenthesis
                let mut arguments = Vec::new();
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
                    let argument = self.expression(current, errors)?;
                    arguments.push(argument);
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
                expr = Expression::FunctionCall {
                    name: match expr {
                        Expression::Variable(name) => name,
                        _ => {
                            errors.push(ParseError::new(
                                "expected function name".to_string(),
                                self.lines[self.tokens[*current].line - 1].clone().collect(),
                                self.tokens[*current].line,
                                self.tokens[*current].column,
                            ));
                            return Err(());
                        }
                    },
                    arguments,
                };
            } else if self.tokens[*current].token_type == TokenType::Dot {
                *current += 1; // consume the dot
                let field =
                    self.consume(current, errors, TokenType::Name, "expected field name")?;
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

    fn primary(
        &self,
        current: &mut usize,
        errors: &mut Vec<ParseError>,
    ) -> Result<Expression<'_>, ()> {
        if self.is_at_end(current) {
            errors.push(ParseError::new(
                "expected expression".to_string(),
                self.lines[self.tokens[*current].line - 1].clone().collect(),
                self.tokens[*current].line,
                self.tokens[*current].column,
            ));
            return Err(());
        }

        let token = &self.tokens[*current];
        match token.token_type {
            TokenType::Integer
            | TokenType::Float
            | TokenType::String
            | TokenType::Char
            | TokenType::True
            | TokenType::False
            | TokenType::Null => {
                *current += 1;
                Ok(Expression::Literal(token))
            }
            TokenType::Name => {
                *current += 1;
                Ok(Expression::Variable(token))
            }
            TokenType::Lparen => {
                *current += 1; // consume the opening parenthesis
                let expr = self.expression(current, errors)?;
                self.consume(
                    current,
                    errors,
                    TokenType::Rparen,
                    "expected closing parenthesis",
                )?;
                Ok(expr)
            }
            _ => {
                errors.push(ParseError::new(
                    "expected expression".to_string(),
                    self.lines[self.tokens[*current].line - 1].clone().collect(),
                    self.tokens[*current].line,
                    self.tokens[*current].column,
                ));
                return Err(());
            }
        }
    }
}
